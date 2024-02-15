"""This tour model is a work in progress and is to be addressed in WP2"""
import os
import pandas as pd
import numpy as np
import mdlfunction as fun
import mdlconfig
import multiprocessing as mp
from typing import Union, Dict, List
import itertools


class TourModel:
    """
    prototype for Tour model
    """

    comments = ["*", ";", "#"]

    def __init__(
        self, nts_fldr: str, tmz_leve: str = "region", over_write: bool = True
    ):
        # tmz_leve = region, county, or ua
        fun.log_stderr("\n***** NTS TOUR MODEL *****")
        self.cfg = mdlconfig.Config(nts_fldr)
        self.num_cpus = max(os.cpu_count() - 2, 1)
        self.out_fldr = self.cfg.dir_output / self.cfg.fld_tour
        self.fld_report, self.tmz_leve = self.cfg.fld_report, tmz_leve

        # read specs
        if over_write:
            self._specs()
            self._read_input()
            self._calc_prod()
            self._calc_rezone(True)
            self._calc_tour(10, True)
            self._calc_mts("freq", True)
            self._calc_dest("freq", True)
            self._calc_trip_by_activity()
            self._reports()
            # self._adjust_tripend()
        else:
            fun.log_stderr(f" .. skipped!")

    # SPECIFICATION
    def _specs(self):
        nts_spec = "gor" if self.tmz_leve == "region" else "ua1998"
        # activity, .csv: taz_p, tour_id, freq, trip
        self.csv_tour = self.out_fldr / f"activity_{nts_spec}.csv"

        # mode time split, .csv: taz_o, taz_d, main_mode, purpose, trip_direction, start_time, freq, trip
        self.csv_dist = self.out_fldr / "distribution_{nts_spec}.csv"

        # tmz to taz lookup, csv: tmz, taz
        self.csv_ztaz = self.cfg.dir_import / "GOR_to_TAZ.csv"

        # nts to taz lookup, csv: nts, taz
        self.csv_ntaz = self.cfg.dir_import / "NTS_to_TAZ.csv"

        # mode-specific hb_production trip-end, csv: tmz_id, purpose, mode, trip
        self.csv_prod = self.cfg.dir_import / "tripend" / "NTEM7_prod_gor.csv"

        # mode-specific hb_attraction trip-end, csv: tmz_id, purpose, mode, trip
        self.csv_attr = self.cfg.dir_import / "tripend" / "NTEM7_attr_gor.csv"

    # READ DATA SPECS
    def _read_input(self):
        fun.log_stderr("\nRead input data")
        self.dfr_ztaz = self._read_csv(self.csv_ztaz)
        self.dfr_ntaz = self._read_csv(self.csv_ntaz)
        self.dfr_tour = self._read_csv(self.csv_tour)
        self.dfr_dist = self._read_csv(self.csv_dist)
        self.dfr_prod = self._read_csv(self.csv_prod)
        self.dfr_attr = self._read_csv(self.csv_attr)
        self._aggr_ua1998()

        # production zones to taz zones, tmz_no -> taz_no
        self.dct_ptaz = self.dfr_ztaz.set_index("tmz_id").to_dict()[
            f"{self.tmz_leve}_no"
        ]
        self.tmz_list = np.unique(self.dfr_ztaz[f"{self.tmz_leve}_no"].values)

        # nts zones to taz zone, nts -> taz.
        # note: tmz and taz zone system have to be the same, otherwise distribution won't work (yet)
        self.taz_leve = self.tmz_leve
        self.dfr_ntaz["nts"] = (
            self.dfr_ntaz[f"{self.taz_leve}_no"]
            if self.taz_leve == "region"
            else self.dfr_ntaz["nts"]
        )
        self.dct_ntaz = self.dfr_ntaz.set_index("nts").to_dict()[f"{self.taz_leve}_no"]
        self.taz_list = np.unique(self.dfr_ntaz[f"{self.taz_leve}_no"].values)

        # tmz to taz lookup, tmz -> taz
        if self.tmz_leve.lower() == self.taz_leve.lower():
            self.dct_ztaz = {key: key for key in self.tmz_list}
        else:
            self.dct_ztaz = self.dfr_ztaz.set_index(f"{self.tmz_leve}_no").to_dict()[
                f"{self.taz_leve}_no"
            ]

        # names for tmz, taz columns from NTS (currently only allow region & ua1998)
        self.col_prod = "gor_id"
        if self.taz_leve == "region":
            self.col_tour = "HHoldGOR_B02ID"
            self.col_dist = {"o": "TripOrigGOR_B02ID", "d": "TripDestGOR_B02ID"}
        else:
            self.col_tour = "HHoldUA_B01ID"
            self.col_dist = {"o": "TripOrigUA_B01ID", "d": "TripDestUA_B01ID"}

        # lower case for column names
        self.col_tour = self.col_tour.lower()
        self.col_prod = self.col_prod.lower()
        self.col_dist = {key: self.col_dist[key].lower() for key in self.col_dist}

        self.nts_list = (
            self.dfr_tour[self.col_tour].unique(),
            self.dfr_dist[self.col_dist["o"]].unique(),
            self.dfr_dist[self.col_dist["d"]].unique(),
        )
        self.nts_list = np.unique(np.concatenate(self.nts_list))
        self.ppx_list = np.unique(self.dfr_dist["purpose"].values)
        self.mdx_list = np.unique(self.dfr_dist["mode"].values)
        self.tsx_list = np.unique(self.dfr_dist["period"].values)

    def _aggr_ua1998(self):
        # aggregate scottish islands -> Highland, & poole -> Bournemouth
        # aggregate made for nts_2_taz and msoa_2_taz
        fun.log_stderr(f" .. address missing UAs ...")
        self.dfr_ntaz["ua_no"], self.dfr_ntaz["ua_de"] = (
            self.dfr_ntaz["ua1998_no"],
            self.dfr_ntaz["ua1998_de"],
        )
        self.dfr_ntaz.loc[
            self.dfr_ntaz["ua_no"].isin([331, 338, 340]), ["ua_no", "ua_de"]
        ] = [333, "Highland"]
        self.dfr_ntaz.loc[self.dfr_ntaz["ua_no"].isin([59]), ["ua_no", "ua_de"]] = [
            55,
            "Bournemouth",
        ]

        if "ua_no" in self.dfr_ztaz.columns:
            self.dfr_ztaz["ua_no"], self.dfr_ztaz["ua_de"] = (
                self.dfr_ztaz["ua1998_no"],
                self.dfr_ztaz["ua1998_de"],
            )
            self.dfr_ztaz.loc[
                self.dfr_ztaz["ua_no"].isin([331, 338, 340]), ["ua_no", "ua_de"]
            ] = [333, "Highland"]
            self.dfr_ztaz.loc[self.dfr_ztaz["ua_no"].isin([59]), ["ua_no", "ua_de"]] = [
                55,
                "Bournemouth",
            ]

    # CALCULATE PRODUCTION
    def _calc_prod(self, for_test: bool = False):
        fun.log_stderr(f" .. HB starting trip-ends ...")
        # convert NTEM data to standard format
        self.dfr_prod = self._stack_dfr(
            self.dfr_prod, [self.col_prod, "mode", "purpose", "direction"]
        )

        # return to this function
        dfr_prod = self.dfr_prod.rename(columns={"purpose": "p", "direction": "d"})
        dfr_prod = (
            dfr_prod.set_index(self.col_prod).rename(index=self.dct_ptaz).reset_index()
        )
        dfr_prod = dfr_prod.loc[(dfr_prod["p"] <= 8) & (dfr_prod["d"] == "hb_fr")]
        dfr_prod = dfr_prod.groupby(by=[self.col_prod, "p"])[["val"]].sum()
        dfr_prod.to_csv(
            self.out_fldr / self.fld_report / f"prod_{self.tmz_leve}_input.csv"
        ) if for_test else None
        self.dct_prod = dfr_prod["val"].to_dict()

        # get proportion of tmz trip-end per taz
        tmz_prop = dfr_prod.reset_index()
        tmz_prop["taz"] = tmz_prop[self.col_prod]
        tmz_prop = tmz_prop.set_index("taz").rename(index=self.dct_ztaz).reset_index()
        dfr_prod = tmz_prop.groupby(by=["taz", "p"])["val"].transform("sum")
        tmz_prop["val"] = tmz_prop["val"].div(dfr_prod).fillna(0)
        self.hbf_prop = tmz_prop.set_index([self.col_prod, "p"]).to_dict()["val"]

    # RE-ZONING FOR MODE-TIME-SPLIT
    def _calc_rezone(self, agg_rail: bool = False):
        fun.log_stderr(f" .. prepare NTS data ...")
        # mode-time split
        if agg_rail:
            self.dfr_dist.loc[
                self.dfr_dist["mode"] == 7, "mode"
            ] = 6  # as input trip-ends combine 6 & 7
        col_indx = [self.col_dist["o"], self.col_dist["d"]]
        self.dfr_dist = (
            self.dfr_dist.set_index(col_indx).rename(index=self.dct_ntaz).reset_index()
        )
        col_grby = [
            self.col_dist["o"],
            self.col_dist["d"],
            "mode",
            "purpose",
            "direction",
            "period",
            "trav_dist",
        ]
        self.dfr_dist = self.dfr_dist.groupby(by=col_grby, observed=True).sum()
        self.dfr_dist = self.dfr_dist.reset_index()
        # tour
        self.dfr_tour = (
            self.dfr_tour.set_index(self.col_tour)
            .rename(index=self.dct_ntaz)
            .reset_index()
        )
        self.dfr_tour = self.dfr_tour.groupby(by=[self.col_tour, "tour_id"]).sum()
        self.dfr_tour = self.dfr_tour.reset_index()

    # MODE-TIME SPLIT
    def _calc_mts(self, col_used: str = "freq", for_test: bool = False):
        fun.log_stderr("\nMode-time split probability")
        col_grby = [
            self.col_dist["o"],
            self.col_dist["d"],
            "mode",
            "purpose",
            "direction",
            "period",
        ]
        dfr_mode = self.dfr_dist.groupby(by=col_grby, observed=True)[[col_used]].sum()
        col_grby = [self.col_dist["o"], self.col_dist["d"], "purpose", "direction"]
        dfr_temp = dfr_mode.groupby(by=col_grby)[col_used].transform("sum")
        dfr_mode[col_used] = dfr_mode[col_used].div(dfr_temp).fillna(0)
        # mode-time %split
        col_grby = [
            self.col_dist["o"],
            self.col_dist["d"],
            "purpose",
            "direction",
            "mode",
            "period",
        ]
        dfr_mode = dfr_mode.groupby(by=col_grby, observed=True).sum()
        dfr_mode.to_csv(
            self.out_fldr / self.fld_report / "nts_mts_output.csv"
        ) if for_test else None
        self.dct_mode = dfr_mode[col_used].to_dict()

    # DISTRIBUTION PROBABILITY
    def _calc_dest(self, col_used: str = "freq", for_test: bool = False):
        fun.log_stderr("\nDistribution probability")
        col_grby = [
            self.col_dist["o"],
            self.col_dist["d"],
            "mode",
            "purpose",
            "direction",
            "period",
        ]
        dfr_dist = self.dfr_dist.groupby(by=col_grby, observed=True)[[col_used]].sum()
        dfr_temp = dfr_dist.groupby(by=[self.col_dist["o"], "purpose", "direction"])[
            col_used
        ].transform("sum")
        dfr_dist[col_used] = dfr_dist[col_used].div(dfr_temp).fillna(0)
        # distribution %split
        col_grby = [self.col_dist["o"], "purpose", "direction", self.col_dist["d"]]
        dfr_dist = dfr_dist.groupby(by=col_grby, observed=True).sum()
        dfr_dist.to_csv(
            self.out_fldr / self.fld_report / "nts_distr_output.csv"
        ) if for_test else None
        self.dct_dist = dfr_dist[col_used].to_dict()

    # ACTIVITY PROBABILITY
    def _calc_tour(self, val_incl: float = 0, for_test: bool = False):
        fun.log_stderr("\nTour probability")
        # filter tour that has +ve trips
        fun.log_stderr(f" .. filter tour_id with {val_incl}+ trips ...")
        col_used = "trip"  # for reporting purpose
        pct_incl = self.dfr_tour[col_used].sum()
        dfr_temp = self.dfr_tour.groupby(by="tour_id")[col_used].transform("sum")
        self.dfr_tour = self.dfr_tour.loc[dfr_temp > val_incl].reset_index(drop=True)
        self.uni_tour = np.unique(self.dfr_tour["tour_id"].values)
        pct_incl = 100 * self.dfr_tour[col_used].sum() / pct_incl
        fun.log_stderr(
            f"    {len(self.uni_tour)} tour_ids (with {val_incl}+ trips) selected, "
            f"account for {pct_incl:.2f}% of total NTS trips"
        )

        # process tour_id, using frequency as 1 tour may consist of multiple trips
        est_tour, pool = [], mp.Pool(self.num_cpus)
        for tor in self.uni_tour:
            pp_first = int(tor.split("_")[1])
            est_tour.append(
                pool.apply_async(self._tour_breakdown, [tor, pp_first, "freq"])
            )
        pool.close()
        pool.join()
        est_tour = [key.get() for key in est_tour]
        est_tour = pd.concat(est_tour, axis=1).fillna(0)

        # calculate tour %split
        est_tour = est_tour.div(est_tour.sum(axis=1, numeric_only=True), axis=0).fillna(
            0
        )
        est_tour.to_csv(
            self.out_fldr / self.fld_report / "nts_tour_output.csv"
        ) if for_test else None
        self.dct_tour = est_tour.to_dict("index")

    def _tour_breakdown(self, tor: str, pp_first: int, col_used: str) -> pd.DataFrame:
        """
        tour[i,0_x_y_z_0] = freq[i,0_x_y_z_0] / sum(freq[i,0_x_..._0], x=1-8)
        """
        dfr_temp = []
        fun.log_stderr(f" .. tour_id {tor} ...")
        if pp_first in self.ppx_list:
            for taz in self.taz_list:
                dfr_value = self.dfr_tour.loc[
                    (self.dfr_tour["tour_id"] == tor)
                    & (self.dfr_tour[self.col_tour] == taz)
                ][col_used].values
                dfr_temp.append(
                    [taz, pp_first, dfr_value[0] if len(dfr_value) > 0 else 0]
                )
        dfr_temp = pd.DataFrame(
            data=dfr_temp, columns=["taz", "purpose", tor]
        ).set_index(["taz", "purpose"])
        return dfr_temp

    # EXPAND TRIPS BY PURPOSES
    def _calc_trip_by_activity(self):
        fun.log_stderr("\nTrip by activity")
        est_trip, pool = [], mp.Pool(self.num_cpus)
        for tor in self.uni_tour:
            act_list = [int(key) for key in tor.split("_")]
            pp_first = act_list[1]
            if pp_first in self.ppx_list:
                est_trip.append(
                    pool.apply_async(self._activity_breakdown, [tor, act_list])
                )
                # a = self._activity_breakdown(tor, act_list)
        pool.close()
        pool.join()
        est_trip = [key.get() for key in est_trip]
        est_trip = pd.concat(est_trip, axis=0)
        col_grby = ["tmz_o", "tmz_d", "mode", "purpose", "direction", "period"]
        est_trip = est_trip.groupby(by=col_grby, observed=True).sum().reset_index()
        self.est_trip = est_trip.copy()

    # EXPAND TOUR
    def _activity_breakdown(self, tor: str, act_list: List) -> pd.DataFrame:
        """tmz and taz have to be the same here, otherwise it won't work,
        as I haven't figured out a way to apply the distribution split from taz level to tmz level yet
        outer loop: for tour[i, 0_p_y_z_0] in tour_list:
            first leg - hb_fr:
                prod[i,p] = prod[i,p] * tour[i,0_p_y_z_0]
                trip[i,j,p,m,t] = prod[i,p] * dest[i,p,fr,j] * mts[i,j,p,fr,m,t]
                attr[j,p] += trip[i,j,p,m,t]
            subsequent legs - nhb:
                inner loop: for p in [x,y,z] of the tour[i,0_x_y_z_0]:
                    orig[i,p] = attr[j,p] if 1st nhb else dest[j,p]
                    trip[i,j,p,m,t] = orig[i,p] * dest[i,p,nhb,j] * mts[i,j,p,nhb,m,t]
                    dest[p,j] += trip[i,j,p,m,t]
            last leg - hb_to:
                orig[i,p] = dest[j,p]
                trip[i,j,p,m,t] = orig[i,p] * dest[i,p,to,j] * mts[i,j,p,to,m,t]
        """
        fun.log_stderr(f" .. tour_id {tor} ...")
        tmz_attr, est_trip, tmz_prop = {}, [], 1
        col_calc = ["tmz_o", "tmz_d", "tour", "mode", "purpose", "direction", "period"]
        for leg in range(1, len(act_list)):
            pp_start, ppx_next = act_list[leg - 1], act_list[leg]
            act = f"{pp_start}_{ppx_next}"
            dir_type = "hb_fr" if pp_start == 0 else "hb_to" if ppx_next == 0 else "nhb"
            dfr_temp, tmz_attr[leg] = [], {tmz: 0 for tmz in self.tmz_list}
            if pp_start == 0:  # home-based: from-home trip,use ppx_next
                for tmz_o in self.tmz_list:
                    taz_o = self.dct_ztaz[tmz_o]
                    # probability that a specific tour that start with purpose p
                    tmz_prod = self._extract_prod(tmz_o, ppx_next) * self._extract_tour(
                        taz_o, ppx_next, tor
                    )
                    if tmz_prod > 0:
                        for tmz_d in self.tmz_list:
                            taz_d = self.dct_ztaz[tmz_d]
                            # tmz_prop = self.hbf_prop[(tmz_d, ppx_next)]
                            for md in self.mdx_list:
                                for ts in self.tsx_list:
                                    taz_dist = self._extract_dest(
                                        taz_o, ppx_next, dir_type, taz_d
                                    )
                                    taz_fact = self._extract_mts(
                                        taz_o, taz_d, ppx_next, dir_type, md, ts
                                    )
                                    tmz_trip = tmz_prod * taz_dist * taz_fact
                                    tmz_attr[leg][tmz_d] += tmz_trip
                                    dfr_temp.append(
                                        [
                                            tmz_o,
                                            tmz_d,
                                            tor,
                                            md,
                                            act,
                                            dir_type,
                                            ts,
                                            tmz_trip,
                                        ]
                                    )
            elif pp_start > 0 and ppx_next > 0:  # none home-based trips
                for tmz_o in self.tmz_list:
                    taz_o = self.dct_ztaz[tmz_o]
                    if tmz_attr[leg - 1][tmz_o] > 0:
                        for tmz_d in self.tmz_list:
                            taz_d = self.dct_ztaz[tmz_d]
                            # tmz_prop = nhb_prop[(tmz_d, ppx_next)]
                            for md in self.mdx_list:
                                for ts in self.tsx_list:
                                    taz_dist = self._extract_dest(
                                        taz_o, ppx_next, dir_type, taz_d
                                    )
                                    taz_fact = self._extract_mts(
                                        taz_o, taz_d, ppx_next, dir_type, md, ts
                                    )
                                    tmz_trip = (
                                        tmz_attr[leg - 1][tmz_o] * taz_dist * taz_fact
                                    )
                                    tmz_attr[leg][tmz_d] += tmz_trip
                                    dfr_temp.append(
                                        [
                                            tmz_o,
                                            tmz_d,
                                            tor,
                                            md,
                                            f"x{act[1:]}",
                                            dir_type,
                                            ts,
                                            tmz_trip,
                                        ]
                                    )
            else:  # home-based: to-home trips
                for tmz_o in self.tmz_list:
                    taz_o = self.dct_ztaz[tmz_o]
                    if tmz_attr[leg - 1][tmz_o] > 0:
                        for tmz_d in self.tmz_list:
                            taz_d = self.dct_ztaz[tmz_d]
                            # tmz_prop = hbt_prop[(tmz_d, pp_start)]
                            for md in self.mdx_list:
                                for ts in self.tsx_list:
                                    taz_dist = self._extract_dest(
                                        taz_o, pp_start, dir_type, taz_d
                                    )
                                    taz_fact = self._extract_mts(
                                        taz_o, taz_d, pp_start, dir_type, md, ts
                                    )
                                    tmz_trip = (
                                        tmz_attr[leg - 1][tmz_o] * taz_dist * taz_fact
                                    )
                                    dfr_temp.append(
                                        [
                                            tmz_o,
                                            tmz_d,
                                            tor,
                                            md,
                                            act,
                                            dir_type,
                                            ts,
                                            tmz_trip,
                                        ]
                                    )
            # create dataframe
            dfr_temp = pd.DataFrame(data=dfr_temp, columns=col_calc + ["trips"])
            est_trip.append(dfr_temp)
        est_trip = pd.concat(est_trip, axis=0).groupby(by=col_calc).sum()
        return est_trip.loc[est_trip["trips"] > 0]

    # ADJUST ATTRACTIONS
    def _adjust_tripend(self):
        fun.log_stderr("\nAdjust NoTEM trip-ends")
        # produce adjusted trip-ends by mode/time-specific
        out_data = {"o": [], "d": []}
        cur_path = self.out_fldr / "tripend"
        fun.mkdir(cur_path)
        # hb_fr (PA - mode agnostic at 24hr to mode specific & time period)
        pool = mp.Pool(2)
        par_pool = [
            "prod",
            "hb_fr",
            self._adjust_aggs(self.dfr_prod, "both"),
            "both",
            rf"{cur_path}\productions_hb_fr.csv",
        ]
        out_data["o"].append(pool.apply_async(self._adjust_engine, par_pool))
        par_pool = [
            "attr",
            "hb_fr",
            self._adjust_aggs(self.dfr_attr, "both"),
            "both",
            rf"{cur_path}\attractions_hb_fr.csv",
        ]
        out_data["d"].append(pool.apply_async(self._adjust_engine, par_pool))
        pool.close()
        pool.join()
        # nhb (OD - mode agnostic by time period to mode specific & time period)
        pool = mp.Pool(2)
        par_pool = [
            "orig",
            "nhb",
            self._adjust_aggs(rf"{cur_path}\attractions_hb_fr.csv", "both"),
            "both",
            rf"{cur_path}\productions_nhb.csv",
        ]
        out_data["o"].append(pool.apply_async(self._adjust_engine, par_pool))
        par_pool = [
            "dest",
            "nhb",
            self._adjust_aggs(rf"{cur_path}\attractions_hb_fr.csv", "both"),
            "both",
            rf"{cur_path}\attractions_nhb.csv",
        ]
        out_data["d"].append(pool.apply_async(self._adjust_engine, par_pool))
        pool.close()
        pool.join()
        # hb_to (OD - mode agnostic by time period to mode specific & time period)
        pool = mp.Pool(2)
        par_pool = [
            "orig",
            "hb_to",
            self._adjust_aggs(rf"{cur_path}\attractions_nhb.csv", "both"),
            "both",
            rf"{cur_path}\productions_hb_to.csv",
        ]
        out_data["o"].append(pool.apply_async(self._adjust_engine, par_pool))
        par_pool = [
            "dest",
            "hb_to",
            self._adjust_aggs(rf"{cur_path}\productions_hb_fr.csv", "both"),
            "both",
            rf"{cur_path}\attractions_hb_to.csv",
        ]
        out_data["d"].append(pool.apply_async(self._adjust_engine, par_pool))
        pool.close()
        pool.join()
        # summary
        out_data = {od: [itm.get() for itm in out_data[od]] for od in out_data}
        out_data = {
            od: pd.concat(out_data["o"], axis=0).rename(columns={"val": odx})
            for od, odx in zip(out_data, ["orig", "dest"])
        }
        out_data = pd.concat([out_data["o"], out_data["d"]], axis=1)
        out_data.to_csv(self.out_fldr / self.fld_report / "notem_summary_output.csv")

    @staticmethod
    def _stack_dfr(dfr: pd.DataFrame, col_grby: Union[List, str]) -> pd.DataFrame:
        lev = len(col_grby)
        dfr = (
            dfr.set_index(col_grby)
            .stack()
            .reset_index(name="val")
            .rename(columns={f"level_{lev}": "period"})
        )
        dfr["period"] = dfr["period"].astype(int)
        return dfr.groupby(col_grby + ["period"])[["val"]].sum().reset_index()

    # internal function 1
    def _adjust_aggs(
        self, csv_file: Union[pd.DataFrame, str], agg_type: str = "none"
    ) -> pd.DataFrame:
        """aggregate trip-end data to mode/time agnostics.
        agg_type:
            mode - aggregate to all mode totals (segmented by [tmz, p, tp])
            time - aggregate to all-week totals (segmented by [tmz, p, m])
            both - aggregate to all modes, 24hrs (segmented by [tmz, p])
        """
        dfr_data = self._read_csv(csv_file) if type(csv_file) is str else csv_file
        if agg_type.lower() == "mode":
            dfr_data = (
                dfr_data.groupby(by=[self.col_prod, "p", "tp"])[["val"]]
                .sum()
                .reset_index()
            )
        elif agg_type.lower() == "time":
            dfr_data = (
                dfr_data.groupby(by=[self.col_prod, "p", "m"])[["val"]]
                .sum()
                .reset_index()
            )
        elif agg_type.lower() == "both":
            dfr_data = (
                dfr_data.groupby(by=[self.col_prod, "p"])[["val"]].sum().reset_index()
            )
        return dfr_data

    # internal function 2
    @staticmethod
    def _adjust_fill(dfr_data: pd.DataFrame, col_dict: Dict) -> pd.DataFrame:
        """add column(s) to to further segment dataframe"""
        dfr_tend: Union[pd.DataFrame, List] = []
        col_dict = {
            col: list(col_dict[col])
            if type(col_dict[col]) is not list
            else col_dict[col]
            for col in col_dict
        }
        num_2add = np.prod([len(col_dict[key]) for key in col_dict])
        col_dict = {
            "col": [[key for key in col_dict] for _ in range(num_2add)],
            "val": [list(col) for col in itertools.product(*col_dict.values())],
        }
        for col, val in zip(col_dict["col"], col_dict["val"]):
            dfr_temp = dfr_data.copy()
            dfr_temp[col] = val
            dfr_tend.append(dfr_temp)
        dfr_tend = pd.concat(dfr_tend, axis=0)
        return dfr_tend

    # internal function 3
    def _adjust_engine(
        self,
        ted_type: str,
        dir_type: str,
        dfr_data: pd.DataFrame,
        col_2add: str,
        csv_file: str,
    ) -> pd.DataFrame:
        """
        adjust and produce prod/attr trip-ends by mode/time specific for DiMo
        col_2add: mode, time, both, or none
            mode: dfr_data already segmented by [p, tp], thus [m] needs infill
            time: dfr_data already segmented by [p, tp], thus [tp] needs infill
            both: dfr_data only segmented by [p], thus both [m, tp] needs infill
            none: def_data already segmented by [p, m, tp], thus no need infill
        """
        # filter relevant trips
        fun.log_stderr(f" .. {dir_type} ({ted_type}) trip-ends ...")
        est_trip = self.est_trip.loc[
            self.est_trip["direction"] == dir_type
        ].reset_index(drop=True)
        est_trip["purpose"] = (
            est_trip["purpose"].apply(lambda x: int(x[-1]))
            if dir_type in ["hb_fr", "nhb"]
            else est_trip["purpose"].apply(lambda x: int(x[0]))
        )
        est_trip["purpose"] = est_trip["purpose"] + (10 if dir_type == "nhb" else 0)
        tmz_type = "tmz_o" if ted_type in ["prod", "orig"] else "tmz_d"
        est_trip = (
            est_trip.groupby(by=[tmz_type, "mode", "purpose", "period"])[["trips"]]
            .sum()
            .reset_index()
        )

        # process input trip-end
        mdx_list = np.unique(est_trip["mode"].values)
        tsx_list = np.unique(est_trip["period"].values)
        if col_2add.lower() == "mode":
            dfr_tend = self._adjust_fill(dfr_data, {"m": mdx_list})
        elif col_2add.lower() == "time":
            dfr_tend = self._adjust_fill(dfr_data, {"tp": tsx_list})
        elif col_2add.lower() == "both":
            dfr_tend = self._adjust_fill(dfr_data, {"m": mdx_list, "tp": tsx_list})
        else:
            dfr_tend = dfr_data.copy()
        # rezoning
        dfr_tend["p"] = dfr_tend["p"] + (
            10
            if dir_type == "nhb" and dfr_tend["p"].max() < 10
            else -10
            if dir_type != "nhb" and dfr_tend["p"].min() > 10
            else 0
        )
        dfr_tend[self.tmz_leve] = dfr_tend[self.col_prod]
        dfr_tend = (
            dfr_tend.set_index(self.tmz_leve).rename(index=self.dct_ptaz).reset_index()
        )
        # merge data
        dfr_tend = pd.merge(
            dfr_tend,
            est_trip,
            how="left",
            left_on=[self.tmz_leve, "m", "p", "tp"],
            right_on=[tmz_type, "mode", "purpose", "period"],
        ).fillna(0)
        dfr_tend.drop(columns=[tmz_type, "mode", "purpose", "period"], inplace=True)
        # apply mode-time split from est_trip
        if col_2add.lower() == "mode":
            dfr_temp = dfr_tend.groupby(by=[self.col_prod, "p", "tp"])[
                "trips"
            ].transform("sum")
            dfr_tend["val"] = (
                dfr_tend["val"].div(dfr_temp).mul(dfr_tend["trips"]).fillna(0)
            )
        elif col_2add.lower == "time":
            dfr_temp = dfr_tend.groupby(by=[self.col_prod, "p", "m"])[
                "trips"
            ].transform("sum")
            dfr_tend["val"] = (
                dfr_tend["val"].div(dfr_temp).mul(dfr_tend["trips"]).fillna(0)
            )
        elif col_2add.lower() == "both":
            dfr_temp = dfr_tend.groupby(by=[self.col_prod, "p"])["trips"].transform(
                "sum"
            )
            dfr_tend["val"] = (
                dfr_tend["val"].div(dfr_temp).mul(dfr_tend["trips"]).fillna(0)
            )
        # control to est_trip totals
        dfr_temp = dfr_tend.groupby(by=[self.tmz_leve, "p", "m", "tp"])[
            "val"
        ].transform("sum")
        dfr_tend["val"] = dfr_tend["val"].div(dfr_temp).mul(dfr_tend["trips"]).fillna(0)
        dfr_tend.drop(columns=[self.tmz_leve, "trips"], inplace=True)
        # output to csv
        dfr_tend.to_csv(csv_file, index=False)
        # summary
        dfr_tend = dfr_tend.groupby(by=["p", "m", "tp"])[["val"]].sum().reset_index()
        dfr_tend["p"] = dfr_tend["p"] - (10 if dir_type == "nhb" else 0)
        dfr_tend["di"] = dir_type
        return dfr_tend.set_index(["m", "p", "di", "tp"])

    # STATISTICS
    def _reports(self):
        # summary
        col_grby = ["mode", "purpose", "direction", "period"]
        col_mode, col_purp, col_zone = (
            self._col_mode(True),
            self._col_purp(),
            self._col_zone(self.tmz_leve),
        )
        est_trip = (
            self.est_trip.set_index(["purpose"])
            .rename(index=self._act_to_purp())
            .reset_index()
        )

        # p/a trip-ends
        self.dfr_attr = self._stack_dfr(
            self.dfr_attr, [self.col_prod, "mode", "purpose", "direction"]
        )
        nts_trip = {"o": self.dfr_prod, "d": self.dfr_attr}
        for od, odx in zip(["o", "d"], ["prod", "attr"]):
            ted_data = (
                est_trip.groupby([f"tmz_{od}"] + col_grby)["trips"].sum().reset_index()
            )
            nts_data = nts_trip[od].rename(columns={self.col_prod: f"tmz_{od}"})
            ted_data = pd.merge(
                ted_data, nts_data, how="outer", on=[f"tmz_{od}"] + col_grby
            ).fillna(0)
            ted_data.rename(columns={"val": "input", "trips": "output"}, inplace=True)
            ted_data = (
                ted_data.set_index(["purpose"]).rename(index=col_purp).reset_index()
            )
            ted_data = ted_data.set_index(["mode"]).rename(index=col_mode).reset_index()
            ted_data = ted_data.set_index([f"tmz_{od}"]).rename(index=col_zone)
            ted_data.to_csv(
                self.out_fldr / self.fld_report / f"{odx}_{self.tmz_leve}_output.csv",
                index=True,
            )

        # output matrix for analysis
        est_trip = est_trip.set_index(["purpose"]).rename(index=col_purp).reset_index()
        est_trip = est_trip.set_index(["mode"]).rename(index=col_mode).reset_index()
        est_trip = est_trip.set_index(["tmz_o", "tmz_d"]).rename(index=col_zone)
        est_trip.to_csv(
            self.out_fldr / self.fld_report / f"matrix_{self.tmz_leve}_output.csv",
            index=True,
        )

    # SUPPORTING MODULES
    def _extract_prod(self, taz_o: int, pp: int) -> float:
        """
        production trip-end: prod[p, i]
        """
        return self.dct_prod.get((taz_o, pp), 0)

    def _extract_tour(self, taz_o: int, pp: int, tour: str) -> float:
        """
        tour probability: tour[i, 0_p_y_z_0]
        """
        return self.dct_tour.get((taz_o, pp), {tour: 0})[tour]

    def _extract_mts(
        self, taz_o: int, taz_d: int, pp: int, direction: str, md: int, time_start: int
    ) -> float:
        """
        mode-time probability: mts[i, j, p, d, m, t]
        """
        return self.dct_mode.get((taz_o, taz_d, pp, direction, md, time_start), 0)

    def _extract_dest(
        self, taz_o: int, pp: int, direction: str, taz_d: int, taz_prop: float = 1
    ) -> float:
        """
        destination probability: dest[i, p, d, j]
        """
        return self.dct_dist.get((taz_o, pp, direction, taz_d), 0) * taz_prop

    @staticmethod
    def _extract_vmax(taz_qnty: Dict, taz_o: int, taz_d: int) -> int:
        """
        calculate number of zones that are within a sector
        """
        return taz_qnty[taz_o] if taz_qnty[taz_o] > taz_qnty[taz_d] else taz_qnty[taz_d]

    @staticmethod
    def _read_csv(csv_file: str) -> pd.DataFrame:
        """
        read and convert csv to dataframe
        """
        fun.log_stderr(f" .. read {csv_file} ...")
        dfr_data = pd.read_csv(csv_file, header=0)
        dfr_data.rename(
            columns={key: key.lower() for key in dfr_data.columns}, inplace=True
        )
        return dfr_data

    @staticmethod
    def _act_to_purp():
        col_name = {}
        ppx_name = {key: key for key in range(1, 10)}
        typ_name = {"0_{}": "{}", "x_{}": "{}", "{}_0": "{}"}
        for tyx in typ_name:
            for ppx in ppx_name:
                col_name[tyx.format(ppx)] = int(typ_name[tyx].format(ppx_name[ppx]))
        return col_name

    @staticmethod
    def _col_purp():
        return {
            1: "Commuting",
            2: "Emp.business",
            3: "Education",
            4: "Shopping",
            5: "Per.business",
            6: "Social",
            7: "Visit friends",
            8: "Holiday",
            9: "NA",
        }

    @staticmethod
    def _col_purp_old():
        col_name = {}
        ppx_name = {
            1: "w",
            2: "eb",
            3: "edu",
            4: "shop",
            5: "pb",
            6: "social",
            7: "vf",
            8: "hol",
            9: "esc",
        }
        typ_name = {"0_{}": "hb{}", "x_{}": "nhb{}", "{}_0": "hb{}"}
        for tyx in typ_name:
            for ppx in ppx_name:
                col_name[tyx.format(ppx)] = typ_name[tyx].format(ppx_name[ppx])
        return col_name

    def _col_zone(self, tmz_type: str = "ua1998"):
        tmz_type = tmz_type.lower()
        col_name = self.dfr_ztaz.set_index(f"{tmz_type}_no").to_dict()[f"{tmz_type}_de"]
        return col_name

    @staticmethod
    def _col_ua98(dfr_data: pd.DataFrame):
        pass

    @staticmethod
    def _col_mode(agg_rail: bool = True):
        col_name = {1: "Walk", 2: "Cycle", 3: "Car", 4: "Van", 5: "Bus"}
        if agg_rail:
            col_name.update({6: "Rail", 8: "Air"})
        else:
            col_name.update({6: "Surface rail", 7: "Light rail", 8: "Air"})
        return col_name

    @staticmethod
    def _dup_to_tuple(val_list: List) -> List:
        # create a list of tuple with value of consecutive duplicates
        # eg. [0, 1, 2, 2, 2, 2, 3, 3, 3, 0] -> [(0, 1), (1, 1), (2, 4), (3, 3), (0, 1)]
        return [
            (key, sum(1 for _ in group)) for key, group in itertools.groupby(val_list)
        ]


# MSOA to NORMS CONVERSION
class MSOAtoNoRMS:
    def __init__(self):
        self.nts_fldr = os.path.dirname(os.path.realpath(__file__))
        self.csv_prod = (
            r"D:\NTS\NoTEM\tripend\full_week_hb_production_9-10_p_m_msoa.csv"
        )
        self.csv_attr = (
            r"D:\NTS\NoTEM\tripend\full_week_hb_attraction_9-10_p_m_msoa.csv"
        )
        self.csv_pops = r"D:\NTS\NoTEM\lookups\msoa_to_norms_population_weight.csv.bz2"
        self.csv_emps = r"D:\NTS\NoTEM\lookups\msoa_to_norms_employment_weight.csv.bz2"

        self._msoa_to_norms()

    @staticmethod
    def _read_csv(csv_file: str) -> pd.DataFrame:
        """
        read and convert csv to dataframe
        """
        fun.log_stderr(f" .. read {csv_file} ...")
        dfr_data = pd.read_csv(csv_file, header=0)
        dfr_data.rename(
            columns={key: key.lower() for key in dfr_data.columns}, inplace=True
        )
        return dfr_data

    def _msoa_to_norms(self):
        # read data
        lok_list = [self.csv_pops, self.csv_emps]
        ted_list = [self.csv_prod, self.csv_attr]
        for key, lok, ted in zip(["production", "attraction"], lok_list, ted_list):
            # read lookup file
            tmz_tran = self._read_csv(lok)
            sec_2tmz = tmz_tran.groupby("msoa_zone_id")["msoa_to_norms"].transform(
                "sum"
            )
            tmz_tran["msoa_to_norms"] = (
                tmz_tran["msoa_to_norms"].div(sec_2tmz).fillna(0)
            )
            # read trip-end file
            ted_data = self._read_csv(ted)
            sum_pres = (
                ted_data.groupby(["m", "p"])[["val"]]
                .sum()
                .rename(columns={"val": "pres"})
            )
            ted_data = pd.merge(
                tmz_tran, ted_data, how="left", on="msoa_zone_id", suffixes=("", "")
            )
            ted_data["val"] = ted_data["val"].mul(ted_data["msoa_to_norms"]).fillna(0)
            ted_data = (
                ted_data.groupby(["norms_zone_id", "p", "m"])["val"].sum().reset_index()
            )
            # check
            sum_post = (
                ted_data.groupby(["m", "p"])[["val"]]
                .sum()
                .rename(columns={"val": "post"})
            )
            sum_post = pd.concat([sum_pres, sum_post], axis=1)
            sum_post["diff"] = sum_post["post"] - sum_post["pres"]
            ted_data.to_csv(
                rf"{self.nts_fldr}\tripend\full_week_hb_{key}_9-10_p_m_norms.csv",
                index=False,
            )
