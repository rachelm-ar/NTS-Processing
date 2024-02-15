import mdlconfig
import mdlfunction as fun
from typing import Union, List, Dict
import mdllookup as luk
import pandas as pd
import numpy as np


class Stage:
    """produce stage-related analysis:
    1. personal affordability
    2. etc.
    """

    def __init__(
        self, nts_fldr: str, cb_version: Union[str, int], over_write: bool = True
    ):
        fun.log_stderr("\n***** NTS STAGE OUTPUTS *****")
        # read config
        self.cfg = mdlconfig.Config(nts_fldr)
        self.luk = luk.Lookup.load_yaml("lookup.yml")
        self.tfn_ttype, self.tfn_atype = self.cfg.tfn_ttype, self.cfg.tfn_atype
        self.cb_version, self.tfn_mode = cb_version, self.cfg.tfn_modes

        # read in cb data
        if over_write:
            fun.log_stderr("Import cb data")
            # income tax/ni bracket
            self.dfr_itax = fun.csv_to_dfr(
                self.cfg.dir_import / "income_tax_brackets.csv"
            )
            self.dfr_itax.set_index(["year", "type"], inplace=True)
            self.dfr_itax = self.dfr_itax.astype(float)
            self.dct_itax = self.dfr_itax.to_dict()
            # fuel costs
            self.dfr_fuel = fun.csv_to_dfr(
                self.cfg.dir_import / "fuel_cost_per_litre.csv"
            )
            self.dfr_fuel.set_index("year", inplace=True)
            self.dfr_fuel = self.dfr_fuel.astype(float)
            # nts classified build
            nts_data = (
                self.cfg.dir_cbuild
                / f"{self.cfg.csv_cbuild}_stage_v{self.cb_version}.csv"
            )
            nts_data = fun.csv_to_dfr(nts_data)
            self._veh_occupancy(
                nts_data, [3, 4], "gor", None, None, [0, 5, 10, 25, 50, 100, 200, 1999]
            )
            self._affordability(nts_data, ["surveyyear", "hholdgor_b02id", "ns"])
        else:
            fun.log_stderr(f" .. skipped!")

    def _affordability(
        self, dfr: pd.DataFrame, seg_incl: Union[str, List], by_individual: bool = True
    ):
        fun.log_stderr("Personal affordability")
        # total household incomes
        col_grby = seg_incl if isinstance(seg_incl, list) else [seg_incl]
        col_hold = ["householdid"] if "householdid" not in col_grby else []
        if by_individual:
            col_hold = col_hold + ["individualid"]
            hhi = (
                dfr.groupby(col_hold + col_grby)[["income", "w2"]].mean().reset_index()
            )
        else:
            hhi = (
                dfr.groupby(col_hold + col_grby)[["hh_income", "w2"]]
                .mean()
                .reset_index()
            )
            hhi.rename(columns={"hh_income": "income"}, inplace=True)
        if "surveyyear" in col_grby:
            hhi["weekly_income"] = hhi.apply(
                lambda x: self._net_income_yr(x["surveyyear"], x["income"]), axis=1
            )
        else:
            hhi["weekly_income"] = hhi["income"].apply(lambda x: self._net_income(x))
        hhi["weekly_income"] = hhi["weekly_income"].mul(hhi["w2"]).div(52)
        hhi["w2"] = hhi["w2"].div(hhi.groupby(["householdid"])["w2"].transform("count"))
        hhi = (
            hhi.groupby(col_grby)
            .agg({"weekly_income": "sum", "w2": "sum"})
            .reset_index()
        )
        # car fuel costs
        dfr["stagefuel"], car_mask = 0.0, dfr["stagemode"].isin([3, 4])  # car & van
        dfr.loc[car_mask & (dfr["fueltype"] == 1), "stagefuel"] = self._lpk_tag(
            dfr, "petrol"
        )
        dfr.loc[car_mask & (dfr["fueltype"] == 2), "stagefuel"] = self._lpk_tag(
            dfr, "diesel"
        )
        dfr.loc[car_mask & (dfr["fueltype"] == 3), "stagefuel"] = self._lpk_tag(
            dfr, "electric"
        )
        dfr.loc[
            car_mask & (dfr["fueltype"].isin([4, 5, 6, 7, 8])), "stagefuel"
        ] = self._lpk_tag(dfr, "other")
        # car/pt fare costs
        col_grby = col_grby + ["stagemode"]
        col_calc = ["stagedistance", "stagefuel", "stagecost"]
        out = dfr[col_grby + col_calc + ["fueltype", "w2", "w5xhh"]].copy()
        for col in col_calc:
            out[col] = out[col].mul(out["w2"])  # .mul(out['w5xhh'])
        out["stagedistance"] = out["stagedistance"].mul(self.cfg.m2k_fact)
        out.loc[
            (out["fueltype"] <= 0) & (out["stagemode"].isin([3, 4])), "stagedistance"
        ] = 0
        out = out.groupby(col_grby)[col_calc].sum()
        out = fun.dfr_complete(out, None, "stagemode").reset_index()
        # merge with income data
        out = pd.merge(
            out, hhi, how="left", on=[col for col in col_grby if col != "stagemode"]
        )
        out = (
            out.set_index("stagemode")
            .rename(index=self.luk.mode("stage")["out"])
            .reset_index()
        )
        if "hholdgor_b02id" in seg_incl:
            out = (
                out.set_index("hholdgor_b02id")
                .rename(index=self.luk.gor_02id)
                .reset_index()
            )
        # write result
        fun.dfr_to_csv(
            out,
            self.cfg.dir_output / self.cfg.fld_stage,
            "personal_affordability",
            False,
        )

    def _veh_occupancy(
        self,
        dfr: pd.DataFrame,
        mode: List = None,
        geo_incl: Union[str, None] = None,
        geo_list: Union[List, Dict] = None,
        seg_incl: Union[List, str] = None,
        agg_band: Union[bool, List] = True,
    ):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # geo_list: list of geo_area to be included
        # agg_band: False, True, or a list of user-defined distance bands
        fun.log_stderr("\nNTS occupancies")
        fun.log_stderr(f" .. process data")
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        lev_orig, lev_dest = lev_2col["o"], lev_2col["d"]
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        dfr["stage_dist"] = dfr["stagedistance"].mul(self.cfg.m2k_fact)
        col_used = [
            "stagemode",
            "purpose",
            "direction",
            "period",
            "stageoccupant",
            "stage_dist",
        ]
        col_used = [lev_orig, lev_dest] + col_used if geo_incl is not None else col_used
        if agg_band or isinstance(agg_band, List):
            col_dist = dfr["stage_dist"].values
            rng_dist = (
                np.array(agg_band)
                if isinstance(agg_band, List)
                else fun.dist_band(col_dist.max())
            )
            dfr["dist_band"] = pd.cut(
                col_dist, rng_dist, right=False, labels=rng_dist[1:]
            )
            dfr["stage_dist"], col_used = 999, col_used + ["dist_band"]
        dfr = dfr[col_used + seg_incl + ["trips"]].copy()
        dfr = (
            dfr.groupby(col_used + seg_incl, observed=True)[["trips"]]
            .sum(col_type)
            .reset_index()
        )
        # write output
        fun.log_stderr(f" .. write output")
        out_fldr = self.cfg.dir_output / self.cfg.fld_occs
        mode = self.tfn_mode if mode is None else mode
        if geo_list is not None and geo_incl is not None:
            dct = fun.list_to_dict(geo_list)
            msk_orig = (
                dfr[lev_orig].str.lower().isin(list(dct))
                if col_type is str
                else dfr[lev_orig].isin(list(dct))
            )
            msk_dest = (
                dfr[lev_dest].str.lower().isin(list(dct))
                if col_type is str
                else dfr[lev_dest].isin(list(dct))
            )
            dfr = dfr.loc[msk_orig | msk_dest]
            if isinstance(geo_list, dict):
                dfr = (
                    dfr.set_index([lev_orig, lev_dest]).rename(index=dct).reset_index()
                )
        dfr["purpose"] = self.luk.nhb_renumber(dfr, col_type)
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl)
        dfr = fun.dfr_filter_mode(dfr, mode, "stagemode")
        dfr = (
            dfr.set_index(["triporiggor_b02id", "tripdestgor_b02id"])
            .rename(index=self.luk.gor_02id)
            .reset_index()
        )
        dfr = (
            dfr.set_index(["purpose"])
            .rename(index=self.luk.purpose()["out"])
            .reset_index()
        )
        fun.dfr_to_csv(dfr, out_fldr, "vehicle_occupancy_stage", False)

    # disposable income factor
    @staticmethod
    def _net_income(x: Union[int, float]) -> float:
        y0 = min(10_000, x)
        y1 = max((1 - 0.20 - 0.12) * (min(x, 50_000) - y0), 0)
        y2 = max((1 - 0.40 - 0.02) * (min(x, 150_000) - 50_000), 0)
        y3 = max((1 - 0.45 - 0.02) * (x - 150_000), 0)
        return y0 + y1 + y2 + y3

    def _net_income_yr(self, yr: int, gross: Union[float, int]) -> float:
        # income tax & ni rates obtained from below:
        # https://www.gov.uk/national-insurance-rates-letters
        # https://www.gov.uk/government/statistics/main-features-of-national-insurance-contributions
        # https://www.gov.uk/government/publications/rates-and-allowances-income-tax
        # https://www.gov.uk/government/statistics/rates-of-income-statistics

        dct_0, tax_both = self.dct_itax["tax_free"], 0
        tax_0, nin_0 = dct_0[(yr, "tax")], dct_0[(yr, "ni")]
        tax_p, nin_p = tax_0, nin_0
        for key in [1, 2, 3]:
            dct_v, dct_r = self.dct_itax[f"b{key}_value"], self.dct_itax[f"b{key}_rate"]
            tax_v, nin_v = dct_v[(yr, "tax")], dct_v[(yr, "ni")]
            tax_r, nin_r = dct_r[(yr, "tax")], dct_r[(yr, "ni")]
            tax_i = max(tax_r * (min(gross, tax_0 + tax_v) - tax_p), 0) / 100
            nin_i = max(nin_r * (min(gross, nin_v) - nin_p), 0) / 100
            tax_p, nin_p = tax_0 + tax_v, nin_v
            tax_both += tax_i + nin_i
        return gross - tax_both

    def _lpk_tag(self, dfr: pd.DataFrame, fuel_type: str = "other") -> pd.Series:
        # fuel prices - https://www.gov.uk/government/statistics/weekly-road-fuel-prices
        fuel = self.dfr_fuel[fuel_type].to_dict()
        # lpk = (a/v + b + c.v + d.v2) - TAG DataBook May 23
        if fuel_type == "petrol":
            a, b, c, d = 0.451946800, 0.096046026, -0.001094078, 0.000007246
        elif fuel_type == "diesel":
            a, b, c, d = 0.481912969, 0.069094402, -0.000664707, 0.000005238
        elif fuel_type == "electric":
            a, b, c, d = 0.000000000, 0.220656352, -0.000000000, 0.000000000
        else:
            a, b, c, d = 0.430405115, 0.077775873, -0.000835678, 0.000005878
        cst = dfr["surveyyear"].apply(lambda x: fuel[x]) / 100
        spd = dfr["stagedistance"].mul(self.cfg.m2k_fact).div(dfr["stagetime"].div(60))
        spd.loc[spd < 10], spd.loc[spd > 100] = 10, 100
        lpk = a / spd + b + c * spd + d * spd**2
        return lpk * dfr["stagedistance"] * self.cfg.m2k_fact * cst
