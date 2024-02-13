import mdlconfig
import mdlfunction as fun
from typing import Union, List
import mdllookup as luk
import pandas as pd
import numpy as np
from sklearn.preprocessing import OneHotEncoder
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_validate

class Output:
    """ outputs from NTS classified build, including:
        1. hb trip-rates (BETA_hsr) by hb purpose (h), traveller_type (s), area_type (r)
        2. hb mode-time-split (RHO_mdhsr) by mode (m), time (d), purpose (h), tt (s), at(r)
        3. nhb trip weights (GAMMA_nmhm) by nhb purpose (n), mode (m), hb attraction purpose (h) and mode (m)
        4. nhb time-split (RHO_dmnr) by time(d), mode (m), purpose (n), area_type (r)
        5. hb tour proportions (PHI_hdhd) by outward purpose (h.o), time (d.o), return purpose (h.r), time (d.r)
        6. attraction trip-rates (ALPHA1_per) by purpose (p), employment type (e), area_type (r)
        7. attraction mode split (ALPHA2_pmkr) by purpose (p), mode (m), J2W type (k) area_type (r)
        8. trip-length distribution (TLD_rmpt) gor (r) by mode (m), purpose (p), time period (t)
        9. car occupancy (Occ_rptk) gor (r) purpose (p), time period (t), distance band (k)
        10. trip chaining by GOR (Tour_r,0xyz0) by gor (r), tour_id home->p1->p2->..->home
        11. trip-rate changes between pre- and post-COVID
        12. trip-length distribution pre- and post-COVID
        13. proportion of wfh by year (not used)
    """

    def __init__(self, nts_fldr: str, cb_version: Union[str, int], over_write: bool = True):
        fun.log_stderr('\n***** NTS TRIP OUTPUTS *****')
        # read config
        self.cfg = mdlconfig.Config(nts_fldr)
        self.luk = luk.Lookup.load_yaml(r"E:\NTS\analysis\outputs\lookup.yml")
        self.tfn_ttype, self.tfn_atype = self.cfg.tfn_ttype, self.cfg.tfn_atype
        self.cb_version, self.tfn_mode = cb_version, self.cfg.tfn_modes
        self.tvt_list = self.luk.tt_to_dfr(self.tfn_ttype, self.cfg.def_ttype)

        # read in cb data
        if over_write:
            fun.log_stderr('Import cb data')
            nts_fldr = f'{self.cfg.dir_cbuild}\\{self.cfg.csv_cbuild}_v{self.cb_version}.csv'
            nts_data = fun.csv_to_dfr(nts_fldr)

            # pre-processing
            nts_data = self._preprocess(nts_data)
            # self._work_from_home(nts_data)
            # self._trip_rates_production(nts_data, self.tfn_mode, None, 'surveyyear', False)
            # self._mts_hbase(nts_data, self.tfn_mode, 'tfn_at', ['hh_type', 'tt'], 2018)  # 'tfn_at', self.tfn_ttype
            # self._trip_rates_nhbase(nts_data, self.tfn_mode, 'tfn_at', None)
            # self._mts_nhbase(nts_data, self.tfn_mode, 'tfn_at', None)
            self._tld_ml_prep(nts_data, ['mode', 'purpose', 'direction', 'period'], 'trav_dist')
            self._trip_length(nts_data, self.tfn_mode, 'gor', None, True)
            self._tour_proportion(nts_data, self.tfn_mode, 'tfn_at', None)
            self._veh_occupancy(nts_data, [3, 4], 'gor', None, [0, 5, 10, 25, 50, 100, 200, 1999])
            self._trip_rates_attraction(nts_data, self.tfn_mode, 'tfn_at', ['sic', 'soc'])
            self._mts_attraction(nts_data,  self.tfn_mode, 'tfn_at', None)
            self._activity(nts_data, None, 'gor')

        else:
            fun.log_stderr(f' .. skipped!')

    @staticmethod
    def _preprocess(dfr: pd.DataFrame) -> pd.DataFrame:
        fun.log_stderr('\nPre-processing')
        # sort data
        col_sort = ['surveyyear', 'householdid', 'individualid', 'dayid', 'tripid']
        fun.log_stderr(f' .. sort data by {col_sort}')
        dfr.sort_values(col_sort, ascending=True, ignore_index=True, inplace=True)
        return dfr

    def _trip_length(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                     seg_incl: Union[List, str, None] = None, agg_band: Union[bool, List] = False):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # agg_band: False, True, or a list of user-defined distance bands
        # TODO: need further disaggregation by 5 rucs & by 11 gors -> 40 TLDs,
        # TODO: also add in sample size threshold to address low sample issue
        # TODO: different distance bands by modes
        # TODO: add in adjustment to reflect NorMITs base year
        fun.log_stderr('\nNTS trip-length distribution')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose', 'direction', 'period', 'trav_dist']
        col_used = [lev_orig] + col_used if geo_incl is not None else col_used
        dfr = dfr[col_used + seg_incl + ['trips']].copy()
        dfr['trav_dist_total'] = dfr['trips'].mul(dfr['trav_dist'])
        if agg_band or isinstance(agg_band, List):
            col_dist = dfr['trav_dist'].values
            rng_dist = np.array(agg_band) if isinstance(agg_band, List) else fun.dist_band(col_dist.max())
            dfr['dist_band'] = pd.cut(col_dist, rng_dist, right=False, labels=rng_dist[1:])
            dfr['trav_dist'], col_used = 999, col_used + ['dist_band']
        dfr = dfr.groupby(col_used + seg_incl, observed=True)[['trips', 'trav_dist_total']].sum().reset_index()
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_tlds}'
        mode = self.tfn_mode if mode is None else mode
        dfr['purpose'] = self.luk.nhb_renumber(dfr, col_type)
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl)
        dfr = fun.dfr_filter_mode(dfr, mode)
        dfr = dfr.rename(columns={lev_orig: lev_prod}) if geo_incl is not None else dfr
        fun.dfr_to_csv(dfr, out_fldr, 'trip_length_distribution', False)

    def _trip_rates_production(self, dfr: pd.DataFrame, mode: Union[List, None] = None,
                               geo_incl: Union[str, None] = None, seg_incl: Union[List, str, None] = None,
                               inc_dist: bool = True):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        fun.log_stderr('\nNTS trip rates (productions)')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        lev_prod, col_used = lev_2col['h'], ['mode', 'purpose', 'direction', 'period']
        col_used = [lev_prod] + col_used if geo_incl is not None else col_used
        col_used = col_used + ['trav_dist'] if inc_dist else col_used
        dfr = dfr[col_used + seg_incl + ['individualid', 'w2', 'trips']].copy()
        # calculate trip-rates
        col_grby = [lev_prod] + seg_incl if geo_incl is not None else seg_incl
        pop = dfr.groupby(col_grby + ['individualid'])['w2'].mean().fillna(0).reset_index()
        pop = pop.groupby(col_grby)['w2'].sum().reset_index()
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum().reset_index()
        dfr = pd.merge(dfr, pop, how='left', on=col_grby, suffixes=('', ''))
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_prod}'
        mode = self.tfn_mode if mode is None else mode
        dfr['purpose'] = self.luk.nhb_renumber(dfr, col_type)
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl)
        dfr = fun.dfr_filter_mode(dfr, mode)
        fun.dfr_to_csv(dfr, out_fldr, 'trip_rates_productions', False)

    def _trip_rates_attraction(self, dfr: pd.DataFrame, mode: Union[List, None] = None,
                               geo_incl: Union[str, None] = None, seg_incl: Union[List, str, None] = None):
        # trip attraction rates Alpha1 (by purpose, land-use type, soc, and area_type)
        fun.log_stderr('\nNTS trip rates (attractions)')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        lev_prod, lev_dest = lev_2col['h'], lev_2col['d']
        dfr = dfr[seg_incl + [lev_dest, 'mode', 'purpose', 'direction', 'individualid', 'w2', 'trips']].copy()
        mode = self.tfn_mode if mode is None else mode
        dfr = dfr.loc[dfr['direction'].isin(['hb_fr', 'nhb'])]
        # trip-rates (based on number of employment)
        col_grby = seg_incl + [lev_dest, 'direction']
        emp = dfr.groupby(col_grby + ['individualid', 'purpose'])['w2'].mean().fillna(0).reset_index()
        emp = emp.groupby(col_grby + ['purpose'])['w2'].sum().reset_index()
        emp = emp.loc[emp['purpose'] == 1].drop(columns='purpose')
        dfr = fun.dfr_filter_mode(dfr, mode)
        ppx_list = dfr['purpose'].unique()
        dfr = dfr.groupby(col_grby + ['purpose'])[['trips']].sum().reset_index()
        dfr = fun.dfr_complete(dfr, col_grby + ['purpose'], 'purpose').reset_index()
        dfr = pd.pivot_table(dfr, values='trips', index=col_grby, columns='purpose').fillna(0)
        dfr.rename(columns={pp: f'p{pp}' for pp in ppx_list}, inplace=True)
        dfr = pd.merge(emp, dfr, how='left', on=col_grby, suffixes=('', ''))
        dfr = fun.dfr_filter_zero(dfr, col_grby)
        dfr = fun.dfr_complete(dfr, col_grby, lev_dest)
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_attr}'
        fun.dfr_to_csv(dfr.sort_index(), out_fldr, 'trip_rates_attractions', True)

    def _tour_proportion(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                         seg_incl: Union[List, str, None] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # TODO: further disaggregated by area type, o.mode, o.purpose, o.time, r.mode, r.purpose. r.time
        # TODO: add in adjustment to reflect NorMITs base year?
        fun.log_stderr('\nNTS tour proportions')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        lev_prod, col_used = lev_2col['h'], ['mode', 'purpose', 'period']
        col_used = [lev_prod] + col_used if geo_incl is not None else col_used
        dfr = dfr[col_used + seg_incl + ['individualid', 'direction', 'tour_group', 'trips']].copy()
        # tour proportion PHI by outward purpose, time and return purpose, time
        col_join = ['individualid', 'tour_group']
        hbf = dfr.loc[dfr['direction'] == 'hb_fr'].reset_index(drop=True).drop(columns='direction')
        hbt = dfr.loc[dfr['direction'] == 'hb_to'].reset_index(drop=True).drop(columns='direction')
        dfr = pd.merge(hbf, hbt, how='left', on=col_join, suffixes=('', '_return'))
        col_return = [f'{col}_return' for col in col_used if col != lev_prod]
        dfr = dfr.groupby(col_used + seg_incl + col_return)[['trips', 'trips_return']].sum().reset_index()
        # dfr['trips'] = dfr[['trips', 'trips_return']].mean(axis=1)
        # dfr = dfr.groupby(col_used + seg_incl + ['period_return'])[['trips']].sum().reset_index()
        # write tour_prop output
        fun.log_stderr(f' .. write output - tour proportion')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_prod}\{self.cfg.fld_hbase}\\{self.cfg.fld_phis}'
        mode = self.tfn_mode if mode is None else mode
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl + col_return)
        dfr = fun.dfr_filter_mode(dfr, mode)
        fun.dfr_to_csv(dfr, out_fldr, 'tour_proportions', False)
        # write phi factors
        fun.log_stderr(f' .. write output - phi factor')
        col_used = [col for col in col_used + seg_incl if col != 'mode']
        for mdx in dfr['mode'].unique():
            out = dfr.loc[dfr['mode'] == mdx].reset_index(drop=True).drop(columns='mode')
            out = fun.dfr_filter_zero(out, col_used + col_return)
            out['phi'] = out.groupby(col_used)['trips'].transform('sum')
            out['phi'] = out['trips'].div(out['phi']).fillna(0)
            out = fun.dfr_complete(out, col_used + col_return, 'period_return')
            fun.dfr_to_csv(out, out_fldr, f'phi_factors_m{mdx}', True)

    def _veh_occupancy(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                       seg_incl: Union[List, str, None] = None, agg_band: Union[bool, List] = True):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # agg_band: False, True, or a list of user-defined distance bands
        # TODO: add in GOR as area type for trip starting
        # TODO: aggregation to address low sample issue (e.g. combine with other purposes, or dist. bands)
        # TODO: add in base year for adjustment from NTS average to base year model
        fun.log_stderr('\nNTS occupancies')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        lev_orig, lev_dest = lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose', 'direction', 'period', 'occupant', 'trav_dist']
        col_used = [lev_orig, lev_dest] + col_used if geo_incl is not None else col_used
        if agg_band or isinstance(agg_band, List):
            col_dist = dfr['trav_dist'].values
            rng_dist = np.array(agg_band) if isinstance(agg_band, List) else fun.dist_band(col_dist.max())
            dfr['dist_band'] = pd.cut(col_dist, rng_dist, right=False, labels=rng_dist[1:])
            dfr['trav_dist'], col_used = 999, col_used + ['dist_band']
        dfr = dfr[col_used + seg_incl + ['trips']].copy()
        dfr = dfr.groupby(col_used + seg_incl, observed=True)[['trips']].sum(col_type).reset_index()
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_occs}'
        mode = self.tfn_mode if mode is None else mode
        dfr['purpose'] = self.luk.nhb_renumber(dfr, col_type)
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl)
        dfr = fun.dfr_filter_mode(dfr, mode)
        fun.dfr_to_csv(dfr, out_fldr, 'vehicle_occupancy', False)

    def _activity(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                  seg_incl: Union[List, str, None] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # TODO: will think about how to address low sample size issue with distribution data
        fun.log_stderr('\nNTS activities')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['surveyyear', 'individualid', 'tripid', 'tour_group', 'mode',
                    'purpose', 'direction', 'period', 'trav_dist']
        col_used = [lev_prod, lev_orig, lev_dest] + col_used if geo_incl is not None else col_used
        dfr = dfr[col_used + seg_incl + ['w2', 'trips']].copy()
        dfr = dfr.loc[(~dfr['purpose'].isin([0])) & (~dfr['direction'].isin(['0', 0]))].reset_index(drop=True)
        dfr.rename(columns={'w2': 'freq', 'trips': 'trip'}, inplace=True)
        col_grby = ['individualid', 'tour_group']
        dfr['freq'] = np.where(dfr['direction'] == 'hb_to', 0, dfr['purpose'])
        dfr['freq'] = dfr['freq'].astype(str)
        dfr['tour_id'] = '0_' + dfr.groupby(col_grby)['freq'].transform(lambda x: '_'.join(x))
        dfr['tour_id'] = dfr['tour_id'].apply(lambda x: x[:-1] + '0' if x[-1] != '0' else x)
        dfr['freq'] = dfr['trip'].div(dfr.groupby(col_grby)['tour_id'].transform('count')).fillna(0)
        mode = self.tfn_mode if mode is None else mode
        dfr = fun.dfr_filter_zero(dfr, ['mode', 'period'])
        dfr = fun.dfr_filter_mode(dfr, mode)
        # write activity
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_tour}'
        act = dfr.groupby([lev_prod, 'tour_id'])[['freq', 'trip']].sum()
        fun.dfr_to_csv(act, out_fldr, f'activity_{"all" if geo_incl is None else geo_incl}')
        # write distribution
        col_grby = ['mode', 'purpose', 'direction', 'period', 'trav_dist']
        col_grby = [lev_orig, lev_dest] + col_grby if geo_incl is not None else col_grby
        dfr = dfr.groupby(col_grby)[['freq', 'trip']].sum()
        dfr = dfr.sort_index().reset_index()
        fun.dfr_to_csv(dfr, out_fldr, f'distribution_{"all" if geo_incl is None else geo_incl}', False)

    def _mts_hbase(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                   seg_incl: Union[List, str, None] = None, yrx_base: Union[int, None] = None):
        # hb trip-rates BETA_hsr (by purpose, traveller_type, area_type)
        # hb mode-time-split RHO (by purpose, area_type, traveller_type, mode, time)
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # TODO: revisit aggregation: level 1 - by hh_type, then level 2 - at1 + at2, at7 + at8, where low sample size
        # TODO: adjust to reflect NorMITs base, need checking
        fun.log_stderr('\nNTS mode-time split - hb')
        fun.log_stderr(f' .. process data')
        lev_prod, col_type = self.luk.lev_to_name(geo_incl)['h'], self.luk.nts_dtype
        mode = self.tfn_mode if mode is None else mode
        dfr = dfr.loc[(dfr['direction'] == 'hb_fr') & (dfr['w1'] == 1)].copy()
        out_spec = ['purpose', 'mode', 'period']
        if yrx_base is not None:
            dfr_year = dfr[out_spec + ['surveyyear', 'trips']].copy()
            dfr_year = fun.dfr_filter_zero(dfr_year, out_spec)
            dfr_year = fun.dfr_filter_mode(dfr_year, mode)
            dfr_aves = dfr_year.groupby(out_spec)['trips'].sum().reset_index()
            dfr_year = dfr_year.loc[dfr_year['surveyyear'] == yrx_base].groupby(out_spec)['trips'].sum().reset_index()
            dfr_aves['rho'] = fun.agg_fill(dfr_aves, 'purpose', ['mode', 'period'], 'trips', 300)
            dfr_year['rho'] = fun.agg_fill(dfr_year, 'purpose', ['mode', 'period'], 'trips', 300)
            dfr_year = pd.merge(dfr_year, dfr_aves, how='outer', on=out_spec, suffixes=('', '_all')).fillna(0)
            dfr_year['factor'] = dfr_year['rho'].div(dfr_year['rho_all']).fillna(1)
            dfr_year.loc[dfr_year['factor'] == 0, 'factor'] = 1
            dfr_year.drop(columns=['trips', 'rho', 'trips_all', 'rho_all'], inplace=True)
        else:
            dfr_year = pd.DataFrame([])
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        seg_incl = ['purpose', lev_prod] + seg_incl if geo_incl is not None else ['purpose'] + seg_incl
        col_used = seg_incl + [col for col in self.tfn_ttype if col not in seg_incl]
        dfr = dfr[col_used + ['mode', 'direction', 'period', 'individualid', 'w1', 'w2', 'trips']].copy()
        # mode-time-split
        col_grby = col_used + ['mode', 'period']
        dfr = dfr.groupby(col_grby)[['trips']].sum().reset_index()
        dfr = fun.dfr_filter_zero(dfr, col_grby)
        dfr = fun.dfr_filter_mode(dfr, mode)
        if 'tt' in seg_incl:
            dfr = dfr.drop(columns='tt')
            dfr = pd.merge(dfr, self.tvt_list, how='outer', on=self.tfn_ttype).fillna(0)
        dfr[col_grby] = dfr[col_grby].astype(int)
        col_used = ['mode', 'purpose', 'period'] + ([lev_prod] if lev_prod is not None else [])
        for col in col_used:
            dfr.loc[dfr[col] == 0, col] = 1
            dfr = fun.dfr_complete(dfr, col_grby, col).reset_index()

        # aggregate split
        dfr = dfr.groupby(seg_incl + ['mode', 'period'])[['trips']].sum().reset_index()
        dfr['split'] = fun.agg_fill(dfr, seg_incl, ['mode', 'period'], 'trips', 300)

        if yrx_base is not None:
            dfr = pd.merge(dfr, dfr_year, how='left', on=out_spec).fillna(1)
            dfr['split'] = dfr['split'].mul(dfr['factor']).fillna(dfr['split'])
            dfr.drop(columns='factor', inplace=True)
            dfr['split'] = fun.agg_fill(dfr, seg_incl, ['mode', 'period'], 'split')

        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_prod}\{self.cfg.fld_hbase}\\{self.cfg.fld_split}'
        fun.dfr_to_csv(dfr, out_fldr, 'mode_time_split_hb', False)

    def _trip_rates_nhbase(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                           seg_incl: Union[List, str, None] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # TODO: address low sample size (from from_home trips) through aggregation
        # TODO: add in adjustment to reflect post-COVID
        fun.log_stderr('\nNTS trip rates - nhb')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose']
        seg_incl = col_used + ([lev_dest] if geo_incl is not None else []) + seg_incl
        col_used = seg_incl + [col for col in self.tfn_ttype if col not in seg_incl]
        dfr = dfr[col_used + ['direction', 'period', 'tour_group', 'individualid', 'trips']].copy()
        mode = self.tfn_mode if mode is None else mode
        # nhb trip-rates (exclude hb_to trips)
        hbf = dfr.loc[dfr['direction'].isin(['hb_fr'])].reset_index(drop=True)
        nhb = dfr.loc[dfr['direction'].isin(['nhb'])].reset_index(drop=True)
        col_join = ['individualid', 'tour_group']
        nhb = pd.merge(nhb, hbf, how='left', on=col_join, suffixes=('', '_hb'))
        col_u4hb = [f'{col}_hb' if col in ['mode', 'purpose'] else col for col in seg_incl]
        hbf = hbf.groupby(seg_incl)[['trips']].sum().reset_index()
        hbf.rename(columns=dict(zip(seg_incl, col_u4hb)), inplace=True)
        col_join = seg_incl + [col for col in col_u4hb if col not in seg_incl]
        nhb = nhb.groupby(col_join)[['trips']].sum().reset_index()
        nhb = pd.merge(nhb, hbf, how='left', on=col_u4hb, suffixes=('', '_hb'))
        nhb = fun.dfr_filter_zero(nhb, col_join)
        nhb['gamma'] = nhb['trips'].div(nhb['trips_hb']).fillna(0)
        nhb = fun.dfr_filter_mode(nhb, mode, 'mode_hb')
        nhb = fun.dfr_filter_mode(nhb, mode)
        nhb = fun.dfr_complete(nhb, col_join, 'mode_hb').reset_index()
        # aggregate split
        nhb = nhb.rename(columns={lev_dest: lev_prod}) if geo_incl is not None else nhb
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_prod}\{self.cfg.fld_nhbase}'
        fun.dfr_to_csv(nhb.sort_index(), out_fldr, 'trip_rates_nhb', False)

    def _mts_nhbase(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                    seg_incl: Union[List, str, None] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # TODO: address low sample size (24hr sample) through aggregation by area type
        # TODO: add in adjustment to reflect post-COVID
        fun.log_stderr('\nNTS mode-time split - nhb')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose']
        seg_incl = col_used + ([lev_orig] if geo_incl is not None else []) + seg_incl
        col_used = seg_incl + [col for col in self.tfn_ttype if col not in seg_incl]
        dfr = dfr[col_used + ['direction', 'period', 'individualid', 'trips']].copy()
        mode = self.tfn_mode if mode is None else mode
        dfr = dfr.loc[dfr['direction'].isin(['nhb'])].reset_index(drop=True)
        # mode-time-split
        col_grby = seg_incl + ['period']
        dfr = dfr.groupby(col_grby)[['trips']].sum().reset_index()
        dfr = fun.dfr_filter_zero(dfr, col_grby)
        dfr = fun.dfr_filter_mode(dfr, mode)
        if 'tt' in seg_incl:
            col_grby = col_grby + ['tt']
            dfr.drop(columns='tt', inplace=True)
            dfr = pd.merge(dfr, self.tvt_list, how='outer', on=self.tfn_ttype).fillna(0)
        dfr[col_grby] = dfr[col_grby].astype(int)
        col_used = ['mode', 'purpose', 'period'] + ([lev_orig] if lev_orig is not None else [])
        for col in col_used:
            dfr.loc[dfr[col] == 0, col] = 1
            dfr = fun.dfr_complete(dfr, col_grby, col).reset_index()

        # aggregate split
        dfr['rho'] = fun.agg_fill(dfr, seg_incl, 'period', 'trips', 50)
        dfr = dfr.rename(columns={lev_orig: lev_prod}) if geo_incl is not None else dfr
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_prod}\{self.cfg.fld_nhbase}'
        fun.dfr_to_csv(dfr.sort_index(), out_fldr, 'mode_time_split_nhb', False)

    def _mts_attraction(self, dfr: pd.DataFrame, mode: Union[List, None] = None, geo_incl: Union[str, None] = None,
                        seg_incl: Union[List, str, None] = None):
        # trip attraction mode-time split ALPHA2 (area_type, by purpose, mode, time)
        # TODO: address low sample size (from from_home trips) through aggregation
        # TODO: add in adjustment to reflect post-COVID
        # TODO: perhaps produce HB MTS for now, and we can think about NHB MTS later
        fun.log_stderr('\nNTS mode-time split (attractions)')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self.luk.lev_to_name(geo_incl), self.luk.nts_dtype
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        lev_prod, lev_dest = lev_2col['h'], lev_2col['d']
        seg_incl = [lev_dest] + seg_incl if geo_incl is not None else seg_incl
        dfr = dfr[seg_incl + ['mode', 'purpose', 'direction', 'period', 'trips']].copy()
        mode = self.tfn_mode if mode is None else mode
        dfr = dfr.loc[dfr['direction'].isin(['hb_fr', 'nhb'])]

        # aggregate split
        dfr = dfr.groupby(seg_incl + ['mode', 'period'])[['trips']].sum().reset_index()
        dfr['alpha'] = fun.agg_fill(dfr, seg_incl, ['mode', 'period'], 'trips', 300)

        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = fr'{self.cfg.dir_output}\{self.cfg.fld_attr}'
        fun.dfr_to_csv(dfr, out_fldr, 'mode_time_split_hb', False)

    def _work_from_home(self, dfr: pd.DataFrame, geo_incl: Union[str, None] = None,
                        seg_incl: Union[List, str, None] = None):

        # method 1 - from individual table
        # out = dfr.groupby(['individualid', 'surveyyear', 'wfh', 'w2'] + self.tfn_ttype)['trips'].sum().reset_index()
        # method 2 - from trip table (weekday + fte only)
        out = (dfr['purpose'] == 1) & dfr['direction'].isin(['hb_fr']) & (dfr['period'].isin([1, 2, 3, 4]))
        out, col_grby = dfr.loc[out].copy(), ['individualid', 'surveyyear', 'dayid', 'w2']
        out = out.groupby(col_grby + self.tfn_ttype)['tripid'].count().reset_index()
        out = out.groupby(['individualid', 'surveyyear', 'w2'] + self.tfn_ttype)['dayid'].count().reset_index()
        out = out.rename(columns={'dayid': 'wfh'})
        out['wfh'] = 5 - out['wfh']

        # write output
        out = out.groupby(self.tfn_ttype + ['surveyyear', 'wfh'])[['w2']].sum()
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_other}'
        fun.dfr_to_csv(out, out_fldr, 'work_fr_home.csv', True)

    def _tld_ml_prep(self, cb, seg_cols, dis_col, threshold = 300):
        update = {}
        for i, j in self.luk.dct_to_specs(self.luk.set_01id).items():
            for k in j:
                if isinstance(k, str):
                    update[k.upper()] = i
                else:
                    update[k] = i
        update = pd.DataFrame.from_dict(update, orient='index', columns=['at'])
        modes = {}
        for mode in cb['mode'].unique():
            inner = cb[cb['mode'] == mode]
            dists = inner[dis_col].values
            bands = fun.dist_band(dists.max())
            inner['band'] = pd.cut(dists, bands)
            modes[mode] = inner
            inner_df = pd.merge(inner, update, left_on='settlement2011ew_b01id', right_index=True)
            inner_df.drop(columns=['band']).to_hdf(r"E:\NTS\analysis\int\data.h5", key=str(mode), mode='a')
            inner_df['count'] = 1
            seg_cols += ['at', 'triporiggor_b02id']
            big_count = inner_df.groupby(seg_cols)[['count','trips']].sum()
            small_count = inner_df.groupby(seg_cols + ['band'], observed=False)['trips'].sum().reset_index(level='band')
            big_count.columns = ['sample size', 'agg trips']
            joined = small_count.join(big_count, how='inner')
            joined['norm_trips'] = joined['trips'] / joined['agg trips']
            joined['band start'] = pd.IntervalIndex(joined['band']).left
            joined.set_index('band start', append=True, inplace=True)
            joined.reset_index(level='mode', inplace=True)
            joined.drop(['mode','band'], axis=1, inplace=True)
            joined.drop(0, inplace=True)
            joined.drop([0,5,6], level='period', inplace=True)
            enc = OneHotEncoder()
            enc.fit(joined.index.to_frame())
            cols = []
            iterator = joined.index.names
            for i, cat in enumerate(enc.categories_):
                for j in cat:
                    cols.append(f"{iterator[i]}_{j}")
            index = enc.transform(joined.index.to_frame()).toarray()
            encoded_index = pd.MultiIndex.from_frame(pd.DataFrame(index, columns=cols))
            ready_data = pd.DataFrame(joined[['norm_trips', 'sample size']].values, index=encoded_index, columns = ['norm_trips', 'sample size'])
            training = ready_data.loc[ready_data['sample size'] > threshold, 'norm_trips']
            to_predict = ready_data.loc[ready_data['sample size'] <= threshold, 'norm_trips']
            X = training.index.to_frame()
            y = training.values
            X_train, X_test, y_train, y_test = train_test_split(X, y)
            regr = RandomForestRegressor()
            score = cross_validate(regr, X, y, cv=5, scoring=("r2"))
            regr.fit(X_train, y_train)
            test = regr.predict(X_test)
            pred = regr.predict(to_predict.index.to_frame())

        return ready_data

def _rfr_tld(data):
    regr = RandomForestRegressor()
    regr.fit(data.index, data.values)


