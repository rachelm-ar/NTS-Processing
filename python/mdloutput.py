import mdlconfig
import mdlfunction as fun
from typing import Union, List, Dict
import mdllookup as luk
import pandas as pd
import numpy as np


class Output:
    """ produce outputs from NTS classified build, including:
        1. hb trip-rates (BETA_hsr) by hb purpose (h), traveller_type (s), area_type (r)
        2. hb mode-time-split (RHO_mdhsr) by mode (m), time (d), h, s, r
        3. nhb trip weights (GAMMA_nmhm) by nhb purpose (n), mode (m), to hb attraction purpose (h) and mode (m)
        4. nhb time-split (RHO_dmnr) by time(d), mode (m), nhb purpose (n), area_type (r)
        5. hb tour proportions (PHI_hdhd) by outward purpose (p), time (d), return purpose (p), time (d)
        6. attraction trip-rates (ALPHA1_per) by purpose (p), land-use (e), area_type (r)
        7. attraction mode split (ALPHA2_pmkr) by purpose (p), mode (m), J2W type (k) area_type (r)
        8. trip-length distribution
        9. trip activity
    """

    def __init__(self, nts_fldr: str, cb_version: Union[str, int], over_write: bool = True):
        fun.log_stderr('\n***** NTS TRIP OUTPUTS *****')
        # read config
        self.cfg = mdlconfig.Config(nts_fldr)
        self.luk = luk.Lookup(self.cfg.nts_dtype)
        self.tfn_ttype, self.tfn_atype = self.cfg.tfn_ttype, self.cfg.tfn_atype
        self.cb_version, self.tfn_mode = cb_version, self.cfg.tfn_modes
        self.tvt_list = self.luk.tt_to_dfr(self.tfn_ttype, self.cfg.def_ttype)

        # read in cb data
        if over_write:
            fun.log_stderr('Import cb data')
            nts_fldr = f'{self.cfg.fld_cbuild}\\{self.cfg.csv_cbuild}_v{self.cb_version}.csv'
            nts_data = fun.csv_to_dfr(nts_fldr)

            # pre-processing
            nts_data = self._preprocess(nts_data)
            # produce data for specific mode and area
            # geo_incl: either gor, county, or tfn_at, geo_list: list of geo_area to extract
            self._mts_hbase(nts_data, self.tfn_mode, 'tfn_at', 'hh_type')  # 'tfn_at', self.tfn_ttype
            self._trip_rates_nhbase(nts_data, self.tfn_mode, 'tfn_at', 'hh_type')
            self._mts_nhbase(nts_data, self.tfn_mode, 'tfn_at', 'hh_type')
            self._trip_length(nts_data, self.tfn_mode, 'tfn_at', None, None)
            self._tour_proportion(nts_data, self.tfn_mode, None, None, None)
            self._occs_vehicle(nts_data, self.tfn_mode, 'gor', None)
            self._activity(nts_data, None, 'gor')
            self._trip_rates_all(nts_data, self.tfn_mode, 'tfn_at', None, None, False)
        else:
            fun.log_stderr(f' .. skipped!')

    def _preprocess(self, dfr: pd.DataFrame, agg_cols: bool = False) -> pd.DataFrame:
        fun.log_stderr('\nPre-processing')
        # aggregate mode, purpose, tfn_at
        if agg_cols:
            fun.log_stderr(f' .. aggregate mode, purpose, and tfn_at')
            col_type = self.cfg.nts_dtype
            dfr = dfr.set_index('purpose').rename(index=self._agg_purpose(col_type)).reset_index()
            dfr = dfr.set_index('mode').rename(index=self._agg_mode(col_type)).reset_index()
            atx_dict = self._agg_atype(col_type, self.tfn_atype)
            for col in ['tfn_at', 'tfn_at_o', 'tfn_at_d']:
                dfr = dfr.set_index(col).rename(index=atx_dict).reset_index()
        # sort data
        col_sort = ['surveyyear', 'householdid', 'individualid', 'dayid', 'tripid']
        fun.log_stderr(f' .. sort data by {col_sort}')
        dfr.sort_values(col_sort, ascending=True, ignore_index=True, inplace=True)
        return dfr

    def _trip_length(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                     geo_list: Union[List, Dict] = None, seg_incl: Union[List, str] = None,
                     agg_band: bool = False):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # geo_list: list of geo_area to be included
        fun.log_stderr('\nNTS trip-length distribution')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose', 'direction', 'period', 'trav_dist']
        col_used = [lev_orig] + col_used if geo_incl is not None else col_used
        dfr = dfr[col_used + seg_incl + ['trips']].copy()
        if agg_band:
            col_dist = dfr['trav_dist'].values
            rng_dist = fun.dist_band(col_dist.max())
            dfr['dist_band'] = np.digitize(col_dist, rng_dist, right=False)
            col_used = [col for col in col_used if col != 'trav_dist'] + ['dist_band']
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum().reset_index()
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_tlds}'
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        if geo_list is not None and geo_incl is not None:
            dct = fun.list_to_dict(geo_list)
            msk_orig = (dfr[lev_orig].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_orig].isin(list(dct)))
            msk_dest = (dfr[lev_dest].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_dest].isin(list(dct)))
            dfr = dfr.loc[msk_orig | msk_dest]
            if isinstance(geo_list, dict):
                dfr = dfr.set_index([lev_orig, lev_dest]).rename(index=dct).reset_index()
        dfr['trav_dist_total'] = dfr['trips'].mul(dfr['trav_dist'])
        dfr['purpose'] = self._renumber_nhb(dfr, col_type)
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl)
        dfr = fun.dfr_filter_mode(dfr, mode)
        dfr = dfr.rename(columns={lev_orig: lev_prod}) if geo_incl is not None else dfr
        fun.dfr_to_csv(dfr, out_fldr, 'trip_length_distribution', False)

    def _trip_rates_all(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                        geo_list: Union[Dict, List] = None, seg_incl: Union[List, str] = None,
                        inc_dist: bool = True):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # geo_list: list of geo_area to be included
        fun.log_stderr('\nNTS trip rates (all)')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
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
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_notem}'
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        if geo_list is not None and geo_incl is not None:
            dct = fun.list_to_dict(geo_list)
            dfr = dfr.loc[dfr[lev_prod].str.lower().isin(list(dct))]
            dfr[lev_prod] = dfr[lev_prod].str.lower()
            if isinstance(geo_list, dict):
                dfr = dfr.set_index(lev_2col['h']).rename(index=dct).reset_index()
        dfr['purpose'] = self._renumber_nhb(dfr, col_type)
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl)
        dfr = fun.dfr_filter_mode(dfr, mode)
        fun.dfr_to_csv(dfr, out_fldr, 'trip_rates_all', False)

    def _tour_proportion(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                         geo_list: Union[List, Dict] = None, seg_incl: Union[List, str] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # geo_list: list of geo_area to be included
        fun.log_stderr('\nNTS tour proportions')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        lev_prod, col_used = lev_2col['h'], ['mode', 'purpose', 'period']
        col_used = [lev_prod] + col_used if geo_incl is not None else col_used
        dfr = dfr[col_used + seg_incl + ['individualid', 'direction', 'tour_group', 'trips']].copy()
        # tour proportion PHI by outward purpose, time and return purpose, time
        col_join = ['individualid', 'tour_group']
        hbf = dfr.loc[dfr['direction'] == 'hb_fr'].reset_index(drop=True).drop(columns='direction')
        hbt = dfr.loc[dfr['direction'] == 'hb_to'].reset_index(drop=True).drop(columns='direction')
        dfr = pd.merge(hbf, hbt, how='left', on=col_join, suffixes=('', '_return'))
        col_return = [f'{col}_return' for col in col_used]
        dfr = dfr.groupby(col_used + seg_incl + col_return)[['trips', 'trips_return']].sum().reset_index()
        # dfr['trips'] = dfr[['trips', 'trips_return']].mean(axis=1)
        # dfr = dfr.groupby(col_used + seg_incl + ['period_return'])[['trips']].sum().reset_index()
        # write tour_prop output
        fun.log_stderr(f' .. write output - tour proportion')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_notem}'
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        if geo_list is not None and geo_incl is not None:
            dct = fun.list_to_dict(geo_list)
            mask = (dfr[lev_prod].str.lower().isin(list(dct)) if col_type is str
                    else dfr[lev_prod].isin(list(dct)))
            dfr = dfr.loc[mask].reset_index(drop=True)
            if isinstance(geo_list, dict):
                dfr = dfr.set_index(lev_prod).rename(index=dct).reset_index()
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl + col_return)
        dfr = fun.dfr_filter_mode(dfr, mode)
        fun.dfr_to_csv(dfr, out_fldr, 'tour_proportions', False)
        # write phi factors
        fun.log_stderr(f' .. write output - phi factor')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_hbase}\\{self.cfg.fld_phis}'
        col_used = [col for col in col_used + seg_incl if col != 'mode']
        for mdx in dfr['mode'].unique():
            out = dfr.loc[dfr['mode'] == mdx].reset_index(drop=True).drop(columns='mode')
            out = fun.dfr_filter_zero(out, col_used + col_return)
            out['phi_factor'] = out.groupby(col_used)['trips'].transform('sum')
            out['phi_factor'] = out['trips'].div(out['phi_factor']).fillna(0)
            out = fun.dfr_complete(out, col_used + col_return, 'period_return')
            fun.dfr_to_csv(out, out_fldr, f'phi_factors_m{mdx}', True)

    def _occs_vehicle(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                      geo_list: Union[List, Dict] = None, seg_incl: Union[List, str] = None,
                      inc_dist: bool = True):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        # geo_list: list of geo_area to be included
        fun.log_stderr('\nNTS occupancies')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
        lev_orig, lev_dest = lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose', 'direction', 'period', 'occupant']
        col_used = [lev_orig, lev_dest] + col_used if geo_incl is not None else col_used
        col_used = col_used + ['trav_dist'] if inc_dist else col_used
        dfr = dfr[col_used + seg_incl + ['trips']].copy()
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum(col_type).reset_index()
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_occs}'
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        if geo_list is not None and geo_incl is not None:
            dct = fun.list_to_dict(geo_list)
            msk_orig = (dfr[lev_orig].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_orig].isin(list(dct)))
            msk_dest = (dfr[lev_dest].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_dest].isin(list(dct)))
            dfr = dfr.loc[msk_orig | msk_dest]
            if isinstance(geo_list, dict):
                dfr = dfr.set_index([lev_orig, lev_dest]).rename(index=dct).reset_index()
        dfr['purpose'] = self._renumber_nhb(dfr, col_type)
        dfr = fun.dfr_filter_zero(dfr, col_used + seg_incl)
        dfr = fun.dfr_filter_mode(dfr, mode)
        fun.dfr_to_csv(dfr, out_fldr, 'vehicle_occupancy', False)

    def _activity(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                  seg_incl: Union[List, str] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        fun.log_stderr('\nNTS activities')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['surveyyear', 'individualid', 'tripid', 'tour_group', 'mode',
                    'purpose', 'direction', 'period', 'trav_dist']
        col_used = [lev_prod, lev_orig, lev_dest] + col_used if geo_incl is not None else col_used
        dfr = dfr[col_used + seg_incl + ['w2', 'trips']].copy()
        dfr = dfr.loc[(dfr['purpose'].isin([0])) & (~dfr['direction'].isin(['0', 0]))].reset_index(drop=True)
        dfr.rename(columns={'w2': 'freq', 'trips': 'trip'}, inplace=True)
        col_grby = ['individualid', 'tour_group']
        dfr['freq'] = np.where(dfr['direction'] == 'hb_to', 0, dfr['purpose'])
        dfr['freq'] = dfr['freq'].astype(str)
        dfr['tour_id'] = '0_' + dfr.groupby(col_grby)['freq'].transform(lambda x: '_'.join(x))
        dfr['tour_id'] = dfr['tour_id'].apply(lambda x: x[:-1] + '0' if x[-1] != '0' else x)
        dfr['freq'] = dfr['trip'].div(dfr.groupby(col_grby)['tour_id'].transform('count')).fillna(0)
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = fun.dfr_filter_zero(dfr, ['mode', 'period'])
        dfr = fun.dfr_filter_mode(dfr, mode)
        # write activity
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_notem}'
        act = dfr.groupby([lev_prod, 'tour_id'])[['freq', 'trip']].sum()
        fun.dfr_to_csv(act, out_fldr, f'activity_{"all" if geo_incl is None else geo_incl}')
        # write distribution
        col_grby = ['mode', 'purpose', 'direction', 'period', 'trav_dist']
        col_grby = [lev_orig, lev_dest] + col_grby if geo_incl is not None else col_grby
        dfr = dfr.groupby(col_grby)[['freq', 'trip']].sum()
        dfr = dfr.sort_index().reset_index()
        fun.dfr_to_csv(dfr, out_fldr, f'distribution_{"all" if geo_incl is None else geo_incl}', False)

    def _mts_hbase(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                   seg_incl: Union[List, str] = None):
        # hb trip-rates BETA_hsr (by purpose, traveller_type, area_type)
        # aggregation: level 1 - by hh_type, then level 2 - at1 + at2, at7 + at8, where low sample size
        # hb mode-time-split RHO (by purpose, area_type, traveller_type, mode, time)
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        fun.log_stderr('\nNTS mode-time split - hb')
        fun.log_stderr(f' .. process data')
        lev_prod, col_type = self._level_to_col(geo_incl)['h'], self.luk.nts_dtype
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        seg_incl = ['purpose', lev_prod] + seg_incl if geo_incl is not None else ['purpose'] + seg_incl
        col_used = seg_incl + [col for col in self.tfn_ttype if col not in seg_incl]
        dfr = dfr[col_used + ['mode', 'direction', 'period', 'individualid', 'w1', 'w2', 'trips']].copy()
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[(dfr['direction'] == 'hb_fr') & (dfr['w1'] == 1)].copy()
        # mode-time-split
        col_grby = seg_incl + ['mode', 'period']
        dfr = dfr.groupby(col_grby)[['trips']].sum().reset_index()
        dfr = fun.dfr_filter_zero(dfr, col_grby)
        dfr = fun.dfr_filter_mode(dfr, mode)
        if 'tt' in seg_incl:
            col_grby = col_grby + ['tt']
            dfr.drop(columns='tt', inplace=True)
            dfr = pd.merge(dfr, self.tvt_list, how='outer', on=self.tfn_ttype).fillna(0)
        dfr[col_grby] = dfr[col_grby].astype(int)
        col_used = ['mode', 'purpose', 'period'] + ([lev_prod] if lev_prod is not None else [])
        for col in col_used:
            dfr.loc[dfr[col] == 0, col] = 1
            dfr = fun.dfr_complete(dfr, col_grby, col).reset_index()

        # aggregate split
        dfr['split'] = fun.agg_fill(dfr, seg_incl, ['mode', 'period'], 'trips', 300)
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_hbase}\\{self.cfg.fld_split}'
        fun.dfr_to_csv(dfr, out_fldr, 'mode_time_split_hb', False)

    def _trip_rates_nhbase(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                           seg_incl: Union[List, str] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        fun.log_stderr('\nNTS trip rates - nhb')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose']
        seg_incl = col_used + ([lev_dest] if geo_incl is not None else []) + seg_incl
        col_used = seg_incl + [col for col in self.tfn_ttype if col not in seg_incl]
        dfr = dfr[col_used + ['direction', 'period', 'tour_group', 'individualid', 'trips']].copy()
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
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
        nhb['nhb_weights'] = nhb['trips'].div(nhb['trips_hb']).fillna(0)
        nhb = fun.dfr_filter_mode(nhb, mode, 'mode_hb')
        nhb = fun.dfr_filter_mode(nhb, mode)
        nhb = fun.dfr_complete(nhb, col_join, 'mode_hb').reset_index()
        # aggregate split
        nhb = nhb.rename(columns={lev_dest: lev_prod}) if geo_incl is not None else nhb
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_nhbase}'
        fun.dfr_to_csv(nhb.sort_index(), out_fldr, 'trip_rates_nhb', False)

    def _mts_nhbase(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                    seg_incl: Union[List, str] = None):
        # geo_incl: geo_area to be included: either gor, county, tfn_at
        fun.log_stderr('\nNTS mode-time split - nhb')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = ['mode', 'purpose']
        seg_incl = col_used + ([lev_orig] if geo_incl is not None else []) + seg_incl
        col_used = seg_incl + [col for col in self.tfn_ttype if col not in seg_incl]
        dfr = dfr[col_used + ['direction', 'period', 'individualid', 'trips']].copy()
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
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
        dfr['split'] = fun.agg_fill(dfr, seg_incl, 'period', 'trips', 300)
        dfr = dfr.rename(columns={lev_orig: lev_prod}) if geo_incl is not None else dfr
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_nhbase}'
        fun.dfr_to_csv(dfr.sort_index(), out_fldr, 'mode_time_split_nhb', False)

    def _trip_rates_attraction(self, dfr: pd.DataFrame, mode: List = None, geo_incl: Union[str, None] = None,
                               seg_incl: Union[List, str] = None):
        # trip attraction rates ALPHA1 (by purpose, land-use indicator and area_type)
        fun.log_stderr('\nNTS trip rates - nhb')
        fun.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(geo_incl), self.luk.nts_dtype
        seg_incl = fun.str_to_list(seg_incl) if seg_incl is not None else []
        lev_prod, col_used = lev_2col['h'], ['purpose']
        col_used = [lev_prod] + col_used if geo_incl is not None else col_used
        dfr = dfr[col_used + seg_incl + ['mode', 'direction', 'period', 'individualid', 'w2', 'trips']].copy()
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[dfr['direction'].isin(['hb_fr'])]
        # mode-time-split
        col_grby = col_used + seg_incl + ['mode', 'period']
        mts = dfr.groupby(col_grby)[['trips']].sum().reset_index()
        mts = fun.dfr_filter_zero(mts, col_grby)
        mts = fun.dfr_filter_mode(mts, mode)
        mts['split'] = mts.groupby(col_used + seg_incl)['trips'].transform('sum')
        mts['split'] = mts['trips'].div(mts['split']).fillna(0)
        mts = fun.dfr_complete(mts, col_grby, lev_prod)
        # hb trip-rates
        col_grby = [col for col in col_used + seg_incl if col != 'purpose']
        pop = dfr.groupby(col_grby + ['individualid'])['w2'].mean().fillna(0).reset_index()
        pop = pop.groupby(col_grby)['w2'].sum().reset_index()
        dfr = fun.dfr_filter_mode(dfr, mode)
        ppx_list = dfr['purpose'].unique()
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum().reset_index()
        dfr = fun.dfr_complete(dfr, col_used + seg_incl, 'purpose').reset_index()
        dfr = pd.pivot_table(dfr, values='trips', index=col_grby, columns='purpose').fillna(0)
        dfr.rename(columns={pp: f'p{pp}' for pp in ppx_list}, inplace=True)
        dfr = pd.merge(pop, dfr, how='left', on=col_grby, suffixes=('', ''))
        dfr = fun.dfr_filter_zero(dfr, col_grby)
        dfr = fun.dfr_complete(dfr, col_grby, lev_prod)
        # write output
        fun.log_stderr(f' .. write output')
        out_fldr = f'{self.cfg.fld_output}\\{self.cfg.fld_hbase}'
        fun.dfr_to_csv(dfr.sort_index(), out_fldr, 'trip_rates_hb', True)
        fun.dfr_to_csv(mts.sort_index(), out_fldr, 'mode_time_split_hb', True)

    # supporting functions
    def _aggregate(self, dfr: pd.DataFrame, col_2agg: List):
        # aggregate function for trip-rates etc.
        """ col_2agg: appears in order of aggregation from lowest to highest
            current method aggregate by ntem_tt, hh_type, area_type, purpose, threshold trips = 300
            need to understand trip-rate similarity/difference by [hh_type, area_type, traveller_type, etc.
            to come up with an appropriate aggregation method
        """

    @staticmethod
    def _level_to_col(level: str = None) -> Dict:
        if level is not None:
            if level.lower() in ['county', 'gor']:
                home = f'hhold{level}_b02id' if level.lower() == 'gor' else f'hhold{level}_b01id'
                orig = f'triporig{level}_b02id' if level.lower() == 'gor' else f'triporig{level}_b01id'
                dest = f'tripdest{level}_b02id' if level.lower() == 'gor' else f'tripdest{level}_b01id'
            else:
                home, orig, dest = level, f'{level}_o', f'{level}_d'
        else:
            home, orig, dest = None, None, None
        return {'h': home, 'o': orig, 'd': dest}

    def _agg_purpose(self, col_type: type = int) -> Dict:
        if col_type is int:
            # 1 - hbw, 2 - hbeb, 3 - hbed, 4 - hbshop,  5- hbpb,  6- hbsoc, 7- hbvf, 8 - hbhol
            out_dict = {key: key for key in [1, 2, 3, 4, 5, 6, 7, 8]}
        else:
            out_dict = {'hbw': 1,
                        'hbeb': 2,
                        'hbed': 3,
                        'hbo': [4, 5, 6, 7, 8]
                        }
        return self.luk.val_to_key(out_dict)

    def _agg_mode(self, col_type: type = int) -> Dict:
        if col_type is int:
            out_dict = {key: key for key in [1, 2, 3, 4, 5, 6, 7, 8]}
        else:
            out_dict = {'active': [1, 2], 'car': 3, 'lgv': 4, 'bus': 5, 'rail': [6, 7], 'air': 8}
        return self.luk.val_to_key(out_dict)

    def _agg_atype(self, col_type: type = int, agg_type: str = 'ntem') -> Dict:
        if col_type is int:
            out_dict = {key: key for key in range(1, 99)}
        else:
            if agg_type == 'ruc2011':
                out_dict = {'major': 1, 'minor': 2, 'urban': 3, 'rural': [4, 5]}
            else:
                out_dict = {'inner london': 1, 'outer london': 2, 'metropolitan': 3, 'urban big': 4,
                            'urban large': 5, 'urban medium': 6, 'urban small': 7, 'rural': 8}
        return self.luk.val_to_key(out_dict)

    @staticmethod
    def _renumber_nhb(dfr: pd.DataFrame, col_type: type = int) -> np.ndarray:
        # TODO: to be updated once finalised
        return np.where(dfr['direction'] == 'xxx', (10 if col_type is int else 'n') + dfr['purpose'],
                        dfr['purpose'])
