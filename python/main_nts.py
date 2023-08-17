import pandas as pd
import numpy as np
from typing import Union, List, Dict, Any
import multiprocessing as mp
import mdlutils as uti
import os

mp.freeze_support()


class NTS:
    """ this process produces NTS data for the following purposes:
        1. trip-length distribution data for the synthetic matrix development & MND verification
        2. trip rate data for MND verification purpose
        3. tour proportions for converting PA2OD
        4. car occupancy for converting person trips to vehicle trips

        note:
        individual = w2 (for trip-rates)
        household = w3 (for car ownership)
        trips = w5 or w2*w5xhh (for trips)

        instructions:
        1. specify locations & filenames of raw NTS database, [nts_fldr] and [nts_file]
        2. specify NTS attributes to be included for analysis, [col_incl]
        3. specify location & filename of the lookup table, [sec_file]. this is used to derive lad to county lookup
        4. specify output location (where NTS processed data will be written out)
        5. specify geographical level that results are to be produced for each output type [lev_incl]
        """

    def __init__(self, nts_fldr):
        # input specs
        self.cur_path = os.path.dirname(os.path.realpath(__file__))
        self.err_indx = True
        self.m2k_fact = 1.6093
        self.num_cpus = max(os.cpu_count() - 2, 1)
        self.fun = Function()
        self.luk = Lookup(int)

        # nts-specific specs
        self.nts_fldr = nts_fldr
        self.nts_file = {'household': 'household_special_2002-2021_protect.tab',
                         'individual': 'individual_special_2002-2021_protect.tab',
                         'day': 'day_special_2002-2021_protect.tab',
                         'trip': 'trip_special_2002-2021_protect.tab',
                         }
        self.col_incl = {'household': ['SurveyYear', 'HouseholdID', 'HHoldOSLAUA_B01ID', 'HHoldCounty_B01ID',
                                       'HHoldGOR_B02ID', 'Settlement2011EW_B01ID', 'HHoldAreaType1_B01ID',
                                       'HHoldAreaType2_B01ID', 'HHoldUA1998_B01ID', 'HHoldUA2009_B01ID',
                                       'HHoldNumAdults', 'NumCarVan', 'W1', 'W2', 'W3'],
                         'individual': ['HouseholdID', 'IndividualID', 'Age_B01ID', 'Sex_B01ID', 'EcoStat_B01ID',
                                        'SIC1992_B02ID', 'SIC2007_B02ID', 'XSOC2000_B02ID', 'NSSEC_B03ID'],
                         'day': ['HouseholdID', 'IndividualID', 'DayID', 'TravelWeekday_B01ID'],
                         'trip': ['HouseholdID', 'IndividualID', 'DayID', 'TripID', 'MainMode_B11ID',
                                  'TripPurpFrom_B01ID', 'TripPurpTo_B01ID', 'TripStart_B01ID', 'TripEnd_B01ID',
                                  'TripDisIncSW', 'TripTravTime', 'TripOrigCounty_B01ID', 'TripOrigGOR_B02ID',
                                  'TripDestCounty_B01ID', 'TripDestGOR_B02ID', 'TripOrigAreaType1_B01ID',
                                  'TripDestAreaType1_B01ID', 'TripOrigAreaType2_B01ID', 'TripDestAreaType2_B01ID',
                                  'TripOrigUA1998_B01ID', 'TripOrigUA2009_B01ID', 'TripDestUA1998_B01ID',
                                  'TripDestUA2009_B01ID', 'W5', 'W5xHH', 'JJXSC']
                         }
        self.col_join = {'household': 'HouseholdID',
                         'individual': ['HouseholdID', 'IndividualID'],
                         'day': ['HouseholdID', 'IndividualID', 'DayID']
                         }
        self.out_fldr = r'D:\NTS\NoTEM\nts_data\2002_2021'

        # import & pre-process
        self._exist()
        nts_data = self._read_nts()
        nts_data = self._preprocess(nts_data)
        self._summary_ub(nts_data)

        # produce data for specific mode and area
        seg_incl = ['ruc_2011', 'aws', 'gender', 'hh_type', 'soc', 'ns',  'tt']
        self._trip_rates_hb(nts_data, None, 'gor', seg_incl)
        self._trip_length(nts_data, None, 'gor', None, 'ruc_orig')
        self._trip_rates(nts_data, None, 'gor', None, ['ruc_2011', 'surveyyear'], False)
        self._tour_props(nts_data, None, 'gor', None, 'ruc_2011')
        self._occs_vehicle(nts_data, None, 'gor', None)
        self._activity(nts_data, None, 'gor')

    def _exist(self):
        uti.log_stderr('\nCheck file existence')
        self.err_indx = uti.exist_path(self.nts_fldr, self.err_indx)
        for col in self.nts_file:
            self.err_indx = uti.exist_file(f'{self.nts_fldr}\\{self.nts_file[col]}', self.err_indx)
        os.makedirs(self.out_fldr, exist_ok=True)
        uti.log_stderr(' .. specified files OK') if self.err_indx else None
        exit() if not self.err_indx else None

    def _read_nts(self) -> pd.DataFrame:
        uti.log_stderr('\nImport NTS data')
        # import NTS data
        dfr: Union[Dict, pd.DataFrame] = {}
        pool = mp.Pool(self.num_cpus)
        for key in self.nts_file:
            csv_file = f'{self.nts_fldr}\\{self.nts_file[key]}'
            dfr[key] = pool.apply_async(self.fun.csv_to_dfr, [csv_file, self.col_incl[key]])
        pool.close()
        pool.join()
        dfr = {key: dfr[key].get() for key in dfr}
        self.err_indx = False if any([dfr[key] is False for key in dfr]) else self.err_indx
        exit() if not self.err_indx else None

        # merge data
        for key in set(dfr) - {'trip'}:
            col_join = self.col_join[key]
            col_join = [col.lower() for col in ([col_join] if not isinstance(col_join, list) else col_join)]
            dfr['trip'] = pd.merge(dfr['trip'], dfr[key], how='left', left_on=col_join,
                                   right_on=col_join, suffixes=('', ''))
        return dfr['trip']

    def _preprocess(self, dfr: pd.DataFrame) -> pd.DataFrame:
        # pre-processing
        uti.log_stderr('\nPre-processing')
        uti.log_stderr(' .. correct dtypes')
        for col in ['w2', 'w5', 'w5xhh', 'jjxsc', 'tripdisincsw', 'triptravtime', 'numcarvan']:
            dfr[col] = dfr[col].str.replace(' ', '0').astype(float) if self._is_object(dfr[col]) else dfr[col]

        dfr['trips'] = dfr['w2'] * dfr['w5xhh'] * dfr['jjxsc']
        dfr['tripdisincsw'] = dfr['tripdisincsw'] * self.m2k_fact
        dfr.rename(columns={'tripdisincsw': 'tripdisincsw_km'}, inplace=True)

        # address incorrectly reported purpose, update preceding leg
        # dfr['trippurpto_b01id'] = np.where(mask, dfr['trippurpfrom_b01id'].shift(-1), dfr['trippurpto_b01id'])
        uti.log_stderr(' .. correct trip chains')
        dfr.sort_values(['surveyyear', 'householdid', 'individualid', 'dayid', 'tripid'], ascending=True,
                        ignore_index=True, inplace=True)
        mask = (dfr['individualid'] == dfr['individualid'].shift()) & (dfr['tripid'] == dfr['tripid'].shift() + 1)
        dfr['trippurpfrom_b01id'] = np.where(mask, dfr['trippurpto_b01id'].shift(), dfr['trippurpfrom_b01id'])
        dfr['triporigcounty_b01id'] = np.where(mask, dfr['tripdestcounty_b01id'].shift(), dfr['triporigcounty_b01id'])
        dfr['triporiggor_b02id'] = np.where(mask, dfr['tripdestgor_b02id'].shift(), dfr['triporiggor_b02id'])

        # infill tripstart_b01id = tripend_b01id - 100 * int(triptravtime / 60)
        uti.log_stderr(' .. infill [tripstart_b01id]')
        if self.luk.col_type is str:
            dfr['fill'] = dfr['tripend_b01id'].apply(lambda x: int(x.split('-')[0]))
            dfr['fill'] = self._fill_tripstart(dfr['fill'], dfr['triptravtime'])
            dfr['fill'] = dfr['fill'].apply(lambda x: f'{str(x).zfill(4)} - {str(x + 59).zfill(4)}')
        else:
            dfr['fill'] = 100 * (dfr['tripend_b01id'] - 1)  # tripend hour
            dfr['fill'] = self._fill_tripstart(dfr['fill'], dfr['triptravtime'])
            dfr['fill'] = (dfr['fill'] / 100 + 1).astype(int)
        mask = (dfr['tripstart_b01id'].isin(['0', 0])) & (~dfr['tripend_b01id'].isin(['0', 0]))
        dfr['tripstart_b01id'] = np.where(mask, dfr['fill'], dfr['tripstart_b01id'])

        # apply lookups
        uti.log_stderr(f' .. apply lookup tables')
        dfr['mode'] = self._lookup(dfr, self.luk.mode())
        dfr['purpose'] = self._lookup(dfr, self.luk.purpose())
        dfr['period'] = self._lookup(dfr, self.luk.time_start())
        dfr['direction'] = self._lookup(dfr, self.luk.direction())
        dfr['ruc_2011'] = self._lookup(dfr, self.luk.settlement())
        dfr['occupant'] = self._lookup(dfr, self.luk.occupant())
        dfr['aws'] = self._lookup(dfr, self.luk.aws())
        dfr['gender'] = self._lookup(dfr, self.luk.gender())
        dfr['hh_type'] = self._lookup(dfr, self.luk.hh_type())
        dfr['soc'] = self._lookup(dfr, self.luk.x_soc())
        dfr['ns'] = self._lookup(dfr, self.luk.ns_sec())
        # issue with aws = fte/pte but ns-sec = unemployed, set ns-sec = not classified
        dfr['ns'] = np.where((dfr['ns'] == 4) & (dfr['aws'].isin([2, 3])), 5, dfr['ns'])

        # create traveller type
        dfr['tt'] = self._traveller_type(dfr, ['aws', 'gender', 'hh_type', 'soc', 'ns'])

        # address invalid records in the database
        uti.log_stderr(f' .. address invalid records')
        col_base, col_target = 'hholdoslaua_b01id', 'hholdcounty_b01id'
        sec_dict = dfr.loc[self._mask_invalid(dfr, col_target)]
        sec_dict = sec_dict.groupby([col_base, col_target])['trips'].sum().reset_index()
        sec_dict = sec_dict.loc[sec_dict.groupby([col_base])['trips'].agg(pd.Series.idxmax)]
        sec_dict = sec_dict.set_index(col_base).to_dict()[col_target]

        dfr['fill'] = dfr[col_base].apply(lambda x: sec_dict[x])
        dfr[col_target] = self._fill_record(dfr, col_target, 'fill')

        # address invalid hhold/triporig_ua_b01id
        dfr['hholdua_b01id'] = self._fill_record(dfr, 'hholdua2009_b01id', 'hholdua1998_b01id')
        dfr['triporigua_b01id'] = self._fill_record(dfr, 'triporigua2009_b01id', 'triporigua1998_b01id')

        # infer ruc_orig from area_type & ruc_2011
        dfr.drop(columns='fill', inplace=True)
        dfr['hholdareatype_b01id'] = self._fill_record(dfr, 'hholdareatype2_b01id', 'hholdareatype1_b01id')
        dfr['triporigareatype_b01id'] = self._fill_record(dfr, 'triporigareatype2_b01id', 'triporigareatype1_b01id')
        dfr['ruc_orig'] = self._infill_ruc(dfr, 'county')

        # populate tour
        def fr_home(x):
            return 1 if self.luk.col_type is str and x.lower() == 'home' else 1 if x == 23 else 0

        uti.log_stderr(' .. create activity group')
        dfr.sort_values(['individualid', 'tripid'], ascending=True, ignore_index=True, inplace=True)
        dfr['tour'] = dfr['trippurpfrom_b01id'].apply(lambda x: fr_home(x))
        dfr['tour'] = dfr.groupby('individualid')['tour'].cumsum()
        return dfr

    def _traveller_type(self, dfr: pd.DataFrame, col_list: List) -> pd.Series:
        # TODO: to be finalised once tfn_tt is decided
        uti.log_stderr(f' .. traveller_type = {col_list}')
        dct = dfr.groupby(col_list)[['trips']].sum().reset_index()
        dct['trips'] = 1
        dct.loc[(dct == 0).any(axis=1), 'trips'] = 0
        dct = dct.set_index(['trips'] + col_list).sort_index().reset_index()
        dct['trips'] = dct['trips'].cumsum()
        dct = dct.set_index(col_list).to_dict()['trips']
        out = self.fun.dfr_to_tuple(dfr, col_list)
        return out.apply(lambda x: dct[x])

    def _mask_invalid(self, dfr: pd.DataFrame, col_name: str) -> pd.Series:
        # exclude invalid records for masking purpose
        return (~dfr[col_name].str.lower().isin(['dead', 'dna', '0', 0]) if self.luk.col_type is str
                else ~dfr[col_name].isin([-8, -9, -10, 0]))

    def _infill_ruc(self, dfr: pd.DataFrame, level: str = 'county') -> np.ndarray:
        # use household data to derive ruc2011 based on [county, area_type]
        col_used = [f'hhold{level}_b01id', 'hholdareatype_b01id']
        col_type = self.luk.col_type
        dct = self._write_stats(dfr, col_used + ['ruc_2011']).reset_index()
        dct = dct.loc[self._mask_invalid(dfr, 'hholdareatype_b01id')]
        dct = dct.loc[dct.groupby(col_used)['trips'].agg(pd.Series.idxmax)]

        # create dictionary {[county, area_type]: ruc}
        for col in col_used:
            dct[col] = dct[col].str.lower() if col_type is str else dct[col]
        dct = dct.set_index(col_used).to_dict()['ruc_2011']

        # apply ruc to trip data [county, area_type]
        col_used = [f'triporig{level}_b01id', 'triporigareatype_b01id']
        ruc = dfr[col_used].copy()
        for col in col_used:
            ruc[col] = ruc[col].str.lower() if col_type is str else ruc[col]
        ruc = self.fun.dfr_to_tuple(ruc, col_used)
        ruc = ruc.apply(lambda x: dct.get(x, 0))

        # final result
        col_hold, col_orig = f'hhold{level}_b01id', f'triporig{level}_b01id'
        ruc = np.where((dfr['direction'] == 'hb_fr') & (~dfr['ruc_2011'].isin([0])) &
                       (dfr[col_hold].str.lower() == dfr[col_orig].str.lower() if col_type is str
                        else dfr[col_hold] == dfr[col_orig]),
                       dfr['ruc_2011'], ruc)
        return ruc

    def _lookup(self, dfr: pd.DataFrame, col_dict: Dict) -> Union[pd.DataFrame, pd.Series]:
        col_name = self.fun.str_to_list(col_dict['col'])
        col_type = self.fun.str_to_list(col_dict['typ'])
        dfr = dfr[col_name].copy()
        for idx, col in enumerate(col_name):
            dfr[col] = dfr[col].str.lower() if col_type[idx] == str else dfr[col].astype(col_type[idx])
        dfr = self.fun.dfr_to_tuple(dfr, col_name)
        dfr = dfr.apply(lambda x: col_dict['val'].get(x, 0))
        return dfr

    def _trip_length(self, dfr: pd.DataFrame, mode: List = None, lev_incl: str = 'county',
                     inc_list: Union[List, Dict] = None, seg_incl: Union[List, str] = None):
        uti.log_stderr('\nNTS trip length distribution')
        uti.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(lev_incl), self.luk.col_type
        lev_orig, lev_dest = lev_2col['o'], lev_2col['d']
        seg_incl = self.fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = [lev_orig, lev_dest, 'mode', 'purpose', 'direction', 'period', 'tripdisincsw']
        dfr = dfr[col_used + seg_incl + ['trips']].copy()
        if 'ruc_orig' in seg_incl:
            dfr = dfr.set_index('ruc_orig').rename(index=self._agg_ruc(col_type)).reset_index()
        dfr = dfr.set_index('purpose').rename(index=self._agg_purpose(col_type)).reset_index()
        dfr = dfr.set_index('mode').rename(index=self._agg_mode(col_type)).reset_index()
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum().reset_index()
        # write output
        uti.log_stderr(f' .. write output')
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[(dfr['mode'].isin(mode)) & (~dfr['purpose'].isin([0])) & (~dfr['direction'].isin([0]))
                      & (~dfr['period'].isin([0]))]
        if inc_list is not None:
            dct = self.fun.list_to_dict(inc_list)
            msk_orig = (dfr[lev_orig].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_orig].isin(list(dct)))
            msk_dest = (dfr[lev_dest].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_dest].isin(list(dct)))
            dfr = dfr.loc[msk_orig | msk_dest]
            if isinstance(inc_list, dict):
                dfr = dfr.set_index([lev_orig, lev_dest]).rename(index=dct).reset_index()
        dfr['purpose'], dfr['trip_kms'] = self._update_nhb(dfr, col_type), dfr['trips'].mul(dfr['tripdisincsw'])
        self._to_csv(dfr, 'NTS_trip_length', False)

    def _trip_rates(self, dfr: pd.DataFrame, mode: List = None, lev_incl: str = 'county',
                    inc_list: Union[Dict, List] = None, seg_incl: Union[List, str] = None,
                    inc_dist: bool = True):
        uti.log_stderr('\nNTS trip rates')
        uti.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(lev_incl), self.luk.col_type
        seg_incl = self.fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = [lev_2col['h'], 'mode', 'purpose', 'direction', 'period']
        col_used = col_used + ['tripdisincsw'] if inc_dist else col_used
        dfr = dfr[col_used + seg_incl + ['individualid', 'w2', 'trips']].copy()
        if 'ruc_2011' in seg_incl:
            dfr = dfr.set_index('ruc_2011').rename(index=self._agg_ruc(col_type)).reset_index()
        dfr = dfr.set_index('purpose').rename(index=self._agg_purpose(col_type)).reset_index()
        dfr = dfr.set_index('mode').rename(index=self._agg_mode(col_type)).reset_index()
        # calculate trip-rates
        pop = dfr.groupby([lev_2col['h'], 'individualid'] + seg_incl)['w2'].mean().fillna(0).reset_index()
        pop = pop.groupby([lev_2col['h']] + seg_incl)['w2'].sum().reset_index()
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum().reset_index()
        dfr = pd.merge(dfr, pop, how='left', left_on=[lev_2col['h']] + seg_incl,
                       right_on=[lev_2col['h']] + seg_incl, suffixes=('', ''))
        # write output
        uti.log_stderr(f' .. write output')
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[(dfr['mode'].isin(mode)) & (~dfr['purpose'].isin([0])) & (~dfr['direction'].isin([0]))
                      & (~dfr['period'].isin([0]))]
        if inc_list is not None:
            dct = self.fun.list_to_dict(inc_list)
            dfr = dfr.loc[dfr[lev_2col['h']].str.lower().isin(list(dct))]
            dfr[lev_2col['h']] = dfr[lev_2col['h']].str.lower()
            if isinstance(inc_list, dict):
                dfr = dfr.set_index(lev_2col['h']).rename(index=dct).reset_index()
        dfr['purpose'] = self._update_nhb(dfr, col_type)
        self._to_csv(dfr, 'NTS_trip_rates', False)

    def _tour_props(self, dfr: pd.DataFrame, mode: List = None, lev_incl: str = 'county',
                    inc_list: Union[List, Dict] = None, seg_incl: Union[List, str] = None):
        uti.log_stderr('\nNTS tour proportions')
        uti.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(lev_incl), self.luk.col_type
        seg_incl = self.fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = [lev_2col['h'], 'mode', 'purpose', 'period']
        dfr = dfr[col_used + seg_incl + ['individualid', 'direction', 'tour', 'trips']].copy()
        if 'ruc_2011' in seg_incl:
            dfr = dfr.set_index('ruc_2011').rename(index=self._agg_ruc(col_type)).reset_index()
        dfr = dfr.set_index('purpose').rename(index=self._agg_purpose(col_type)).reset_index()
        dfr = dfr.set_index('mode').rename(index=self._agg_mode(col_type)).reset_index()
        # from home
        frh = dfr.loc[dfr['direction'] == 'hb_fr'].sort_values(['individualid', 'tour'])
        toh = dfr.loc[dfr['direction'] == 'hb_to', ['individualid', 'tour', 'period', 'trips']]
        dfr = pd.merge(frh, toh, how='left', left_on=['individualid', 'tour'], right_on=['individualid', 'tour'],
                       suffixes=('', '_return'))
        dfr['trips'] = dfr[['trips', 'trips_return']].mean(axis=1)
        dfr = dfr.groupby(col_used + seg_incl + ['period_return'])[['trips']].sum().reset_index()
        # write output
        uti.log_stderr(f' .. write output')
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[(dfr['mode'].isin(mode)) & (~dfr['purpose'].isin([0])) & (~dfr['period'].isin([0])) &
                      (~dfr['period_return'].isin([0]))]
        if inc_list is not None:
            dct = self.fun.list_to_dict(inc_list)
            mask = (dfr[lev_2col['h']].str.lower().isin(list(dct)) if col_type is str
                    else dfr[lev_2col['h']].isin(list(dct)))
            dfr = dfr.loc[mask]
            if isinstance(inc_list, dict):
                dfr = dfr.set_index(lev_2col['h']).rename(index=dct).reset_index()
        self._to_csv(dfr, 'NTS_tour_proportions', False)

    def _occs_vehicle(self, dfr: pd.DataFrame, mode: List = None, lev_incl: str = 'county',
                      inc_list: Union[List, Dict] = None, seg_incl: Union[List, str] = None,
                      inc_dist: bool = True):
        uti.log_stderr('\nNTS occupancies')
        uti.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(lev_incl), self.luk.col_type
        lev_orig, lev_dest = lev_2col['o'], lev_2col['d']
        seg_incl = self.fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = [lev_orig, lev_dest, 'mode', 'purpose', 'direction', 'period', 'occupant']
        col_used = col_used + ['tripdisincsw'] if inc_dist else col_used
        dfr = dfr[col_used + seg_incl + ['trips']].copy()
        dfr = dfr.set_index('purpose').rename(index=self._agg_purpose(col_type)).reset_index()
        dfr = dfr.set_index('mode').rename(index=self._agg_mode(col_type)).reset_index()
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum(col_type).reset_index()
        # write output
        uti.log_stderr(f' .. write output')
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[(dfr['mode'].isin(mode)) & (~dfr['purpose'].isin([0])) & (~dfr['direction'].isin([0]))
                      & (~dfr['period'].isin([0]))]
        if inc_list is not None:
            dct = self.fun.list_to_dict(inc_list)
            msk_orig = (dfr[lev_orig].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_orig].isin(list(dct)))
            msk_dest = (dfr[lev_dest].str.lower().isin(list(dct)) if col_type is str else
                        dfr[lev_dest].isin(list(dct)))
            dfr = dfr.loc[msk_orig | msk_dest]
            if isinstance(inc_list, dict):
                dfr = dfr.set_index([lev_orig, lev_dest]).rename(index=dct).reset_index()
        dfr['purpose'] = self._update_nhb(dfr, col_type)
        self._to_csv(dfr, 'NTS_vehicle_occupancy', False)

    def _activity(self, dfr: pd.DataFrame, mode: List = None, lev_incl: str = 'county',
                  seg_incl: Union[List, str] = None):
        uti.log_stderr('\nNTS activity')
        uti.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(lev_incl), self.luk.col_type
        lev_prod, lev_orig, lev_dest = lev_2col['h'], lev_2col['o'], lev_2col['d']
        seg_incl = self.fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = [lev_prod, lev_orig, lev_dest, 'surveyyear', 'individualid', 'tripid', 'tour', 'mode',
                    'purpose', 'direction', 'period', 'tripdisincsw']
        dfr = dfr[col_used + seg_incl + ['w2', 'trips']].copy()
        # dfr = dfr.loc[~dfr['surveyyear'].isin([2020, 2021])].copy()
        dfr.rename(columns={'w2': 'freq', 'trips': 'trip'}, inplace=True)
        dfr.sort_values(['individualid', 'tour', 'tripid'], inplace=True, ascending=True, ignore_index=True)
        dfr['freq'] = np.where(dfr['direction'] == 'hb_to', 0, dfr['purpose'])
        dfr['freq'] = dfr['freq'].astype(str)
        dfr['tour_id'] = '0_' + dfr.groupby(['individualid', 'tour'])['freq'].transform(lambda x: '_'.join(x))
        dfr['tour_id'] = dfr['tour_id'].apply(lambda x: x[:-1] + '0' if x[-1] != '0' else x)
        dfr['freq'] = dfr['trip'].div(dfr.groupby(['individualid', 'tour'])['tour_id'].transform('count')).fillna(0)

        # write output
        uti.log_stderr(f' .. write output')
        act = dfr.groupby([lev_prod, 'tour_id'])[['freq', 'trip']].sum()
        self._to_csv(act, f'NTS_activity_{lev_incl}')

        col_grby = [lev_orig, lev_dest, 'mode', 'purpose', 'direction', 'period', 'tripdisincsw']
        dfr = dfr.groupby(col_grby)[['freq', 'trip']].sum()
        dfr = dfr.sort_index().reset_index()
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[(dfr['mode'].isin(mode)) & (~dfr['purpose'].isin([0])) & (~dfr['direction'].isin([0]))
                      & (~dfr['period'].isin([0]))]
        self._to_csv(dfr, f'NTS_mode_time_split_{lev_incl}', False)

    def _trip_rates_hb(self, dfr: pd.DataFrame, mode: List = None, lev_incl: str = 'county',
                       seg_incl: Union[List, str] = None):
        uti.log_stderr('\nNTS hb trip rates')
        uti.log_stderr(f' .. process data')
        lev_2col, col_type = self._level_to_col(lev_incl), self.luk.col_type
        seg_incl = self.fun.str_to_list(seg_incl) if seg_incl is not None else []
        col_used = [lev_2col['h'], 'purpose']
        dfr = dfr[col_used + seg_incl + ['mode', 'direction', 'individualid', 'w2', 'trips']].copy()
        if 'ruc_2011' in seg_incl:
            dfr = dfr.set_index('ruc_2011').rename(index=self._agg_ruc(col_type)).reset_index()
        dfr = dfr.set_index('purpose').rename(index=self._agg_purpose(col_type)).reset_index()
        dfr = dfr.set_index('mode').rename(index=self._agg_mode(col_type)).reset_index()
        # calculate trip-rates
        mode = list(self._agg_mode(col_type).values()) if mode is None else mode
        dfr = dfr.loc[(dfr['direction'].isin(['hb_fr'])) & (dfr['mode'].isin(mode))]
        pop = dfr.groupby([lev_2col['h'], 'individualid'] + seg_incl)['w2'].mean().fillna(0).reset_index()
        pop = pop.groupby([lev_2col['h']] + seg_incl)['w2'].sum().reset_index()
        dfr = dfr.groupby(col_used + seg_incl)[['trips']].sum().reset_index()
        col_indx = list(set(col_used + seg_incl) - {'purpose'})
        dfr = pd.pivot_table(dfr, values='trips', index=col_indx, columns='purpose').fillna(0)
        dfr = pd.merge(pop, dfr, how='left', left_on=col_indx, right_on=col_indx, suffixes=('', ''))
        # write output
        uti.log_stderr(f' .. write output')
        self._to_csv(dfr.loc[~dfr['tt'].isin([0])], 'NTS_trip_rates_hb', False)

    @staticmethod
    def _level_to_col(level: str = 'county') -> Dict:
        home = f'hhold{level}_b02id' if level.lower() == 'gor' else f'hhold{level}_b01id'
        orig = f'triporig{level}_b02id' if level.lower() == 'gor' else f'triporig{level}_b01id'
        dest = f'tripdest{level}_b02id' if level.lower() == 'gor' else f'tripdest{level}_b01id'
        return {'h': home, 'o': orig, 'd': dest}

    def _agg_purpose(self, col_type: type = int) -> Dict:
        if col_type is int:
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

    def _agg_ruc(self, col_type: type = int) -> Dict:
        if col_type is int:
            out_dict = {key: key for key in [1, 2, 3, 4, 5]}
        else:
            out_dict = {'major': 1, 'minor': 2, 'urban': 3, 'rural': [4, 5]}
        return self.luk.val_to_key(out_dict)

    def _fill_record(self, dfr: pd.DataFrame, col_update: str, frm_column: str,
                     direction: Union[List, str] = None) -> np.ndarray:
        """ update [col_update] by infilling data from [frm_column] if invalid data & direction """
        direction = ['hb_fr', 'hb_to', 'nhb', '0', 0] if direction is None else direction
        direction = self.fun.str_to_list(direction)
        rec = np.where((dfr[col_update].str.lower().isin(['dead', 'dna', '0']) if self._is_object(dfr[col_update])
                        else dfr[col_update].isin([-8, -9, -10, 0])) &
                       (dfr['direction'].isin(direction)), dfr[frm_column], dfr[col_update])
        return rec

    @staticmethod
    def _fill_tripstart(dfr_fill: pd.Series, dfr_time: pd.Series) -> pd.Series:
        dfr_fill = dfr_fill - 100 * (dfr_time.astype(float) / 60).astype(int)
        _mask = dfr_fill.loc[dfr_fill < 0]
        while len(_mask) > 0:
            dfr_fill = np.where(dfr_fill < 0, 2400, 0) + dfr_fill
            _mask = dfr_fill.loc[dfr_fill < 0]
        return dfr_fill.astype(int)

    @staticmethod
    def _is_object(dfr: pd.Series) -> bool:
        return dfr.dtype in ['str', 'object']

    @staticmethod
    def _update_nhb(dfr: pd.DataFrame, col_type: type = int) -> np.ndarray:
        return np.where(dfr['direction'] == 'xxx', (10 if col_type is int else 'n') + dfr['purpose'], dfr['purpose'])

    def _summary_ub(self, dfr: pd.DataFrame):
        uti.log_stderr(f'\nSummary (unclassified)')
        self._write_stats(dfr, ['surveyyear'], 'NTS_ub_year')
        self._write_stats(dfr, ['hholdgor_b02id'], 'NTS_ub_gor')
        self._write_stats(dfr, ['hholdcounty_b01id'], 'NTS_ub_county')
        self._write_stats(dfr, ['ruc_2011', 'settlement2011ew_b01id'], 'NTS_ub_ruc2011')
        self._write_stats(dfr, ['mode', 'mainmode_b11id'], 'NTS_ub_mode')
        self._write_stats(dfr, ['purpose', 'direction', 'trippurpfrom_b01id', 'trippurpto_b01id'], 'NTS_ub_purpose')
        self._write_stats(dfr, ['period', 'tripstart_b01id'], 'NTS_ub_period')
        self._write_stats(dfr, ['gender', 'sex_b01id', 'age_b01id'], 'NTS_ub_gender')
        self._write_stats(dfr, ['aws', 'gender', 'ecostat_b01id'], 'NTS_ub_aws')
        self._write_stats(dfr, ['hh_type', 'hholdnumadults', 'numcarvan'], 'NTS_ub_hhold')
        self._write_stats(dfr, ['soc', 'xsoc2000_b02id', 'age_b01id', 'ecostat_b01id'], 'NTS_ub_soc')
        self._write_stats(dfr, ['ns', 'nssec_b03id'], 'NTS_ub_ns')
        self._write_stats(dfr, ['tt', 'aws', 'gender', 'hh_type', 'soc', 'ns'], 'NTS_ub_trips_by_tt')

    def _to_csv(self, dfr: pd.DataFrame, csv_name: str, index: bool = True):
        dfr.to_csv(fr'{self.out_fldr}\{csv_name}.csv', index=index)

    # swap orig/dest for return_home trips
    @staticmethod
    def _transpose(dfr: pd.DataFrame, col_swap: List) -> pd.DataFrame:
        col_orig, col_dest = dfr[col_swap[0]], dfr[col_swap[1]]
        dfr.loc[dfr['trip_direction'] == 'hb_to', col_swap[0]] = col_dest
        dfr.loc[dfr['trip_direction'] == 'hb_to', col_swap[1]] = col_orig
        return dfr

    def _write_stats(self, dfr: pd.DataFrame, col_stat: Union[List, str], csv_name: str = None) -> pd.DataFrame:
        col_stat = [col.lower() for col in self.fun.str_to_list(col_stat)]
        uti.log_stderr(f' .. write {col_stat} -> {csv_name}') if csv_name is not None else None
        out = dfr.groupby(col_stat)['trips'].sum()
        self._to_csv(out, csv_name) if csv_name is not None else None
        return out


# CLASS FUNCTIONS
class Function:
    def __init__(self):
        """ NTS supporting functions """

    @staticmethod
    def csv_to_dfr(csv_file: str, col_incl: Union[List, str] = None) -> Union[pd.DataFrame, bool]:
        uti.log_stderr(f' .. read {csv_file}')
        _, _, _, csv_extn = uti.split_file(csv_file)
        csv_extn = '\t' if csv_extn.lower() == '.tab' else ','
        if col_incl is not None:
            col_incl = list(col_incl.lower()) if isinstance(col_incl, str) else [key.lower() for key in col_incl]
        # dfr_data = dd.read_csv(csv_file, assume_missing=True, low_memory=False)
        dfr = pd.read_csv(csv_file, sep=csv_extn, low_memory=False)
        dfr = dfr.rename(columns={key: key.lower().strip() for key in dfr.columns})
        try:
            dfr = dfr[col_incl] if col_incl is not None else dfr
            dfr = dfr.fillna('0')
        except KeyError as err:
            uti.log_stderr(f'    error with {csv_file}: {err}')
            dfr = False
        return dfr

    @staticmethod
    def str_to_list(str_text: Union[str, float, int, List]) -> List:
        return [str_text] if not isinstance(str_text, list) else str_text

    @staticmethod
    def list_to_dict(dct: Union[List, Dict]) -> Dict:
        return {col.lower(): dct[col] for col in dct} if isinstance(dct, dict) else {col.lower(): 1 for col in dct}

    @staticmethod
    def dfr_to_dict(dfr: pd.DataFrame, key: str, val: str) -> Dict:
        return dfr[[key, val]].set_index(key).to_dict()[val]

    @staticmethod
    def dfr_to_tuple(dfr: pd.DataFrame, col_used: List) -> pd.Series:
        # create tuple from dfr columns
        col_2zip = zip(*[dfr[col] for col in col_used])
        return dfr[col_used[0]] if len(col_used) == 1 else pd.Series([col for col in col_2zip])


# CLASS LOOKUP
class Lookup:
    def __init__(self, col_type: type = int):
        self.col_type = col_type
        # trip purpose
        self.tpp_01id = {
            'com': {1: 'work', 18: 'escort work'},
            'emb': {2: 'in course of work', 19: 'escort in course of work'},
            'edu': {3: 'education', 20: 'escort education'},
            'shp': {4: 'food shopping', 5: 'non food shopping', 21: 'escort shopping / personal business'},
            'peb': {6: 'personal business medical', 7: 'personal business eat / drink', 8: 'personal business other',
                    16: 'other non-escort', 17: 'escort home', 22: 'other escort'},
            'soc': {9: 'eat / drink with friends', 11: 'other social', 12: 'entertain /  public activity',
                    13: 'sport: participate'},
            'vis': {10: 'visit friends'},
            'hol': {14: 'holiday: base', 15: 'day trip / just walk'},
            'hom': {23: 'home'}
        }

        # weekday & weekend
        self.wkd_01id = {
            'wkd': {1: 'monday', 2: 'tuesday', 3: 'wednesday', 4: 'thursday', 5: 'friday'},
            'sat': {6: 'saturday'},
            'sun': {7: 'sunday'}
        }

        # hours to periods
        self.ttp_01id = {
            'am': {8: '0700 - 0759', 9: '0800 - 0859', 10: '0900 - 0959'},
            'ip': {11: '1000 - 1059', 12: '1100 - 1159', 13: '1200 - 1259',
                   14: '1300 - 1359', 15: '1400 - 1459', 16: '1500 - 1559'},
            'pm': {17: '1600 - 1659', 18: '1700 - 1759', 19: '1800 - 1859'},
            'op': {1: '0000 - 0059', 2: '0100 - 0159', 3: '0200 - 0259', 4: '0300 - 0359',
                   5: '0400 - 0459', 6: '0500 - 0559', 7: '0600 - 0659',
                   20: '1900 - 1959', 21: '2000 - 2059', 22: '2100 - 2159', 23: '2200 - 2259', 24: '2300 - 2359'},
            'na': {-8: 'na', -10: 'dead', 0: '0', '0': '0'}
        }

        # age profile
        self.age_01id = {
            'child': {1: 'less than 1 year', 2: '1 - 2 years', 3: '3 - 4 years', 4: '5 - 10 years', 5: '11 - 15 years'},
            'adult': {6: '16 years', 7: '17 years', 8: '18 years', 9: '19 years', 10: '20 years', 11: '21 - 25 years',
                      12: '26 - 29 years', 13: '30 - 39 years', 14: '40 - 49 years', 15: '50 - 59 years',
                      16: '60 - 64 years', 17: '65 - 69 years', 18: '70 - 74 years'},
            'elder': {19: '75 - 79 years', 20: '80 - 84 years', 21: '85 years +'}
        }

        # gender
        self.sex_01id = {
            'male': {1: 'male'},
            'female': {2: 'female'}
        }
        # work status
        self.eco_01id = {
            'fte': {1: 'employees: full-time', 3: 'self-employed: full-time'},
            'pte': {2: 'employees: part-time', 4: 'self-employed: part-time'},
            'stu': {7: 'economically inactive: student'},
            'unm': {5: 'ILO unemployed', 6: 'economically inactive: retired',
                    8: 'economically inactive: looking after family / home',
                    9: 'economically inactive: permanently sick / disabled',
                    10: 'economically inactive: temporarily sick / injured',
                    11: 'economically inactive: other'},
            'dna': {-8: 'na',  -9: 'dna', -10: 'dead', 0: '0'}
        }

        # standard occupational classification
        self.soc_02id = {
            'hig': {1: 'managers and senior officials', 2: 'professional occupations',
                    3: 'associate professional and technical occupations'},
            'med': {4: 'administrative and secretarial occupations', 5: 'skilled trades occupations',
                    6: 'personal service occupations', 7: 'sales and customer service occupations'},
            'low': {8: 'process, plant and machine operatives', 9: 'elementary occupations'},
            'dna': {-8: 'na', -9: 'dna'}
        }

        # national statistics - social economic classification
        self.sec_03id = {
            'ns1': {1: 'managerial and professional occupations'},
            'ns2': {2: 'intermediate occupations and small employers'},
            'ns3': {3: 'routine and manual occupations'},
            'ns4': {4: 'never worked and long-term unemployed'},
            'ns5': {5: 'not classified (including students)'},
            'dna': {-9: 'dna'}
        }

        # main mode
        self.mmd_01id = {
            'walk': {1: 'walk, less than 1 mile', 2: 'walk, 1 mile or more'},
            'bike': {3: 'bicycle'},
            'car_d': {5: 'private car: driver', 7: 'motorcycle / scooter / moped: driver',
                      11: 'other private transport'},
            'car_p': {6: 'private car: passenger', 8: 'motorcycle / scooter / moped: passenger',
                      20: 'taxi', 21: 'minicab'},
            'van_d': {9: 'van / lorry: driver'},
            'van_p': {10: 'van / lorry: passenger'},
            'bus_d': {4: 'private (hire) bus'},
            'bus_p': {12: 'london stage bus', 13: 'other stage bus', 14: 'coach / express bus',
                      15: 'excursion / tour bus', 22: 'other public transport'},
            'rail_l': {16: 'london underground', 18: 'light rail'},
            'rail_s': {17: 'surface rail'},
            'air': {19: 'air'}
        }

        # settlement ruc 2011
        self.set_01id = {
            'major': {'a1': 'urban - major conurbation'},
            'minor': {'b1': 'urban - minor conurbation', '1': 'large urban area - scotland'},
            'city': {'c1': 'urban - city and town', 'c2': 'urban - city and town in a sparse setting',
                     '2': 'other urban area - scotland'},
            'town': {'d1': 'rural - town and fringe', 'd2': 'rural - town and fringe in a sparse setting',
                     '3': 'accessible small town - scotland', '4': 'remote small town - scotland',
                     '5': 'very remote small town - scotland'},
            'village': {'e1': 'rural - village', 'e2': 'rural - village in a sparse setting',
                        'f1': 'rural - hamlets and isolated dwellings',
                        'f2': 'rural - hamlets and isolated dwellings in a sparse setting',
                        '6': 'accessible rural area - scotland', '7': 'remote rural area - scotland',
                        '8': 'very remote rural area - scotland'}
        }

        # government office region
        self.gor_02id = {1: 'north east', 2: 'north west', 3: 'yorkshire and the humber',
                         4: 'east midlands', 5: 'west midlands', 6: 'east of england',
                         7: 'london', 8: 'south east', 9: 'south west',
                         10: 'wales', 11: 'scotland', -8: 'na', -9: 'dna'
                         }

        # convert specs to either key or values
        self.set_01id = self.dct_to_specs(self.set_01id, col_type)
        self.mmd_01id = self.dct_to_specs(self.mmd_01id, col_type)
        self.tpp_01id = self.dct_to_specs(self.tpp_01id, col_type)
        self.age_01id = self.dct_to_specs(self.age_01id, col_type)
        self.sex_01id = self.dct_to_specs(self.sex_01id, col_type)
        self.eco_01id = self.dct_to_specs(self.eco_01id, col_type)
        self.wkd_01id = self.dct_to_specs(self.wkd_01id, col_type)
        self.ttp_01id = self.dct_to_specs(self.ttp_01id, col_type)
        self.soc_02id = self.dct_to_specs(self.soc_02id, col_type)
        self.sec_03id = self.dct_to_specs(self.sec_03id, col_type)

        self.nhb_trip = (self.tpp_01id['com'] + self.tpp_01id['emb'] + self.tpp_01id['edu'] +
                         self.tpp_01id['shp'] + self.tpp_01id['peb'] + self.tpp_01id['soc'] +
                         self.tpp_01id['vis'] + self.tpp_01id['hol'])

    def hh_type(self) -> Dict:
        # household type
        out_dict = {'col': ['hholdnumadults', 'numcarvan'], 'typ': [int, int],
                    'val': {1: [(1, 0)],
                            2: uti.product([1], range(1, 10)),  # 1 adult with 1+ cars
                            3: [(2, 0)],
                            4: [(2, 1)],
                            5: uti.product([2], range(2, 10)),  # 2 adults with 2+ cars
                            6: uti.product(range(3, 11), [0]),
                            7: uti.product(range(3, 11), [1]),
                            8: uti.product(range(3, 11), range(2, 10))  # 3+ adults with 2+ cars
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def aws(self) -> Dict:
        # age, work, status
        eco_01id, age_01id = self.eco_01id, self.age_01id
        eco_over = eco_01id['fte'] + eco_01id['pte'] + eco_01id['stu'] + eco_01id['unm']
        out_dict = {'col': ['age_b01id', 'ecostat_b01id'], 'typ': [self.col_type, self.col_type],
                    'val': {1: uti.product(age_01id['child'], eco_01id['dna']),  # child
                            2: uti.product(age_01id['adult'], eco_01id['fte']),  # fte
                            3: uti.product(age_01id['adult'], eco_01id['pte']),  # pte
                            4: uti.product(age_01id['adult'], eco_01id['stu']),  # student
                            5: uti.product(age_01id['adult'], eco_01id['unm']),  # unemployed
                            6: uti.product(age_01id['elder'], eco_over + eco_01id['dna']),  # over 75
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def x_soc(self) -> Dict:
        # soc
        soc_02id, age_01id, eco_01id = self.soc_02id, self.age_01id, self.eco_01id
        all_xsoc = soc_02id['hig'] + soc_02id['med'] + soc_02id['low'] + soc_02id['dna']
        all_ages = age_01id['child'] + age_01id['adult'] + age_01id['elder']
        emp_ecos = eco_01id['fte'] + eco_01id['pte']
        all_ecos = emp_ecos + eco_01id['stu'] + eco_01id['unm'] + eco_01id['dna']
        out_dict = {'col': ['xsoc2000_b02id', 'age_b01id', 'ecostat_b01id'],
                    'typ': [self.col_type, self.col_type, self.col_type],
                    'val': {1: uti.product(soc_02id['hig'], age_01id['adult'], emp_ecos),  # high skill
                            2: uti.product(soc_02id['med'], age_01id['adult'], emp_ecos),  # med skill
                            3: uti.product(soc_02id['low'], age_01id['adult'], emp_ecos),  # low skill
                            4: (uti.product(all_xsoc, all_ages, eco_01id['stu']) +  # student
                                uti.product(all_xsoc, age_01id['adult'], eco_01id['unm']) +  # unemployed
                                uti.product(all_xsoc, age_01id['child'], all_ecos) +  # children
                                uti.product(all_xsoc, age_01id['elder'], all_ecos))  # over 75
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def ns_sec(self) -> Dict:
        # ns-sec
        out_dict = {'col': ['nssec_b03id'], 'typ': [self.col_type],
                    'val': {1: self.sec_03id['ns1'],  # ns-sec 1
                            2: self.sec_03id['ns2'],  # ns-sec 2
                            3: self.sec_03id['ns3'],  # ns-sec 3
                            4: self.sec_03id['ns4'],  # ns-sec 4
                            5: self.sec_03id['ns5'] + self.sec_03id['dna'],  # ns-sec 5
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def settlement(self) -> Dict:
        # ruc 2011 to area type
        out_dict = {'col': 'settlement2011ew_b01id', 'typ': str,
                    'val': {1: self.set_01id['major'],
                            2: self.set_01id['minor'],
                            3: self.set_01id['city'],
                            4: self.set_01id['town'],
                            5: self.set_01id['village']
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def mode(self) -> Dict:
        # main mode
        out_dict = {'col': ['mainmode_b11id'], 'typ': [self.col_type],
                    'val': {1: self.mmd_01id['walk'],  # walk
                            2: self.mmd_01id['bike'],  # cycle
                            3: self.mmd_01id['car_d'] + self.mmd_01id['car_p'],  # car driver/passenger
                            4: self.mmd_01id['van_d'] + self.mmd_01id['van_p'],  # van driver/passenger
                            5: self.mmd_01id['bus_d'] + self.mmd_01id['bus_p'],  # bus
                            6: self.mmd_01id['rail_s'],  # surface rail
                            7: self.mmd_01id['rail_l'],  # light rail/underground
                            8: self.mmd_01id['air']  # air
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def occupant(self) -> Dict:
        # occupant
        mmd_01id = self.mmd_01id
        out_dict = {'col': ['mainmode_b11id'], 'typ': [self.col_type],
                    'val': {'driver': (mmd_01id['walk'] + mmd_01id['bike'] + mmd_01id['car_d'] + 
                                       mmd_01id['van_d'] + mmd_01id['bus_d']),
                            'passenger': (mmd_01id['car_p'] + mmd_01id['van_p'] + mmd_01id['bus_p'] +
                                          mmd_01id['rail_s'] + mmd_01id['rail_l'] + mmd_01id['air'])
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def purpose(self) -> Dict:
        # trip purpose
        tpp_01id, end_home = self.tpp_01id, self.tpp_01id['hom']
        all_trip = self.nhb_trip + end_home
        out_dict = {'col': ['trippurpfrom_b01id', 'trippurpto_b01id'], 'typ': [self.col_type, self.col_type],
                    'val': {1: uti.product(all_trip, tpp_01id['com']) + uti.product(tpp_01id['com'], end_home),
                            2: uti.product(all_trip, tpp_01id['emb']) + uti.product(tpp_01id['emb'], end_home),
                            3: uti.product(all_trip, tpp_01id['edu']) + uti.product(tpp_01id['edu'], end_home),
                            4: uti.product(all_trip, tpp_01id['shp']) + uti.product(tpp_01id['shp'], end_home),
                            5: uti.product(all_trip, tpp_01id['peb']) + uti.product(tpp_01id['peb'], end_home),
                            6: uti.product(all_trip, tpp_01id['soc']) + uti.product(tpp_01id['soc'], end_home),
                            7: uti.product(all_trip, tpp_01id['vis']) + uti.product(tpp_01id['vis'], end_home),
                            8: uti.product(all_trip, tpp_01id['hol']) + uti.product(tpp_01id['hol'], end_home),
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def time_start(self) -> Dict:
        # trip start time
        wkd_01id, ttp_01id = self.wkd_01id, self.ttp_01id
        week_end = ttp_01id['am'] + ttp_01id['ip'] + ttp_01id['pm'] + ttp_01id['op'] + ttp_01id['na']
        out_dict = {'col': ['travelweekday_b01id', 'tripstart_b01id'], 'typ': [self.col_type, self.col_type],
                    'val': {1: uti.product(wkd_01id['wkd'], ttp_01id['am']),
                            2: uti.product(wkd_01id['wkd'], ttp_01id['ip']),
                            3: uti.product(wkd_01id['wkd'], ttp_01id['pm']),
                            4: uti.product(wkd_01id['wkd'], ttp_01id['op']),
                            5: uti.product(wkd_01id['sat'], week_end),
                            6: uti.product(wkd_01id['sun'], week_end)
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def time_end(self) -> Dict:
        # trip end time
        wkd_01id, ttp_01id = self.wkd_01id, self.ttp_01id
        all_24hr = ttp_01id['am'] + ttp_01id['ip'] + ttp_01id['pm'] + ttp_01id['op'] + ttp_01id['na']
        out_dict = {'col': ['travelweekday_b01id', 'tripend_b01id'], 'typ': [self.col_type, self.col_type],
                    'val': {1: uti.product(wkd_01id['wkd'], ttp_01id['am']),
                            2: uti.product(wkd_01id['wkd'], ttp_01id['ip']),
                            3: uti.product(wkd_01id['wkd'], ttp_01id['pm']),
                            4: uti.product(wkd_01id['wkd'], ttp_01id['op']),
                            5: uti.product(wkd_01id['sat'], all_24hr),
                            6: uti.product(wkd_01id['sun'], all_24hr)
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def direction(self) -> Dict:
        # direction of travel, hb_fr, hb_to, nhb
        out_dict = {'col': ['trippurpfrom_b01id', 'trippurpto_b01id'], 'typ': [self.col_type, self.col_type],
                    'val': {'hb_fr': uti.product(self.tpp_01id['hom'], self.nhb_trip),
                            'hb_to': uti.product(self.nhb_trip, self.tpp_01id['hom']),
                            'nhb': uti.product(self.nhb_trip, self.nhb_trip)
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def gender(self) -> Dict:
        # gender
        age_01id, sex_01id = self.age_01id, self.sex_01id
        out_dict = {'col': ['sex_b01id', 'age_b01id'], 'typ': [self.col_type, self.col_type],
                    'val': {1: uti.product(sex_01id['male'] + sex_01id['female'], age_01id['child']),  # child
                            2: uti.product(sex_01id['male'], age_01id['adult'] + age_01id['elder']),  # male
                            3: uti.product(sex_01id['female'], age_01id['adult'] + age_01id['elder'])  # female
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    @staticmethod
    def str_lower(val: Any) -> Any:
        if isinstance(val, str):
            return val.lower()
        elif isinstance(val, (tuple, list, dict, set, np.ndarray)):
            return tuple(itm.lower() if isinstance(itm, str) else itm for itm in val)
        else:
            return val

    def val_to_key(self, dct: Dict) -> Dict:
        dct = {key: [dct[key]] if not isinstance(dct[key], list) else dct[key] for key in dct}
        return {self.str_lower(val): key for key in dct for val in dct[key]}

    @staticmethod
    def dct_to_specs(dct: Dict, out: type = int) -> Dict:
        return {key: list(dct[key].keys() if out is int else dct[key].values()) for key in dct}


# main application
if __name__ == '__main__':
    NTS(r"E:\NTS\UKDA-7553-tab\tab")
