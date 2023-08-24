from typing import Union, List, Dict
import multiprocessing as mp
import mdlfunction as fun
import mdllookup as luk
import mdlconfig
import pandas as pd
import numpy as np
import os

mp.freeze_support()


class ClassifiedBuild:
    """ this process produces NTS classified build for the subsequent NTS processing:
        1. trip-length distribution data for the synthetic matrix development & MND verification
        2. trip rate data for verification purpose
        3. tour proportions for converting PA2OD
        4. car occupancy for converting person trips to vehicle trips

        note:
        1. w1 - household/dwelling unit selection -> xDU*wHH
        2. w2 - household-level non-participation -> for trip-rates
        3. w3 - account for "not every individual completed the interview"
        4. w5xhh = w5/w2 for weekday and 1 for weekend
        5. w5 - composite weights = w1*w2*w3, or w2*w5xhh
        6. jjxsc - series of call & short-walk weighting: 0 if series of call, else 7 if short-walk else 1
        7. trips = w5xhh*w2*jjxsc or w5*jjxsc
        8. trip_rates = trips/w2

        instructions:
        1. specify locations & filenames of raw NTS database, [nts_fldr] and [nts_file]
        2. specify NTS attributes to be included for analysis, [col_incl]
        3. specify output location (where NTS processed data will be written out)
        4. specify geographical level that results are to be produced for each output type [lev_incl]
        """

    def __init__(self, nts_fldr: str, out_fldr: str, cb_version: Union[str, int]):
        # input specs
        self.cur_path = os.path.dirname(os.path.realpath(__file__))
        self.num_cpus = max(os.cpu_count() - 2, 1)
        self.err_indx = True

        # nts-specific specs
        self.nts_fldr, self.out_fldr = nts_fldr, out_fldr
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

        # nts specs
        self.cfg = mdlconfig.Config(out_fldr)
        self.luk = luk.Lookup(self.cfg.nts_dtype)
        self.def_ttype, self.def_atype = self.cfg.def_ttype, self.cfg.def_atype
        self.m2k_fact = self.cfg.m2k_fact
        self.cb_version = cb_version
        yrs_list = self.cfg.yrs_included

        # import & pre-process
        self._exist()
        nts_data = self._read_nts(yrs_list)
        nts_data = self._preprocess(nts_data)
        nts_data = self._apply_lookups(nts_data)
        self._summary(nts_data)

        # write out classified build
        fun.log_stderr('\nWrite cb data to csv')
        out_fldr, csv_name = self.cfg.fld_build, f'{self.cfg.csv_build}_v{self.cb_version}.csv'
        fun.log_stderr(f' .. write to {out_fldr}\\{csv_name}')
        fun.dfr_to_csv(nts_data, out_fldr, csv_name, False)

    def _exist(self):
        fun.log_stderr('\nCheck file existence')
        self.err_indx = fun.exist_path(self.nts_fldr, self.err_indx)
        for col in self.nts_file:
            self.err_indx = fun.exist_file(f'{self.nts_fldr}\\{self.nts_file[col]}', self.err_indx)
        os.makedirs(self.out_fldr, exist_ok=True)
        fun.log_stderr(' .. specified files OK') if self.err_indx else None
        exit() if not self.err_indx else None

    def _read_nts(self, yrs_incl: List = None) -> pd.DataFrame:
        fun.log_stderr('\nImport NTS data')
        # import NTS data
        dfr: Union[Dict, pd.DataFrame] = {}
        pool = mp.Pool(self.num_cpus)
        for key in self.nts_file:
            csv_file = f'{self.nts_fldr}\\{self.nts_file[key]}'
            dfr[key] = pool.apply_async(fun.csv_to_dfr, [csv_file, self.col_incl[key]])
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
        dfr = dfr['trip']
        return dfr if yrs_incl is None else dfr.loc[dfr['surveyyear'].isin(yrs_incl)]

    def _preprocess(self, dfr: pd.DataFrame) -> pd.DataFrame:
        # pre-processing
        fun.log_stderr('\nPre-processing')
        fun.log_stderr(' .. correct dtypes')
        for col in ['w1', 'w2', 'w5', 'w5xhh', 'jjxsc', 'tripdisincsw', 'triptravtime', 'numcarvan']:
            dfr[col] = dfr[col].str.replace(' ', '0').astype(float) if self._is_object(dfr[col]) else dfr[col]

        dfr['trips'] = dfr['w2'].mul(dfr['w5xhh']).mul(dfr['jjxsc'])
        dfr['trav_dist'] = dfr['tripdisincsw'] * self.m2k_fact

        # address incorrectly reported purpose, update preceding leg
        # dfr['trippurpto_b01id'] = np.where(mask, dfr['trippurpfrom_b01id'].shift(-1), dfr['trippurpto_b01id'])
        fun.log_stderr(' .. correct trip chains')
        dfr.sort_values(['surveyyear', 'householdid', 'individualid', 'dayid', 'tripid'], ascending=True,
                        ignore_index=True, inplace=True)
        mask = (dfr['individualid'] == dfr['individualid'].shift()) & (dfr['tripid'] == dfr['tripid'].shift() + 1)
        dfr['trippurpfrom_b01id'] = np.where(mask, dfr['trippurpto_b01id'].shift(), dfr['trippurpfrom_b01id'])
        dfr['triporigcounty_b01id'] = np.where(mask, dfr['tripdestcounty_b01id'].shift(), dfr['triporigcounty_b01id'])
        dfr['triporiggor_b02id'] = np.where(mask, dfr['tripdestgor_b02id'].shift(), dfr['triporiggor_b02id'])

        # infill tripstart_b01id = tripend_b01id - 100 * int(triptravtime / 60)
        fun.log_stderr(' .. infill [tripstart_b01id]')
        if self.cfg.nts_dtype is str:
            dfr['fill'] = dfr['tripend_b01id'].apply(lambda x: int(x.split('-')[0]))
            dfr['fill'] = self._fill_tripstart(dfr['fill'], dfr['triptravtime'])
            dfr['fill'] = dfr['fill'].apply(lambda x: f'{str(x).zfill(4)} - {str(x + 59).zfill(4)}')
        else:
            dfr['fill'] = 100 * (dfr['tripend_b01id'] - 1)  # tripend hour
            dfr['fill'] = self._fill_tripstart(dfr['fill'], dfr['triptravtime'])
            dfr['fill'] = (dfr['fill'] / 100 + 1).astype(int)
        mask = (dfr['tripstart_b01id'].isin(['0', 0])) & (~dfr['tripend_b01id'].isin(['0', 0]))
        dfr['tripstart_b01id'] = np.where(mask, dfr['fill'], dfr['tripstart_b01id'])

        # address invalid records in the database
        fun.log_stderr(f' .. address invalid records')
        col_base, col_target = 'hholdoslaua_b01id', 'hholdcounty_b01id'
        sec_dict = dfr.loc[self._mask_invalid(dfr, col_target)]
        sec_dict = sec_dict.groupby([col_base, col_target])['trips'].sum().reset_index()
        sec_dict = sec_dict.loc[sec_dict.groupby([col_base])['trips'].agg(pd.Series.idxmax)]
        sec_dict = sec_dict.set_index(col_base).to_dict()[col_target]

        dfr['fill'] = dfr[col_base].apply(lambda x: sec_dict[x])
        dfr[col_target] = self._fill_record(dfr, col_target, 'fill')

        # address invalid hhold/trip{o/d}_ua{2009/1998}/areatype{1/2}_b01id
        dfr['hholdua_b01id'] = self._fill_record(dfr, 'hholdua2009_b01id', 'hholdua1998_b01id')
        dfr['hholdareatype_b01id'] = self._fill_record(dfr, 'hholdareatype2_b01id', 'hholdareatype1_b01id')
        for col in ['orig', 'dest']:
            dfr[f'trip{col}ua_b01id'] = self._fill_record(dfr, f'trip{col}ua2009_b01id', f'trip{col}ua1998_b01id')
            dfr[f'trip{col}areatype_b01id'] = self._fill_record(dfr, f'trip{col}areatype2_b01id',
                                                                f'trip{col}areatype1_b01id')

        # populate tour
        def fr_home(x):
            return 1 if self.cfg.nts_dtype is str and x.lower() == 'home' else 1 if x == 23 else 0

        fun.log_stderr(' .. create activity group')
        dfr.sort_values(['individualid', 'tripid'], ascending=True, ignore_index=True, inplace=True)
        dfr['tour_group'] = dfr['trippurpfrom_b01id'].apply(lambda x: fr_home(x))
        dfr['tour_group'] = dfr.groupby('individualid')['tour_group'].cumsum()

        return dfr.drop(columns='fill')

    def _apply_lookups(self, dfr: pd.DataFrame) -> pd.DataFrame:
        # apply lookups
        fun.log_stderr(f'\nApply lookup tables')
        dfr['mode'] = self._lookup(dfr, self.luk.mode())
        dfr['occupant'] = self._lookup(dfr, self.luk.occupant())
        dfr['period'] = self._lookup(dfr, self.luk.time_start())
        dfr['purpose'] = self._lookup(dfr, self.luk.purpose())
        dfr['direction'] = self._lookup(dfr, self.luk.direction())
        if self.def_atype == 'ruc2011':
            dfr['tfn_at'] = self._lookup(dfr, self.luk.settlement())
            dfr['tfn_at_o'], dfr['tfn_at_d'] = self._infill_ruc(dfr, 'county')
        else:
            dfr['tfn_at'] = self._lookup(dfr, self.luk.ntem_at('hholdareatype_b01id'))
            dfr['tfn_at_o'] = self._lookup(dfr, self.luk.ntem_at('triporigareatype_b01id'))
            dfr['tfn_at_d'] = self._lookup(dfr, self.luk.ntem_at('tripdestareatype_b01id'))
        dfr['aws'] = self._lookup(dfr, self.luk.aws())
        dfr['gender'] = self._lookup(dfr, self.luk.gender())
        dfr['hh_type'] = self._lookup(dfr, self.luk.hh_type())
        dfr['soc'] = self._lookup(dfr, self.luk.x_soc())
        dfr['ns'] = self._lookup(dfr, self.luk.ns_sec())
        # issue with aws = fte/pte but ns-sec = unemployed, set ns-sec = not classified
        dfr['ns'] = np.where((dfr['ns'] == 4) & (dfr['aws'].isin([2, 3])), 5, dfr['ns'])
        # create traveller type
        dfr['tt'] = self._traveller_type(dfr, self.def_ttype)

        return dfr

    def _traveller_type(self, dfr: pd.DataFrame, col_list: List) -> pd.Series:
        # TODO: to be finalised once tfn_tt is decided
        fun.log_stderr(f' .. {col_list} -> traveller_type')
        dct = dfr.groupby(col_list)[['trips']].sum().reset_index()
        dct['trips'] = 1
        dct.loc[(dct == 0).any(axis=1), 'trips'] = 0
        dct = dct.set_index(['trips'] + col_list).sort_index().reset_index()
        dct['trips'] = dct['trips'].cumsum()
        dct = dct.set_index(col_list).to_dict()['trips']
        dct.update(self.luk.tfn_tt(col_list))  # update tfn_tt
        out = fun.dfr_to_tuple(dfr, col_list)
        return out.apply(lambda x: dct[x])

    def _mask_invalid(self, dfr: pd.DataFrame, col_name: str) -> pd.Series:
        # exclude invalid records for masking purpose
        return (~dfr[col_name].str.lower().isin(['dead', 'dna', '0', 0]) if self.cfg.nts_dtype in [str, object]
                else ~dfr[col_name].isin([-8, -9, -10, 0]))

    def _infill_ruc(self, dfr: pd.DataFrame, level: str = 'county') -> List:
        # use household data to derive ruc2011 based on [county, area_type]
        fun.log_stderr(f' .. infill ruc_2011 - {level} level')
        col_used = [f'hhold{level}_b01id', 'hholdareatype_b01id']
        col_type = self.cfg.nts_dtype
        dct = self._write_stats(dfr, col_used + ['tfn_at']).reset_index()
        dct = dct.loc[self._mask_invalid(dfr, col_used[1])]
        dct = dct.loc[dct.groupby(col_used)['trips'].agg(pd.Series.idxmax)]

        # create dictionary {[county, area_type]: ruc}
        for col in col_used:
            dct[col] = dct[col].str.lower() if col_type is str else dct[col]
        dct = dct.set_index(col_used).to_dict()['tfn_at']

        # apply ruc to trip data [county, area_type]
        out_list: List = []
        for odx in ['orig', 'dest']:
            dir_type = 'hb_fr' if odx == 'orig' else 'hb_to'
            col_used = [f'trip{odx}{level}_b01id', f'trip{odx}areatype_b01id']
            ruc = dfr[col_used].copy()
            for col in col_used:
                ruc[col] = ruc[col].str.lower() if col_type is str else ruc[col]
            ruc = fun.dfr_to_tuple(ruc, col_used)
            ruc = ruc.apply(lambda x: dct.get(x, 0))

            # final result
            col_hold, col_trip = f'hhold{level}_b01id', f'trip{odx}{level}_b01id'
            ruc = np.where((dfr['direction'] == dir_type) & (~dfr['tfn_at'].isin([0])) &
                           (dfr[col_hold].str.lower() == dfr[col_trip].str.lower() if col_type is str
                            else dfr[col_hold] == dfr[col_trip]), dfr['tfn_at'], ruc)
            out_list.append(ruc)
        return out_list

    @staticmethod
    def _lookup(dfr: pd.DataFrame, col_dict: Dict) -> Union[pd.DataFrame, pd.Series]:
        col_name = fun.str_to_list(col_dict['col'])
        col_type = fun.str_to_list(col_dict['typ'])
        fun.log_stderr(f' .. {col_name} -> {col_dict["out"]}')
        dfr = dfr[col_name].copy()
        for idx, col in enumerate(col_name):
            dfr[col] = dfr[col].str.lower() if col_type[idx] == str else dfr[col].astype(col_type[idx])
        dfr = fun.dfr_to_tuple(dfr, col_name)
        dfr = dfr.apply(lambda x: col_dict['val'].get(x, 0))
        return dfr

    def _fill_record(self, dfr: pd.DataFrame, col_update: str, col_from: str) -> np.ndarray:
        # update [col_update] by infilling data from [col_from] if invalid data
        rec = np.where((dfr[col_update].str.lower().isin(['dead', 'dna', '0']) if self._is_object(dfr[col_update])
                        else dfr[col_update].isin([-8, -9, -10, 0])), dfr[col_from], dfr[col_update])
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
    def _merge_nhb(dfr: pd.DataFrame, col_type: type = int) -> np.ndarray:
        # TODO: to be updated once finalised
        return np.where(dfr['direction'] == 'nhb', np.where(dfr['purpose'] == 7, 8, dfr['purpose']), dfr['purpose'])

    def _summary(self, dfr: pd.DataFrame):
        fun.log_stderr(f'\nSummary (classified)')
        self._write_stats(dfr, ['surveyyear'], 'NTS_cb_year')
        self._write_stats(dfr, ['hholdgor_b02id'], 'NTS_cb_gor')
        self._write_stats(dfr, ['hholdcounty_b01id'], 'NTS_cb_county')
        self._write_stats(dfr, ['tfn_at', 'settlement2011ew_b01id'], f'NTS_cb_{self.def_atype}')
        self._write_stats(dfr, ['mode', 'mainmode_b11id'], 'NTS_cb_mode')
        self._write_stats(dfr, ['purpose', 'direction', 'trippurpfrom_b01id', 'trippurpto_b01id'], 'NTS_cb_purpose')
        self._write_stats(dfr, ['period', 'tripstart_b01id'], 'NTS_cb_period')
        self._write_stats(dfr, ['gender', 'sex_b01id', 'age_b01id'], 'NTS_cb_gender')
        self._write_stats(dfr, ['aws', 'gender', 'ecostat_b01id'], 'NTS_cb_aws')
        self._write_stats(dfr, ['hh_type', 'hholdnumadults', 'numcarvan'], 'NTS_cb_hhold')
        self._write_stats(dfr, ['soc', 'xsoc2000_b02id', 'age_b01id', 'ecostat_b01id'], 'NTS_cb_soc')
        self._write_stats(dfr, ['ns', 'nssec_b03id'], 'NTS_cb_ns_sec')
        self._write_stats(dfr, ['tt', 'aws', 'gender', 'hh_type', 'soc', 'ns'], 'NTS_cb_trips_by_tt')

    # swap orig/dest for return_home trips
    @staticmethod
    def _transpose(dfr: pd.DataFrame, col_swap: List) -> pd.DataFrame:
        col_orig, col_dest = dfr[col_swap[0]], dfr[col_swap[1]]
        dfr.loc[dfr['trip_direction'] == 'hb_to', col_swap[0]] = col_dest
        dfr.loc[dfr['trip_direction'] == 'hb_to', col_swap[1]] = col_orig
        return dfr

    def _write_stats(self, dfr: pd.DataFrame, col_grby: Union[List, str], csv_name: str = None) -> pd.DataFrame:
        col_grby = [col.lower() for col in fun.str_to_list(col_grby)]
        fun.log_stderr(f' .. write {col_grby} -> {csv_name}') if csv_name is not None else None
        out = dfr.groupby(col_grby)['trips'].sum()
        out_fldr = f'{self.cfg.fld_build}\\{self.cfg.fld_report}'
        fun.dfr_to_csv(out, out_fldr, csv_name) if csv_name is not None else None
        return out
