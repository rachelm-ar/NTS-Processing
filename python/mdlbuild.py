from typing import Union, List, Dict
import multiprocessing as mp
import mdlfunction as fun
import mdllookup as luk
import mdlconfig as cfg
import pandas as pd
import numpy as np
import os

mp.freeze_support()


class ClassifiedBuild:
    """ produces NTS classified build for the subsequent NTS processes.
        note:
        1. w1 - household/dwelling unit selection -> xDU*wHH
        2. w2 - household-level non-participation -> for trip-rates
        3. w3 - account for "not every individual completed the interview"
        4. w5xhh = trip weight, w5/w2 for weekday and 1 for weekend
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

    def __init__(self, nts_fldr: str, cbo_fldr: str, cb_version: Union[str, int], over_write: bool = False):
        fun.log_stderr('\n***** NTS CLASSIFIED BUILD *****')
        # input specs
        self.cur_path = os.path.dirname(os.path.realpath(__file__))
        self.num_cpus = max(os.cpu_count() - 2, 1)
        self.err_indx = True

        # nts-specific specs
        nts_name = 'special_2002-2021_protect'
        xls_name = nts_name.replace('-', '_to_').replace('_protect', '')
        self.nts_fldr, self.out_fldr = nts_fldr, cbo_fldr
        self.xls_spec = f'7553_nts_lookup_table_response_levels_{xls_name}.xlsx'
        self.nts_file = {'trips': {'household': f'household_{nts_name}.tab',
                                   'individual': f'individual_{nts_name}.tab',
                                   'day': f'day_{nts_name}.tab',
                                   'trip': f'trip_{nts_name}.tab'},
                         'stage': {'stage': f'stage_{nts_name}.tab',
                                   'vehicle': f'vehicle_{nts_name}.tab',
                                   'ticket': f'ticket_{nts_name}.tab'}
                         }
        self.col_incl = {'household': ['SurveyYear', 'HouseholdID', 'HHoldOSLAUA_B01ID', 'HHoldCounty_B01ID',
                                       'HHoldGOR_B02ID', 'Settlement2011EW_B01ID', 'HHoldAreaType1_B01ID',
                                       'HHoldAreaType2_B01ID', 'HHoldUA1998_B01ID', 'HHoldUA2009_B01ID',
                                       'HHoldNumAdults', 'HHoldNumChildren', 'NumCarVan', 'HHIncome2002_B01ID',
                                       'W1', 'W2', 'W3'],
                         'individual': ['HouseholdID', 'IndividualID', 'Age_B01ID', 'Sex_B01ID', 'EcoStat_B01ID',
                                        'SIC1992_B02ID', 'SIC2007_B02ID', 'XSOC2000_B02ID', 'NSSEC_B03ID',
                                        'IndIncome2002_B01ID', 'HRPRelation_B01ID', 'OftHome_B01ID'],
                         'day': ['HouseholdID', 'IndividualID', 'DayID', 'TravelWeekday_B01ID'],
                         'trip': ['HouseholdID', 'IndividualID', 'DayID', 'TripID', 'MainMode_B11ID',
                                  'TripPurpFrom_B01ID', 'TripPurpTo_B01ID', 'TripStart_B01ID', 'TripEnd_B01ID',
                                  'TripDisIncSW', 'TripTravTime', 'TripOrigCounty_B01ID', 'TripOrigGOR_B02ID',
                                  'TripDestCounty_B01ID', 'TripDestGOR_B02ID', 'TripOrigAreaType1_B01ID',
                                  'TripDestAreaType1_B01ID', 'TripOrigAreaType2_B01ID', 'TripDestAreaType2_B01ID',
                                  'TripOrigUA1998_B01ID', 'TripOrigUA2009_B01ID', 'TripDestUA1998_B01ID',
                                  'TripDestUA2009_B01ID', 'W5', 'W5xHH', 'JJXSC', 'TripPurpose_B01ID'],
                         'stage': ['HouseholdID', 'IndividualID', 'DayID', 'TripID', 'StageID', 'VehicleID',
                                   'IndTicketID', 'StageSeq', 'StageMode_B11ID', 'StageDistance', 'StageTime',
                                   'StageFareCost', 'StageCost'],
                         'vehicle': ['HouseholdID', 'IndividualID', 'VehicleID', 'VehMainDriv_B01ID', 'VehType_B01ID',
                                     'VehType_B03ID', 'VehAge_B01ID', 'VehPropType_B01ID', 'VehAnMileage_B01ID',
                                     'CompanyCar_B01ID', 'VehParkLoc_B01ID', 'VehPropTypeN_B01ID'],
                         'ticket': ['HouseholdID', 'IndividualID', 'IndTicketID', 'TicketMode_B01ID',
                                    'TicketPeriod_B01ID', 'TicketCost', 'TicketUseDiary',
                                    'TicketTravelWeekDistance', 'TicketTripCost'],
                         }
        # columns to join trip table to each of the other tables in order
        self.col_join = {'household': 'HouseholdID',
                         'individual': ['HouseholdID', 'IndividualID'],
                         'day': ['HouseholdID', 'IndividualID', 'DayID'],
                         'stage': ['HouseholdID', 'IndividualID', 'DayID', 'TripID'],
                         'vehicle': ['HouseholdID', 'IndividualID', 'VehicleID'],
                         'ticket': ['HouseholdID', 'IndividualID', 'IndTicketID'],
                         }

        # nts specs
        self.cfg = cfg.Config(cbo_fldr)
        self.nts_dtype, def_years = self.cfg.nts_dtype, self.cfg.def_years
        self.tfn_ttype, self.tfn_atype = self.cfg.tfn_ttype, self.cfg.tfn_atype
        self.m2k_fact, self.tfn_modes = self.cfg.m2k_fact, self.cfg.tfn_modes
        self.luk = luk.Lookup(self.nts_dtype)
        self.cb_version = cb_version

        # import & pre-process
        if over_write:
            self._exist()
            self.nts_spec = self._import_spec()
            self._write_lookups()
            nts_data = self._import_data('trips', pd.DataFrame(), def_years)
            nts_data = self._preprocess(nts_data)
            nts_data = self._apply_lookups(nts_data)
            self._summary(nts_data)
            self._write_cb(nts_data, f'{self.cfg.csv_cbuild}_v{self.cb_version}')

            # write affordability c.build to csv
            nts_data = self._import_data('stage', nts_data)
            nts_data = self._preprocess_stage(nts_data)
            self._write_cb(nts_data, f'{self.cfg.csv_cbuild}_stage_v{self.cb_version}')
        else:
            fun.log_stderr(f' .. skipped!')

    def _exist(self):
        fun.log_stderr('\nCheck file existence')
        self.err_indx = fun.exist_path(self.nts_fldr, self.err_indx)
        self.err_indx = fun.exist_file(f'{self.nts_fldr}\\mrdoc\\excel\\{self.xls_spec}', self.err_indx)
        for key in self.nts_file:
            for col in self.nts_file[key]:
                self.err_indx = fun.exist_file(f'{self.nts_fldr}\\tab\\{self.nts_file[key][col]}', self.err_indx)
        os.makedirs(self.out_fldr, exist_ok=True)
        fun.log_stderr(' .. specified files OK') if self.err_indx else None
        exit() if not self.err_indx else None

    def _import_spec(self) -> pd.DataFrame:
        dfr = pd.read_excel(f'{self.nts_fldr}\\mrdoc\\excel\\{self.xls_spec}', engine='openpyxl')
        dfr.rename(columns={col: col.strip().lower() for col in dfr.columns}, inplace=True)
        for col in ['table', 'variable']:
            dfr[col] = dfr[col].str.lower().str.strip()
        return dfr

    def _import_data(self, nts_spec: str, nts_trip: pd.DataFrame = pd.DataFrame(), yrs_incl: List = None
                     ) -> pd.DataFrame:
        """ import NTS data:
            1. import trip-related database, nts_spec = trip, merge household, individual, day to trip table
            2. import stage-related database, nts_spec = stage, nts_trip = trip database from (1),
                merge stage, vehicle, ticket to trip table
        """
        fun.log_stderr(f'\nImport NTS data - {nts_spec} database')
        dct: Union[Dict, pd.DataFrame] = {}
        pool, nts_spec = mp.Pool(self.num_cpus), nts_spec.lower().strip()
        for key in self.nts_file[nts_spec]:
            csv_file = f'{self.nts_fldr}\\tab\\{self.nts_file[nts_spec][key]}'
            dct[key] = pool.apply_async(fun.csv_to_dfr, [csv_file, self.col_incl[key], self.cfg.nts_dtype])
        pool.close()
        pool.join()
        dct = {key: dct[key].get() for key in dct}
        self.err_indx = False if any([dct[key] is False for key in dct]) else self.err_indx
        exit() if not self.err_indx else None

        # merge data
        dfr = dct['trip'] if 'trip' in dct else nts_trip
        for key in [col for col in dct if col not in (['trip'] if nts_spec == 'trips' else [])]:
            col_join = self.col_join[key]
            col_join = [col.lower() for col in ([col_join] if not isinstance(col_join, list) else col_join)]
            dfr = pd.merge(dfr, dct[key], how='left', on=col_join, suffixes=('', '')).fillna(0)
        return dfr if yrs_incl is None else dfr.loc[dfr['surveyyear'].isin(yrs_incl)]

    def _preprocess(self, dfr: pd.DataFrame) -> pd.DataFrame:
        # pre-processing
        fun.log_stderr('\nPre-processing')
        col_type = ['w2', 'w5', 'w5xhh', 'tripdisincsw', 'triptravtime']
        fun.log_stderr(f' .. {col_type} -> float')
        dfr[col_type] = dfr[col_type].astype('float64')
        # weighted trip & distance to km
        dfr['trips'] = dfr['w2'].mul(dfr['w5xhh']).mul(dfr['jjxsc']).fillna(0)
        dfr['trav_dist'] = dfr['tripdisincsw'] * self.m2k_fact
        # sort database
        col_sort = ['surveyyear', 'householdid', 'individualid', 'dayid', 'tripid']
        dfr.sort_values(col_sort, ascending=True, ignore_index=True, inplace=True)

        # address disconnected purposes, update [_from] from the preceding [_to] leg
        fun.log_stderr(' .. correct trip sequences')
        mask = (dfr['individualid'] == dfr['individualid'].shift()) & (dfr['tripid'] == dfr['tripid'].shift() + 1)
        dfr.loc[mask, 'trippurpfrom_b01id'] = dfr['trippurpto_b01id'].shift(fill_value=0)
        dfr.loc[mask, 'triporigcounty_b01id'] = dfr['tripdestcounty_b01id'].shift(fill_value=0)
        dfr.loc[mask, 'triporiggor_b02id'] = dfr['tripdestgor_b02id'].shift(fill_value=0)

        # infill tripstart_b01id = tripend_b01id - 100 * int(triptravtime / 60)
        fun.log_stderr(' .. infill [tripstart_b01id]')
        if self.nts_dtype is str:
            dfr['fill'] = dfr['tripend_b01id'].apply(lambda x: int(x.split('-')[0]))
            dfr['fill'] = self._fill_tripstart(dfr['fill'], dfr['triptravtime'])
            dfr['fill'] = dfr['fill'].apply(lambda x: f'{str(x).zfill(4)} - {str(x + 59).zfill(4)}')
        else:
            dfr['fill'] = 100 * (dfr['tripend_b01id'] - 1)  # tripend hour
            dfr['fill'] = self._fill_tripstart(dfr['fill'], dfr['triptravtime'])
            dfr['fill'] = (dfr['fill'] / 100 + 1).astype(int)
        mask = ['0', '-8', -8, 0]
        mask = (dfr['tripstart_b01id'].isin(mask)) & (~dfr['tripend_b01id'].isin(mask))
        dfr.loc[mask, 'tripstart_b01id'] = dfr['fill']

        # address invalid records in the database
        fun.log_stderr(f' .. address invalid records')
        col_base, col_target = 'hholdoslaua_b01id', 'hholdcounty_b01id'
        sec_dict = dfr.loc[self._mask_invalid(dfr, col_target)]
        sec_dict = sec_dict.groupby([col_base, col_target])['trips'].sum().reset_index()
        sec_dict = sec_dict.loc[sec_dict.groupby([col_base])['trips'].agg(pd.Series.idxmax)]
        sec_dict = sec_dict.set_index(col_base).to_dict()[col_target]

        dfr['fill'] = dfr[col_base].apply(lambda x: sec_dict[x])
        dfr[col_target] = self._fill_record(dfr, col_target, 'fill')

        # address invalid hhold/trip{o/d}_ua{2009/1998}/areatype{1/2}_b01id (same specs)
        dfr['hholdua_b01id'] = self._fill_record(dfr, 'hholdua2009_b01id', 'hholdua1998_b01id')
        dfr['hholdareatype_b01id'] = self._fill_record(dfr, 'hholdareatype2_b01id', 'hholdareatype1_b01id')
        for col in ['orig', 'dest']:
            dfr[f'trip{col}ua_b01id'] = self._fill_record(dfr, f'trip{col}ua2009_b01id', f'trip{col}ua1998_b01id')
            dfr[f'trip{col}areatype_b01id'] = self._fill_record(dfr, f'trip{col}areatype2_b01id',
                                                                f'trip{col}areatype1_b01id')

        # infer missing first/last trips if not from/to home
        # if first trip not from home, infer from home using the 23-
        # populate tour
        def fr_home(x):
            return 1 if (self.nts_dtype is str and x.lower() == 'home') else 1 if x == 23 else 0

        fun.log_stderr(' .. create activity groups')
        dfr['tour_group'] = dfr['trippurpfrom_b01id'].apply(lambda x: fr_home(x))
        dfr['tour_group'] = dfr.groupby('individualid')['tour_group'].cumsum()

        return dfr.drop(columns='fill')

    def _apply_lookups(self, dfr: pd.DataFrame) -> pd.DataFrame:
        # apply lookups
        fun.log_stderr(f'\nApply lookup tables')
        dfr['mode'] = self._lookup(dfr, self.luk.mode())
        dfr['occupant'] = self._lookup(dfr, self.luk.occupant('main'))
        dfr['period'] = self._lookup(dfr, self.luk.period())
        dfr['purpose'] = self._lookup(dfr, self.luk.purpose())
        dfr['direction'] = self._lookup(dfr, self.luk.direction())
        dfr = self._infill_ruc(dfr)
        dfr = self._infill_gor(dfr)
        if self.tfn_atype == 'ruc2011':
            dfr['tfn_at'] = self._lookup(dfr, self.luk.settlement('hhold'))
            dfr['tfn_at_o'] = self._lookup(dfr, self.luk.settlement('triporig'))
            dfr['tfn_at_d'] = self._lookup(dfr, self.luk.settlement('tripdest'))
        elif self.tfn_atype == 'ntem':
            dfr['tfn_at'] = self._lookup(dfr, self.luk.at_ntem('hhold'))
            dfr['tfn_at_o'] = self._lookup(dfr, self.luk.at_ntem('triporig'))
            dfr['tfn_at_d'] = self._lookup(dfr, self.luk.at_ntem('tripdest'))
        else:
            dfr['tfn_at'] = self._lookup(dfr, self.luk.at_tfn('hhold'))
            dfr['tfn_at_o'] = self._lookup(dfr, self.luk.at_tfn('triporig'))
            dfr['tfn_at_d'] = self._lookup(dfr, self.luk.at_tfn('tripdest'))
        dfr['aws'] = self._lookup(dfr, self.luk.aws())
        dfr['gender'] = self._lookup(dfr, self.luk.gender())
        dfr['hh_type'] = self._lookup(dfr, self.luk.hh_type())
        dfr['hh_child'] = self._lookup(dfr, self.luk.hh_child())
        # ns-sec
        dfr['ns_ind'] = self._lookup(dfr, self.luk.ns_sec())  # individual
        # if aws = 2.fte/3.pte & ns-sec = 4.unemployed -> ns-sec = 5.not classified
        dfr.loc[(dfr['aws'].isin([2, 3])) & (dfr['ns_ind'] == 4), 'ns_ind'] = 5
        # if aws = 4.stu -> ns-sec = 5.not classified
        dfr.loc[(dfr['aws'].isin([4])), 'ns_ind'] = 5
        # aws = 5.unm, some still have ns-sec 1-5
        # soc and adjustment
        dfr['soc'] = self._lookup(dfr, self.luk.soc())  # individual
        dfr.loc[(dfr['soc'] == 0) & (dfr['ns_ind'].isin([1, 2, 3])), 'soc'] = dfr['ns_ind']
        dfr.loc[(dfr['soc'] == 0) & (dfr['ns_ind'] == 5), 'soc'] = 3  # move to low skilled
        dfr.loc[(dfr['soc'] == 3) & (dfr['ns_ind'] == 1), 'ns_ind'] = 2
        # final household ns-sec
        dfr['ns'] = self._lookup(dfr, self.luk.hrp()).mul(dfr['ns_ind'])
        dfr['ns'] = dfr.groupby('householdid')['ns'].transform('max')
        dfr.loc[dfr['ns'] == 0, 'ns'] = dfr.groupby('householdid')['ns_ind'].transform('min')
        # sic & incomes
        dfr['sic'] = self._lookup(dfr, self.luk.sic('2007'))
        dfr['sic'] = np.where(dfr['sic'].isin([0, '0']), self._lookup(dfr, self.luk.sic('1992')), dfr['sic'])
        dfr['income'] = self._lookup(dfr, self.luk.income('ind'))
        dfr['hh_income'] = self._lookup(dfr, self.luk.income('hh'))
        dfr['income_tag'] = self._lookup(dfr, self.luk.income('ind', '02'))
        dfr['wfh'] = self._lookup(dfr, self.luk.wfh('ofthome_b01id'))
        # traveller type
        dfr['tt'] = self._lookup_tt(dfr)

        # for QA purpose
        # a = dfr.loc[(dfr['hh_type'] == 5) & (dfr['soc'] == 3) & (dfr['ns'] == 4)]['householdid'].unique()
        # a = dfr.loc[dfr['householdid'].isin(a)]
        # a.to_csv('test.csv', index=False)
        return dfr

    def _lookup_tt(self, dfr: pd.DataFrame) -> pd.Series:
        def_ttype, tfn_ttype = self.cfg.def_ttype, self.cfg.tfn_ttype
        fun.log_stderr(f' .. {tfn_ttype} -> traveller type')
        dct = self.luk.tt_tfn(tfn_ttype, def_ttype)  # update tfn or ntem _tt
        self.dfr_ttype = self.luk.tt_to_dfr(tfn_ttype, def_ttype)
        dfr_type, col_calc = self.dfr_ttype.copy(), ['tt']
        for col in tfn_ttype:
            val = f'{col}_sec' if col == 'ns' else col
            dct_type = eval(f'self.luk.{val}()')['out']
            dfr_type[f'{col}_desc'] = dfr_type[col].apply(lambda x: dct_type.get(x, x))
            col_calc = col_calc + [col, f'{col}_desc']
        fun.dfr_to_csv(dfr_type[col_calc], f'{self.cfg.dir_cbuild}', f'traveller_type_{def_ttype}', False)
        out = fun.dfr_to_tuple(dfr, tfn_ttype)
        return out.apply(lambda x: dct.get(x, 0))

    def _mask_invalid(self, dfr: pd.DataFrame, col_name: str) -> pd.Series:
        # exclude invalid records for masking purpose
        return (~dfr[col_name].str.lower().isin(['dead', 'dna', '0', 0]) if self.nts_dtype in [str, object]
                else ~dfr[col_name].isin([-8, -9, -10, 0]))

    def _infill_gor(self, dfr: pd.DataFrame) -> pd.DataFrame:
        out_type, col_type = 'hholdgor_b02id', self.nts_dtype
        lev_grby = ['{}county_b01id', '{}ua_b01id']
        for lev, key in enumerate(lev_grby):
            col_used = (lev_grby if lev == 0 else lev_grby[:-lev])
            col_grby = [col.format('hhold') for col in col_used]
            fun.log_stderr(f' .. {col_grby} -> {out_type}')
            dct = self._write_stats(dfr, col_grby + [out_type, 'householdid']).reset_index()
            dct = dct.groupby(col_grby + [out_type])[['householdid']].count().reset_index()
            dct.rename(columns={'householdid': 'trips'}, inplace=True)
            for col in col_grby:
                dct = dct.loc[self._mask_invalid(dct, col)]
            dct = dct.loc[dct.groupby(col_grby)['trips'].agg(pd.Series.idxmax)]

            # create dictionary {(county, ua): gor}
            for col in col_grby:
                dct[col] = dct[col].str.lower() if col_type is str else dct[col]
            dct = dct.set_index(col_grby).to_dict()[out_type]

            # apply gor to trip data [(county, ua) -> gor]
            for odx in ['orig', 'dest']:
                col_grby = [col.format(f'trip{odx}') for col in col_used]
                gor = dfr[col_grby].copy()
                for col in col_grby:
                    gor[col] = gor[col].str.lower() if col_type is str else gor[col]
                gor = fun.dfr_to_tuple(gor, col_grby)
                gor = gor.apply(lambda x: dct.get(x, 0))

                # final output
                mask = dfr[f'trip{odx}gor_b02id'].isin([-8, 0, '0'])
                dfr.loc[mask, f'trip{odx}gor_b02id'] = gor
        return dfr

    def _infill_ruc(self, dfr: pd.DataFrame) -> pd.DataFrame:
        # use household data to derive ruc2011 based on [gor, county, ua, area_type]
        set_type, col_type = 'settlement2011ew_b01id', self.nts_dtype
        dfr['hholdruc2011_b01id'] = dfr[set_type]
        lev_grby = ['{}gor_b02id', '{}county_b01id', '{}ua_b01id']
        for lev, key in enumerate(lev_grby):
            col_used = (lev_grby if lev == 0 else lev_grby[:-lev]) + ['{}areatype_b01id']
            col_grby = [col.format('hhold') for col in col_used]
            fun.log_stderr(f' .. {col_grby} -> {set_type}')
            dct = self._write_stats(dfr, col_grby + [set_type, 'householdid']).reset_index()
            dct = dct.groupby(col_grby + [set_type])[['householdid']].count().reset_index()
            dct.rename(columns={'householdid': 'trips'}, inplace=True)
            for col in col_grby:
                dct = dct.loc[self._mask_invalid(dct, col)]
            dct = dct.loc[dct.groupby(col_grby)['trips'].agg(pd.Series.idxmax)]

            # create dictionary {(gor, county, ua, area_type): ruc}
            for col in col_grby:
                dct[col] = dct[col].str.lower() if col_type is str else dct[col]
            dct = dct.set_index(col_grby).to_dict()[set_type]

            # apply ruc to trip data [(gor, county, ua, area_type) -> ruc]
            for odx in ['orig', 'dest']:
                dir_type = 'hb_fr' if odx == 'orig' else 'hb_to'
                col_grby = [col.format(f'trip{odx}') for col in col_used]
                ruc = dfr[col_grby].copy()
                for col in col_grby:
                    ruc[col] = ruc[col].str.lower() if col_type is str else ruc[col]
                ruc = fun.dfr_to_tuple(ruc, col_grby)
                ruc = ruc.apply(lambda x: dct.get(x, 0))

                # final output
                dfr[f'trip{odx}ruc2011_b01id'] = dfr[set_type]
                mask = (dfr['direction'] != dir_type) & (ruc != 0)
                dfr.loc[mask, f'trip{odx}ruc2011_b01id'] = ruc
        return dfr

    @staticmethod
    def _lookup(dfr: pd.DataFrame, col_dict: Dict) -> Union[pd.DataFrame, pd.Series]:
        col_name = fun.str_to_list(col_dict['col'])
        col_type = fun.str_to_list(col_dict['typ'])
        fun.log_stderr(f' .. {col_name} -> {col_dict["log"]}')
        dfr = dfr[col_name].copy()
        for idx, col in enumerate(col_name):
            dfr[col] = (dfr[col].str.lower() if col_type[idx] == str else
                        np.where(dfr[col].astype(str) == ' ', 0, dfr[col]).astype(col_type[idx]))
        dfr = fun.dfr_to_tuple(dfr, col_name)
        dfr = dfr.apply(lambda x: col_dict['val'].get(x, 0))
        return dfr

    def _fill_record(self, dfr: pd.DataFrame, col_update: str, col_from: str) -> pd.Series:
        # update [col_update] by infilling data from [col_from] if invalid data
        mask = (dfr[col_update].str.lower().isin(['dead', 'dna', '0']) if self._is_object(dfr[col_update])
                else dfr[col_update].isin([-8, -9, -10, 0]))
        # rec = np.where((dfr[col_update].str.lower().isin(['dead', 'dna', '0']) if self._is_object(dfr[col_update])
        #                 else dfr[col_update].isin([-8, -9, -10, 0])), dfr[col_from], dfr[col_update])
        dfr.loc[mask, col_update] = dfr[col_from]
        return dfr[col_update]

    @staticmethod
    def _fill_tripstart(dfr_fill: pd.Series, dfr_time: pd.Series) -> np.ndarray:
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
    def _merge_nhb(dfr: pd.DataFrame) -> np.ndarray:
        # TODO: to be updated once finalised
        return np.where(dfr['direction'] == 'nhb', np.where(dfr['purpose'] == 7, 8, dfr['purpose']), dfr['purpose'])

    # swap orig/dest for return_home trips
    @staticmethod
    def _transpose(dfr: pd.DataFrame, col_swap: List) -> pd.DataFrame:
        col_orig, col_dest = dfr[col_swap[0]], dfr[col_swap[1]]
        dfr.loc[dfr['trip_direction'] == 'hb_to', col_swap[0]] = col_dest
        dfr.loc[dfr['trip_direction'] == 'hb_to', col_swap[1]] = col_orig
        return dfr

    def _summary(self, dfr: pd.DataFrame):
        fun.log_stderr(f'\nSummary (classified)')
        self._write_stats(dfr, ['surveyyear'], 'NTS_cb_year')
        self._write_stats(dfr, ['hholdgor_b02id'], 'NTS_cb_gor')
        self._write_stats(dfr, ['hholdcounty_b01id', 'hholdgor_b02id'], 'NTS_cb_county')
        self._write_stats(dfr, ['triporigcounty_b01id', 'triporiggor_b02id'], 'NTS_cb_county_o')
        self._write_stats(dfr, ['tripdestcounty_b01id', 'tripdestgor_b02id'], 'NTS_cb_county_d')
        self._write_stats(dfr, ['tfn_at', 'hholdgor_b02id', 'hholdcounty_b01id',
                                'hholdareatype_b01id', 'hholdruc2011_b01id'], f'NTS_cb_at_{self.tfn_atype}')
        self._write_stats(dfr, ['tfn_at_o', 'triporiggor_b02id', 'triporigcounty_b01id',
                                'triporigareatype_b01id', 'triporigruc2011_b01id'], f'NTS_cb_at_{self.tfn_atype}_o')
        self._write_stats(dfr, ['tfn_at_d', 'tripdestgor_b02id', 'tripdestcounty_b01id',
                                'tripdestareatype_b01id', 'tripdestruc2011_b01id'], f'NTS_cb_at_{self.tfn_atype}_d')
        self._write_stats(dfr, ['mode', 'mainmode_b11id'], 'NTS_cb_mode')
        self._write_stats(dfr, ['purpose', 'direction', 'trippurpfrom_b01id', 'trippurpto_b01id'],
                          'NTS_cb_purpose')
        self._write_stats(dfr, ['period', 'tripstart_b01id'], 'NTS_cb_period')
        self._write_stats(dfr, ['gender', 'sex_b01id', 'age_b01id'], 'NTS_cb_gender')
        self._write_stats(dfr, ['aws', 'gender', 'ecostat_b01id'], 'NTS_cb_aws')
        self._write_stats(dfr, ['hh_type', 'hholdnumadults', 'numcarvan'], 'NTS_cb_hh_type')
        self._write_stats(dfr, ['soc', 'xsoc2000_b02id', 'age_b01id', 'ecostat_b01id'], 'NTS_cb_soc')
        self._write_stats(dfr, ['ns', 'nssec_b03id'], 'NTS_cb_ns_sec')
        self._write_stats(dfr, ['householdid', 'individualid'] + self.tfn_ttype + ['hh_income', 'income'],
                          'NTS_cb_income')
        self._write_stats(dfr, ['tripdestgor_b02id', 'gender', 'aws', 'sic', 'purpose', 'direction'], 'NTS_cb_sic')
        self._write_stats(dfr, ['hholdua_b01id', 'hholdua1998_b01id', 'hholdua2009_b01id'], 'NTS_cb_hholdua')
        self._write_stats(dfr, ['hholdareatype_b01id', 'hholdareatype1_b01id', 'hholdareatype2_b01id'],
                          'NTS_cb_areatype')
        self._write_stats(dfr, ['triporigareatype_b01id', 'triporigareatype1_b01id', 'triporigareatype2_b01id'],
                          'NTS_cb_areatype_o')
        self._write_stats(dfr, ['tripdestareatype_b01id', 'tripdestareatype1_b01id', 'tripdestareatype2_b01id'],
                          'NTS_cb_areatype_d')
        self._write_stats(dfr, ['mode', 'purpose', 'direction', 'mainmode_b11id', 'trippurpfrom_b01id',
                                'trippurpto_b01id'], 'NTS_cb_mode_purpose_direction')
        self._write_stats(dfr, ['householdid', 'hh_type', 'individualid', 'hrprelation_b01id', 'gender', 'aws',
                                'ns', 'soc', 'ns_ind'], 'NTS_cb_hrp')
        self._write_stats(dfr, ['hh_type', 'hholdnumadults', 'numcarvan', 'hholdnumchildren'],
                          'NTS_cb_hh_type_with_children')
        # write traveller types
        out = self._write_stats(dfr, self.tfn_ttype).reset_index()
        out = pd.merge(out, self.dfr_ttype, how='outer', on=self.tfn_ttype).fillna(0)
        self._write_stats(out, ['tt'] + self.tfn_ttype, 'NTS_cb_traveller_type')

        # write production trips by at/gor/ruc for analysis
        col_grby = ['tfn_at', 'hholdgor_b02id', 'hholdcounty_b01id', 'hholdareatype_b01id', 'settlement2011ew_b01id']
        col_trip = ['mode', 'purpose', 'direction']
        out = dfr.groupby(col_grby + ['individualid', 'w2'] + col_trip)[['trips']].sum().reset_index()
        pop = out.groupby(col_grby + ['individualid', 'direction', 'w2'])[['trips']].sum().reset_index()
        out = out.groupby(col_grby + col_trip)[['trips']].sum()
        out = fun.dfr_complete(out, None, 'purpose')
        out = fun.dfr_complete(out, None, 'mode').reset_index()
        pop = pop.groupby(col_grby + ['direction'])[['w2']].sum().reset_index()
        out = pd.merge(out, pop, how='left', on=col_grby + ['direction'])
        fun.dfr_to_csv(out, f'{self.cfg.dir_cbuild}\\{self.cfg.fld_report}', 'NTS_cb_trip_rates_production', False)

        # write attraction trips by at/sic for analysis
        col_grby = ['tfn_at_d', 'sic']
        col_trip = ['mode', 'purpose', 'direction']
        out = dfr.groupby(col_grby + ['individualid', 'w2'] + col_trip)[['trips']].sum().reset_index()
        pop = out.groupby(col_grby + ['individualid', 'direction', 'w2'])[['trips']].sum().reset_index()
        out = out.groupby(col_grby + col_trip)[['trips']].sum()
        out = fun.dfr_complete(out, None, 'purpose')
        out = fun.dfr_complete(out, None, 'mode').reset_index()
        pop = pop.groupby(col_grby + ['direction'])[['w2']].sum().reset_index()
        out = pd.merge(out, pop, how='left', on=col_grby + ['direction'])
        fun.dfr_to_csv(out, f'{self.cfg.dir_cbuild}\\{self.cfg.fld_report}', 'NTS_cb_trip_rates_attraction', False)

    def _preprocess_stage(self, dfr: pd.DataFrame, agg_to_trip: bool = False) -> pd.DataFrame:
        fun.log_stderr('\nProcess stage database')
        col_list = ['stagedistance', 'stagetime', 'stagefarecost', 'stagecost']
        dfr[col_list] = dfr[col_list].astype('float64')
        if agg_to_trip:
            dfr['ismain'] = (dfr['mainmode_b11id'] == dfr['stagemode_b11id'])
            col_grby = ['surveyyear', 'householdid', 'individualid', 'dayid', 'tripid']
            for col in col_list:
                dfr[f'{col}_main'] = dfr.groupby(col_grby + ['ismain'])[col].transform('sum')
                dfr[f'{col}_sub'] = dfr.groupby(col_grby)[col].transform('sum').sub(dfr[f'{col}_main'])
            dfr['stageseq'] = dfr.groupby(['individualid', 'tripid'])['stageseq'].transform('max')
            dfr = dfr.loc[dfr['ismain']].reset_index(drop=True)  # keep records where stagemode = mainmode
            dfr.sort_values(col_grby + ['stagedistance'], ascending=True, inplace=True)
            dfr = dfr.drop_duplicates(col_grby, keep='last', ignore_index=True)  # retain longest distance
            dfr = dfr.drop(columns=['stageid', 'ismain'] + col_list, errors='ignore')

        # apply lookups
        dfr['stagemode'] = self._lookup(dfr, self.luk.mode('stage'))
        dfr['stageoccupant'] = self._lookup(dfr, self.luk.occupant('stage'))
        # vehproptype is provided for only one of either driver/passenger who share the trip
        dfr['fueltype'] = self._lookup(dfr, self.luk.fuel_type('vehproptype_b01id'))
        dfr.loc[dfr['fueltype'] == -10, 'fueltype'] = self._lookup(dfr, self.luk.fuel_type('vehproptypen_b01id'))
        self._write_stats(dfr, ['stagemode', 'fueltype', 'vehproptype_b01id', 'vehproptypen_b01id'],
                          'NTS_cb_vehfueltype')

        return dfr

    def _write_stats(self, dfr: pd.DataFrame, col_grby: Union[List, str], csv_name: str = None) -> pd.DataFrame:
        col_grby = [col.lower() for col in fun.str_to_list(col_grby)]
        fun.log_stderr(f' .. write {col_grby} -> {csv_name}') if csv_name is not None else None
        out = dfr.groupby(col_grby)[['trips']].sum()
        out_fldr = f'{self.cfg.dir_cbuild}\\{self.cfg.fld_report}'
        fun.dfr_to_csv(out, out_fldr, csv_name) if csv_name is not None else None
        return out

    def _write_cb(self, dfr: pd.DataFrame, cbo_name: str):
        out_fldr = self.cfg.dir_cbuild
        fun.log_stderr('\nWrite cb trip data to csv')
        fun.log_stderr(f' .. write to {out_fldr}\\{cbo_name}')
        fun.dfr_to_csv(dfr, out_fldr, cbo_name, False)

    def _write_lookups(self):
        # output folder
        out_fldr = f'{self.cfg.dir_cbuild}\\{self.cfg.fld_lookup}'

        # local function
        def _lookup(func: str):
            arg, var_list = func.split(':'), self.nts_spec['variable'].unique()
            arg = arg[:1] + [f'("{arg[1]}")' if len(arg) > 1 else '()']
            try:
                luk_eval = eval(f'self.luk.{arg[0]}{arg[1]}')
            except AttributeError:
                luk_eval = {'col': func, 'val': {}}
            luk_eval['col'] = luk_eval['col'] if isinstance(luk_eval['col'], list) else [luk_eval['col']]
            dfr = {key: val for key, val in luk_eval['val'].items()}
            dfr = pd.DataFrame.from_dict(dfr, orient='index').reset_index()
            dfr[luk_eval['col']] = dfr['index'].to_list() if len(luk_eval['col']) > 1 else dfr[['index']]
            dfr = dfr.drop(columns='index').rename(columns={0: arg[0]})
            dfr[f'{arg[0]}_name'] = dfr[arg[0]].apply(lambda x: luk_eval['out'].get(x, x))
            for col in luk_eval['col']:
                if col in var_list:
                    col_x = col
                else:
                    col_x = col.split('_')
                    col_x = (f'{col_x[0]}2_{col_x[1]}' if 'areatype' in col else
                             f'{col_x[0]}2009_{col_x[1]}' if 'ua' in col else
                             f'settlement2011ew_b01id' if 'ruc2011' in col else col)

                dct = self.nts_spec.loc[self.nts_spec['variable'] == col_x].set_index('id').to_dict()['desc']
                if len(luk_eval['val']) > 0:
                    dfr[f'{col}_desc'] = dfr[col].apply(lambda x: dct.get(x, x))
                else:
                    dfr = pd.DataFrame.from_dict(dct, orient='index').reset_index()
                    dfr.rename(columns={'index': col, 0: f'{col}_desc'}, inplace=True)
            fun.dfr_to_csv(dfr, out_fldr, f'luk_{arg[0]}', False)

        # run functions
        _lookup('at_ntem:hhold')
        _lookup('at_tfn:hhold')
        _lookup('hholdua_b01id')
        _lookup('hholdruc2011_b01id')
        _lookup('hholdgor_b02id')
        _lookup('hholdcounty_b01id')
        _lookup('mode')
        _lookup('purpose')
        _lookup('hh_type')
        _lookup('soc')
        _lookup('ns_sec')
