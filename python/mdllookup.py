from typing import Dict, List
import mdlfunction as fun


class Lookup:
    def __init__(self, col_type: type = int):
        self.nts_dtype = col_type
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
            'child': {1: 'less than 1 year', 2: '1 - 2 years', 3: '3 - 4 years',
                      4: '5 - 10 years', 5: '11 - 15 years'},
            'adult': {6: '16 years', 7: '17 years', 8: '18 years', 9: '19 years', 10: '20 years',
                      11: '21 - 25 years', 12: '26 - 29 years', 13: '30 - 39 years', 14: '40 - 49 years',
                      15: '50 - 59 years', 16: '60 - 64 years', 17: '65 - 69 years', 18: '70 - 74 years'},
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
            'dna': {-8: 'na', -9: 'dna', -10: 'dead', 0: '0'}
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

        # ntem area type2
        self.at2_01id = {
            1: {1: 'inner london'},
            2: {2: 'outer london built-up areas'},
            3: {3: 'west midlands built-up areas', 4: 'greater manchester built-up areas',
                5: 'west yorkshire built-up areas', 6: 'glasgow built-up areas', 7: 'liverpool built-up areas',
                8: 'tyneside built-up areas', 9: 'south yorkshire built-up areas'},
            4: {10: 'other urban areas - over 250k population'},
            5: {11: 'other urban areas - 100k to 250k population'},
            6: {12: 'other urban areas - 50k to 100k population', 13: 'other urban areas - 25k to 50k population'},
            7: {14: 'other urban areas - 10k to 25k population', 15: 'other urban areas - 3k to 10k population'},
            8: {16: 'rural'},
            0: {-8: 'na', -9: 'dna'}
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
        self.at2_01id = self.dct_to_specs(self.at2_01id, col_type)

        self.nhb_trip = (self.tpp_01id['com'] + self.tpp_01id['emb'] + self.tpp_01id['edu'] +
                         self.tpp_01id['shp'] + self.tpp_01id['peb'] + self.tpp_01id['soc'] +
                         self.tpp_01id['vis'] + self.tpp_01id['hol'])

    def hh_type(self) -> Dict:
        # household type
        out_dict = {'col': ['hholdnumadults', 'numcarvan'],
                    'typ': [int, int],
                    'out': 'hh_type',
                    'val': {1: [(1, 0)],
                            2: fun.product([1], range(1, 10)),  # 1 adult with 1+ cars
                            3: [(2, 0)],
                            4: [(2, 1)],
                            5: fun.product([2], range(2, 10)),  # 2 adults with 2+ cars
                            6: fun.product(range(3, 11), [0]),
                            7: fun.product(range(3, 11), [1]),
                            8: fun.product(range(3, 11), range(2, 10))  # 3+ adults with 2+ cars
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def aws(self) -> Dict:
        # age, work, status
        eco_01id, age_01id = self.eco_01id, self.age_01id
        eco_over = eco_01id['fte'] + eco_01id['pte'] + eco_01id['stu'] + eco_01id['unm']
        out_dict = {'col': ['age_b01id', 'ecostat_b01id'],
                    'typ': [self.nts_dtype, self.nts_dtype],
                    'out': 'aws',
                    'val': {1: fun.product(age_01id['child'], eco_01id['dna']),  # child
                            2: fun.product(age_01id['adult'], eco_01id['fte']),  # fte
                            3: fun.product(age_01id['adult'], eco_01id['pte']),  # pte
                            4: fun.product(age_01id['adult'], eco_01id['stu']),  # student
                            5: fun.product(age_01id['adult'], eco_01id['unm']),  # unemployed
                            6: fun.product(age_01id['elder'], eco_over + eco_01id['dna']),  # over 75
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
                    'typ': [self.nts_dtype, self.nts_dtype, self.nts_dtype],
                    'out': 'soc',
                    'val': {1: fun.product(soc_02id['hig'], age_01id['adult'], emp_ecos),  # high skill
                            2: fun.product(soc_02id['med'], age_01id['adult'], emp_ecos),  # med skill
                            3: fun.product(soc_02id['low'], age_01id['adult'], emp_ecos),  # low skill
                            4: (fun.product(all_xsoc, all_ages, eco_01id['stu']) +  # student
                                fun.product(all_xsoc, age_01id['adult'], eco_01id['unm']) +  # unemployed
                                fun.product(all_xsoc, age_01id['child'], all_ecos) +  # children
                                fun.product(all_xsoc, age_01id['elder'], all_ecos))  # over 75
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def ns_sec(self) -> Dict:
        # ns-sec
        out_dict = {'col': ['nssec_b03id'],
                    'typ': [self.nts_dtype],
                    'out': 'ns-sec',
                    'val': {1: self.sec_03id['ns1'],  # ns-sec 1
                            2: self.sec_03id['ns2'],  # ns-sec 2
                            3: self.sec_03id['ns3'],  # ns-sec 3
                            4: self.sec_03id['ns4'],  # ns-sec 4
                            5: self.sec_03id['ns5'] + self.sec_03id['dna'],  # ns-sec 5
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def ntem_at(self, col_name: str) -> Dict:
        # ntem area type
        out_dict = {'col': col_name,
                    'typ': self.nts_dtype,
                    'out': 'ntem_at',
                    'val': self.at2_01id}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def tfn_tt(self, col_list: List) -> Dict:
        # tfn traveller type
        def _dct_value(idx: int) -> List:
            try:
                key = col_list[idx]
                return list(set(par_dict[key]['val'].values()))
            except IndexError:
                return [None, None, None, None]

        def _dct_update(*args) -> Dict:
            args = (fun.str_to_list(col) for col in args)
            args = (col for col in args if all(itm is not None for itm in col))
            if len(par_dict) == 0:
                return {idx: val for idx, val in enumerate(fun.product(*args), 1)}
            else:
                idx_init = len(par_dict) + 1
                par_dict.update({idx: val for idx, val in enumerate(fun.product(*args), idx_init)})
                return par_dict

        # tt = [aws, gender, hh_type, soc, ns]
        par_dict = {'aws': self.aws(), 'gender': self.gender(), 'hh_type': self.hh_type(),
                    'soc': self.x_soc(), 'ns': self.ns_sec()}
        aws, gen, hh = _dct_value(0), _dct_value(1), _dct_value(2)
        soc, ns = _dct_value(3), _dct_value(4)

        # child: aws=1, gender=1, hh=1-8, soc=4, ns=5
        par_dict = {}
        par_dict = _dct_update(aws[0], gen[0], hh, soc[-1], ns[-1])
        # fte/pte: aws=2-3, gender=2-3, hh=1-8, soc=1-3, ns=1-3 & 5
        par_dict = _dct_update(aws[1], gen[1:], hh, soc[:3], ns[:3] + [ns[-1]])
        par_dict = _dct_update(aws[2], gen[1:], hh, soc[:3], ns[:3] + [ns[-1]])
        # stu/unm/retired: aws=4-6, gender=2-3, hh=1-8, soc=4, ns=1-5
        par_dict = _dct_update(aws[3], gen[1:], hh, soc[-1], ns)
        par_dict = _dct_update(aws[4], gen[1:], hh, soc[-1], ns)
        par_dict = _dct_update(aws[5], gen[1:], hh, soc[-1], ns)

        return self.val_to_key(par_dict)

    def settlement(self) -> Dict:
        # ruc 2011 to area type
        out_dict = {'col': 'settlement2011ew_b01id',
                    'typ': str,
                    'out': 'ruc_2011',
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
        out_dict = {'col': ['mainmode_b11id'],
                    'typ': [self.nts_dtype],
                    'out': 'main mode',
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
        out_dict = {'col': ['mainmode_b11id'],
                    'typ': [self.nts_dtype],
                    'out': 'occupancy',
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
        out_dict = {'col': ['trippurpfrom_b01id', 'trippurpto_b01id'],
                    'typ': [self.nts_dtype, self.nts_dtype],
                    'out': 'purpose',
                    'val': {1: fun.product(all_trip, tpp_01id['com']) + fun.product(tpp_01id['com'], end_home),
                            2: fun.product(all_trip, tpp_01id['emb']) + fun.product(tpp_01id['emb'], end_home),
                            3: fun.product(all_trip, tpp_01id['edu']) + fun.product(tpp_01id['edu'], end_home),
                            4: fun.product(all_trip, tpp_01id['shp']) + fun.product(tpp_01id['shp'], end_home),
                            5: fun.product(all_trip, tpp_01id['peb']) + fun.product(tpp_01id['peb'], end_home),
                            6: fun.product(all_trip, tpp_01id['soc']) + fun.product(tpp_01id['soc'], end_home),
                            7: fun.product(all_trip, tpp_01id['vis']) + fun.product(tpp_01id['vis'], end_home),
                            8: fun.product(all_trip, tpp_01id['hol']) + fun.product(tpp_01id['hol'], end_home),
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def time_start(self) -> Dict:
        # trip start time
        wkd_01id, ttp_01id = self.wkd_01id, self.ttp_01id
        week_end = ttp_01id['am'] + ttp_01id['ip'] + ttp_01id['pm'] + ttp_01id['op'] + ttp_01id['na']
        out_dict = {'col': ['travelweekday_b01id', 'tripstart_b01id'],
                    'typ': [self.nts_dtype, self.nts_dtype],
                    'out': 'start time',
                    'val': {1: fun.product(wkd_01id['wkd'], ttp_01id['am']),
                            2: fun.product(wkd_01id['wkd'], ttp_01id['ip']),
                            3: fun.product(wkd_01id['wkd'], ttp_01id['pm']),
                            4: fun.product(wkd_01id['wkd'], ttp_01id['op']),
                            5: fun.product(wkd_01id['sat'], week_end),
                            6: fun.product(wkd_01id['sun'], week_end)
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def time_end(self) -> Dict:
        # trip end time
        wkd_01id, ttp_01id = self.wkd_01id, self.ttp_01id
        all_24hr = ttp_01id['am'] + ttp_01id['ip'] + ttp_01id['pm'] + ttp_01id['op'] + ttp_01id['na']
        out_dict = {'col': ['travelweekday_b01id', 'tripend_b01id'],
                    'typ': [self.nts_dtype, self.nts_dtype],
                    'out': 'end time',
                    'val': {1: fun.product(wkd_01id['wkd'], ttp_01id['am']),
                            2: fun.product(wkd_01id['wkd'], ttp_01id['ip']),
                            3: fun.product(wkd_01id['wkd'], ttp_01id['pm']),
                            4: fun.product(wkd_01id['wkd'], ttp_01id['op']),
                            5: fun.product(wkd_01id['sat'], all_24hr),
                            6: fun.product(wkd_01id['sun'], all_24hr)
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def direction(self) -> Dict:
        # direction of travel, hb_fr, hb_to, nhb
        out_dict = {'col': ['trippurpfrom_b01id', 'trippurpto_b01id'],
                    'typ': [self.nts_dtype, self.nts_dtype],
                    'out': 'direction',
                    'val': {'hb_fr': fun.product(self.tpp_01id['hom'], self.nhb_trip),
                            'hb_to': fun.product(self.nhb_trip, self.tpp_01id['hom']),
                            'nhb': fun.product(self.nhb_trip, self.nhb_trip)
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    def gender(self) -> Dict:
        # gender
        age_01id, sex_01id = self.age_01id, self.sex_01id
        out_dict = {'col': ['sex_b01id', 'age_b01id'],
                    'typ': [self.nts_dtype, self.nts_dtype],
                    'out': 'gender',
                    'val': {1: fun.product(sex_01id['male'] + sex_01id['female'], age_01id['child']),  # child
                            2: fun.product(sex_01id['male'], age_01id['adult'] + age_01id['elder']),  # male
                            3: fun.product(sex_01id['female'], age_01id['adult'] + age_01id['elder'])  # female
                            }}
        out_dict['val'] = self.val_to_key(out_dict['val'])
        return out_dict

    @staticmethod
    def val_to_key(dct: Dict) -> Dict:
        dct = {key: [dct[key]] if not isinstance(dct[key], list) else dct[key] for key in dct}
        return {fun.str_lower(val): key for key in dct for val in dct[key]}

    @staticmethod
    def dct_to_specs(dct: Dict, out: type = int) -> Dict:
        return {key: list(dct[key].keys() if out is int else dct[key].values()) for key in dct}
