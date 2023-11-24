from mdlfunction import mkdir


class Config:
    def __init__(self, out_fldr: str):
        # specs
        self.def_years = [yr for yr in range(2002, 2022)]
        self.tfn_atype = 'tfn'  # ntem (8), ruc2011 (5), or tfn (17)
        self.def_ttype = 'tfn'  # ntem (88) or tfn (632)
        self.tfn_ttype = (['gender', 'aws', 'hh_type'] if self.def_ttype == 'ntem' else
                          ['gender', 'aws', 'hh_type', 'soc', 'ns'])
        self.tfn_modes = [1, 2, 3, 5, 6, 7]  # 1-walk, 2-cycle, 3-car, 4-lgv, 5-bus, 6-surface rail, 7-light rail
        self.m2k_fact = 1.6093
        self.nts_dtype = int
        self.csv_cbuild = 'cb_tfn'

        # sub-folders
        self.dir_cbuild = f'{out_fldr}\\classified builds'
        self.dir_output = f'{out_fldr}\\outputs'
        self.fld_dbase = 'database'
        self.fld_report = 'reports'
        self.fld_lookup = 'lookups'
        self.fld_notem = 'notem'
        self.fld_tlds = 'tld'
        self.fld_hbase = 'hb'
        self.fld_rates = 'trip_rates'
        self.fld_split = 'mode_time_splits'
        self.fld_phis = 'phi_factors'
        self.fld_nhbase = 'nhb'
        self.fld_occs = 'occs'
        self.fld_graph = 'graphs'
        self.fld_stage = 'stage'

        # create directory
        self._setup_directory()

    def _setup_directory(self):
        # create sub-folders
        mkdir(f'{self.dir_cbuild}\\{self.fld_report}')
        mkdir(f'{self.dir_cbuild}\\{self.fld_lookup}')
        mkdir(f'{self.dir_output}\\{self.fld_dbase}')
        mkdir(f'{self.dir_output}\\{self.fld_notem}')
        mkdir(f'{self.dir_output}\\{self.fld_occs}')
        mkdir(f'{self.dir_output}\\{self.fld_nhbase}')
        mkdir(f'{self.dir_output}\\{self.fld_tlds}')
        mkdir(f'{self.dir_output}\\{self.fld_stage}')
        mkdir(f'{self.dir_output}\\{self.fld_hbase}\\{self.fld_graph}')
        mkdir(f'{self.dir_output}\\{self.fld_hbase}\\{self.fld_phis}')
        mkdir(f'{self.dir_output}\\{self.fld_hbase}\\{self.fld_rates}')
        mkdir(f'{self.dir_output}\\{self.fld_hbase}\\{self.fld_split}')
