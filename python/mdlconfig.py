import os
from mdlfunction import mkdir
from pathlib import Path
os.environ['R_HOME'] = r'C:\Program Files\R\R-4.3.2'


class Config:
    def __init__(self, out_fldr: str):
        # specs
        self.nts_dtype = int
        self.def_years = [yr for yr in range(2002, 9999)]
        self.tfn_atype = 'tfn'  # ntem (8), ruc2011 (5), or tfn (20)
        self.def_ttype = 'tfn'  # ntem (88) or tfn (712)
        self.tfn_ttype = (['gender', 'aws', 'hh_type'] if self.def_ttype == 'ntem' else
                          ['gender', 'aws', 'hh_type', 'soc', 'ns'])
        # mode: 1-walk, 2-cycle, 3-car, 4-lgv, 5-bus, 6-surface rail, 7-light rail
        self.tfn_modes = [1, 2, 3, 4, 5, 6, 7, 8]
        self.m2k_fact = 1.6093
        self.csv_cbuild = 'cb_tfn'
        self.out_fldr = Path(out_fldr)

        # main directory
        self.dir_import = self.out_fldr / 'imports'
        self.dir_cbuild = self.out_fldr / 'builds'
        self.dir_output = self.out_fldr / 'outputs'

        # sub-folders
        self.fld_dbase = 'database'
        self.fld_report = 'reports'
        self.fld_lookup = 'lookups'
        self.fld_prod = 'productions'
        self.fld_attr = 'attractions'
        self.fld_tour = 'tour'
        self.fld_tlds = 'tld'
        self.fld_hbase = 'hb'
        self.fld_rates = 'trip_rates'
        self.fld_split = 'mode_time_splits'
        self.fld_phis = 'phi_factors'
        self.fld_nhbase = 'nhb'
        self.fld_occs = 'occs'
        self.fld_graph = 'graphs'
        self.fld_stage = 'stage'
        self.fld_other = 'others'

        # create directory
        self._setup_directory()

    def _setup_directory(self):
        # classified build
        mkdir(self.dir_cbuild / self.fld_report)
        # lookups
        mkdir(self.dir_cbuild / self.fld_lookup)
        # production
        mkdir(self.dir_output / self.fld_prod / self.fld_dbase)
        mkdir(self.dir_output / self.fld_prod / self.fld_hbase / self.fld_graph)
        mkdir(self.dir_output / self.fld_prod / self.fld_hbase / self.fld_phis)
        mkdir(self.dir_output / self.fld_prod / self.fld_hbase / self.fld_rates)
        mkdir(self.dir_output / self.fld_prod / self.fld_hbase / self.fld_split)
        mkdir(self.dir_output / self.fld_prod / self.fld_nhbase)
        # attraction
        mkdir(self.dir_output / self.fld_attr)
        # tour model
        mkdir(self.dir_output / self.fld_tour / self.fld_report)
        # occupancy
        mkdir(self.dir_output / self.fld_occs)
        # tlds
        mkdir(self.dir_output / self.fld_tlds)
        # stage
        mkdir(self.dir_output / self.fld_stage)
        # others
        mkdir(self.dir_output / self.fld_other)
