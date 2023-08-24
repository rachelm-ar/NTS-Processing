import os

NTEM_AT = 'ntem'
RUC2011 = 'ruc2011'


class Config:
    def __init__(self, out_fldr: str):
        # specs
        self.yrs_included = [yr for yr in range(2002, 2020)]
        self.def_ttype = ['aws', 'gender', 'hh_type']  # , 'soc', 'ns']
        self.m2k_fact = 1.6093
        self.nts_dtype = int
        self.def_atype = NTEM_AT  # ntem or ruc2011
        self.csv_build = 'cb_tfn'

        # sub-folders
        self.fld_build = f'{out_fldr}\\classified builds'
        self.fld_output = f'{out_fldr}\\outputs'
        self.fld_dbase = 'hb_trip_rates'
        self.fld_report = 'reports'
        self.fld_notem = 'notem'
        self.fld_tlds = 'tld'
        self.fld_hbase = 'hb'
        self.fld_phis = 'phi_factors'
        self.fld_nhbase = 'nhb'
        self.fld_occs = 'occs'

        # create directory
        self._setup_directory()

    def _setup_directory(self):
        # create sub-folders
        os.makedirs(f'{self.fld_build}\\{self.fld_report}', exist_ok=True)
        os.makedirs(f'{self.fld_build}\\{self.fld_dbase}', exist_ok=True)
        os.makedirs(f'{self.fld_output}\\{self.fld_notem}', exist_ok=True)
        os.makedirs(f'{self.fld_output}\\{self.fld_occs}', exist_ok=True)
        os.makedirs(f'{self.fld_output}\\{self.fld_nhbase}', exist_ok=True)
        os.makedirs(f'{self.fld_output}\\{self.fld_tlds}', exist_ok=True)
        os.makedirs(f'{self.fld_output}\\{self.fld_hbase}', exist_ok=True)
        os.makedirs(f'{self.fld_output}\\{self.fld_hbase}\\{self.fld_phis}', exist_ok=True)
