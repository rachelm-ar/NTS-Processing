import pandas as pd
from typing import Union
import mdlfunction as fun
import statsmodels.api as sm
import statsmodels.formula.api as smf
import mdlconfig


class TripRate:
    """ calculate trip-rates """

    def __init__(self, nts_fldr: str, cb_version: Union[str, int]):
        # read config
        fun.log_stderr('\nTrip rates model')
        self.cfg = mdlconfig.Config(nts_fldr)
        self.cb_version = cb_version

        # read in cb data
        nts_fldr = f'{self.cfg.fld_build}\\{self.cfg.csv_build}_v{self.cb_version}.csv'
        nts_data = fun.csv_to_dfr(nts_fldr)

        # process
        self._model_form()
        self._trip_rates_hb_build(nts_data)

        # regression models

    def _trip_rates_hb_build(self, dfr: pd.DataFrame):
        fun.log_stderr('\nTrip rates model')
        # produce data required for trip_rate_hb
        dfr = dfr.loc[(dfr['direction'] == 'hb_fr') & (dfr['w1'] == 1) & (dfr['purpose'] > 0)].copy()
        out_fldr = f'{self.cfg.fld_build}\\{self.cfg.fld_dbase}'
        # output trip rates
        fun.log_stderr(f' .. trip rates')
        col_grby = ['surveyyear', 'individualid', 'tfn_at', 'purpose'] + self.cfg.def_ttype
        out = dfr.groupby(col_grby)[['jjxsc']].sum().rename(columns={'jjxsc': 'weekly_trips'})
        out = fun.dfr_complete(out, None, 'purpose')
        fun.dfr_to_csv(out, out_fldr, f'hb_trip_rates_build', True)
        self.trm_rates = out.copy()
        #  response weight
        fun.log_stderr(f' .. response weights')
        col_grby = ['surveyyear', 'purpose']
        dfr['w2'] = dfr['w1'].mul(dfr['w2']).mul(dfr['jjxsc'])
        out = dfr.groupby(col_grby)[['trips', 'w2', 'w1']].sum()
        out['trips'] = out['trips'].div(out['w2']).fillna(0)
        out = out.rename(columns={'trips': 'r_weights', 'w1': 'count'}).drop(columns='w2')
        out = fun.dfr_complete(out, None, 'purpose')
        fun.dfr_to_csv(out, out_fldr, f'hb_trip_rates_response_weights', True)
        self.trm_response = out.copy()

    def _model_form(self):
        # temporary - create model forms by purpose/aws
        self.mdl_form = {}
        for pp in [1, 2, 3, 4, 5, 6, 7, 8]:
            self.mdl_form[pp] = {}
            for aws in [1, 2, 3, 4, 5, 6]:
                self.mdl_form[pp][aws] = {}
                mdl_form = 'nb'
                if (pp, aws) in [(1, 2)]:
                    mdl_form = 'zip'
                elif (pp, aws) in [(1, 3), (1, 4), (1, 5), (1, 6), (2, 3), (3, 4)]:
                    mdl_form = 'zinb'
                self.mdl_form[pp][aws]['form'] = mdl_form
                self.mdl_form[pp][aws]['formula'] = "weekly_trips ~ gender + hh_type + tfn_at + surveyyear"
