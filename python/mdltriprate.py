import os
import numpy as np
import pandas as pd
from typing import Union, List
import mdlfunction as fun
import mdlconfig
import mdllookup

# R & statsmodels packages
import statsmodels.api as sm
os.environ['R_HOME'] = r'C:\Program Files\R\R-4.3.1'
import rpy2.robjects as robj
from rpy2.robjects import pandas2ri as pd2r, packages as rpkg


class TripRate:
    """ calculate trip-rates u sing regression model
        require: R4.1.2, utils, MASS 7.3.54 and pscl 1.5.5
    """

    def __init__(self, nts_fldr: str, cb_version: Union[str, int], over_write: bool = True):
        fun.log_stderr('\n***** TRIP-RATE REGRESSION MODEL - HB PRODUCTION *****')
        # read config
        self.cfg = mdlconfig.Config(nts_fldr)
        self.luk = mdllookup.Lookup(self.cfg.nts_dtype)
        self.cb_version, self.tfn_modes = cb_version, self.cfg.tfn_modes
        self.tfn_ttype = self.cfg.tfn_ttype
        self.ppx_list = set(self.luk.purpose()['val'].values())
        self.aws_list = set(self.luk.aws()['val'].values())
        self.hhx_list = list(set(self.luk.hh_type()['val'].values()))

        # read in cb data
        if over_write:
            self._install_r()
            self._regx_form()
            fun.log_stderr('\nImport cb data')
            nts_fldr = f'{self.cfg.dir_cbuild}\\{self.cfg.csv_cbuild}_v{self.cb_version}.csv'
            nts_data = fun.csv_to_dfr(nts_fldr)

            # prepare database
            self._trip_rates_hb(nts_data)
            #
            # # run with python or R codes
            # # self._regx_model_py()
            self._regx_model_rs()
            self._regx_output()
            # self._compare_python_vs_r()
            self._analysis()
        else:
            fun.log_stderr(f' .. skipped!')

    @staticmethod
    def _install_r(upgrade: bool = False):
        # package version
        def _version(name):
            robj.r('''
                pkg_version <- function(pkg) packageVersion(pkg)
            ''')
            out = {0: val for _, val in robj.r['pkg_version'](name).items()}
            return '.'.join(str(col) for col in list(out[0]))

        # import utilities
        fun.log_stderr('\nImport R packages')
        ruti = rpkg.importr('utils')
        ruti.chooseCRANmirror(ind=1)
        # check installed packages
        for pkg in ('MASS', 'pscl'):
            if not rpkg.isinstalled(pkg) or upgrade:
                ruti.install_packages(pkg)
            else:
                fun.log_stderr(f' .. {pkg} - current version installed: {_version(pkg)}')
            rpkg.importr(pkg)
        fun.log_stderr(f' .. !!! be mindful of result differences when upgrading to a newer version !!!')
        pd2r.activate()

    def _trip_rates_hb(self, dfr: pd.DataFrame):
        fun.log_stderr('\nProcess NTS data')
        fun.log_stderr(f' .. prepare data')
        self.dfr_ttype = self.luk.tt_to_dfr(self.cfg.tfn_ttype, self.cfg.def_ttype)
        pop = dfr.groupby(['individualid', 'w2'])[['trips']].sum().reset_index().drop(columns='trips')
        dfr = dfr.loc[(dfr['direction'] == 'hb_fr') & (dfr['w1'] == 1)].copy()
        dfr['w5xhh'] = dfr['jjxsc'].mul(dfr['w5xhh']).fillna(0)
        # prepare data
        tfn_type = ['tt'] + self.tfn_ttype + ['tfn_at', 'purpose']
        col_grby = tfn_type + ['surveyyear', 'individualid', 'mode', 'w2']
        dfr = dfr.groupby(col_grby, observed=True)[['w1', 'jjxsc', 'w5xhh', 'trips']].sum()
        dfr = fun.dfr_complete(dfr, None, 'purpose')
        dfr = fun.dfr_complete(dfr, None, 'mode').reset_index()
        dfr = fun.dfr_filter_mode(dfr, self.cfg.tfn_modes)

        # weighted trip rates
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_hbase}'
        fun.log_stderr(f' .. weighted trip rates')
        ppx_list = dfr['purpose'].unique()
        out = dfr.groupby(tfn_type + ['individualid'])[['trips']].sum()
        col_grby = [col for col in tfn_type if col != 'purpose']
        out = pd.pivot_table(out, values='trips', index=col_grby + ['individualid'], columns='purpose').fillna(0)
        out.rename(columns={pp: f'p{pp}' for pp in ppx_list}, inplace=True)
        out = out.groupby(col_grby + ['individualid']).sum().reset_index()
        out = pd.merge(out, pop, how='left', on='individualid', suffixes=('', ''))
        out = pd.merge(out, self.dfr_ttype, how='outer', on=['tt'] + self.cfg.tfn_ttype).fillna(0)
        out[['tt', 'tfn_at']] = out[['tt', 'tfn_at']].astype(int)
        out = out.groupby(col_grby).sum().drop(columns='individualid')
        out = fun.dfr_complete(out, None, 'tfn_at')
        out = fun.dfr_filter_zero(out.reset_index(), col_grby)
        fun.dfr_to_csv(out, out_fldr, 'trip_rates_hb', False)
        out.rename(columns={f'p{pp}': pp for pp in ppx_list}, inplace=True)
        out = out.groupby(col_grby + ['w2']).sum().stack().reset_index()
        out.rename(columns={f'level_{len(col_grby) + 1}': 'purpose', 0: 'trip_rates'}, inplace=True)
        out = fun.dfr_filter_zero(out, ['purpose'])
        out['trip_rates'] = out['trip_rates'].div(out['w2']).fillna(0)
        fun.dfr_to_csv(out, out_fldr, 'trip_rates_hb_long', False)
        # travel diary
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_dbase}'
        fun.log_stderr(f' .. trip rates sample')
        dfr = fun.dfr_filter_zero(dfr, tfn_type)
        col_grby = ['surveyyear', 'individualid'] + col_grby + ['purpose', 'w2']
        out = dfr.groupby(col_grby, observed=True)[['jjxsc', 'w5xhh', 'trips']].sum()
        out.rename(columns={'jjxsc': 'weekly_trips', 'trips': 'trips_nts'}, inplace=True)
        fun.dfr_to_csv(out, out_fldr, f'hb_trip_rates_build', True)
        self.obs_trip = out.reset_index()
        # response weight
        fun.log_stderr(f' .. response weights')
        dfr['w2'] = dfr['w2'].mul(dfr['jjxsc']).fillna(0)  # weighted individual
        dfr = dfr.groupby(['surveyyear', 'purpose'], observed=True)[['w1', 'w2', 'jjxsc', 'trips']].sum()
        dfr.rename(columns={'trips': 'trips_nts'}, inplace=True)
        fun.dfr_to_csv(dfr, out_fldr, f'hb_trip_rates_response_weights', True)
        self.obs_resw = dfr.reset_index()

    def _regx_form(self):
        def _str_remove(str_main: str, str_removed: str) -> str:
            return str_main.replace(f' + {str_removed}', '').replace(f'{str_removed} + ', '')

        # create model forms by purpose/aws
        self.mdl_form = {}
        tfn_ttype = f'weekly_trips ~ surveyyear + tfn_at + {" + ".join(col for col in self.tfn_ttype)}'
        tfn_ttype = _str_remove(tfn_ttype, 'aws')  # aws is part of regression
        for pp in self.ppx_list:
            self.mdl_form[pp] = {}
            for aws in self.aws_list:
                self.mdl_form[pp][aws] = {}
                if (pp, aws) in [(1, 2)]:
                    mdl_form = 'zip'
                elif (pp, aws) in [(1, 3), (1, 4), (1, 5), (1, 6), (2, 3), (3, 4)]:
                    mdl_form = 'zinb'
                else:
                    mdl_form = 'nb'
                self.mdl_form[pp][aws]['form'] = mdl_form
                inf_zero = ' | 1' if mdl_form != 'nb' else ''  # for zero-inflated models
                mdl_spec = _str_remove(tfn_ttype, 'gender') if aws in [1] else tfn_ttype  # a child has no gender
                mdl_spec = _str_remove(mdl_spec, 'soc') if aws in [1, 4, 5, 6] else mdl_spec
                self.mdl_form[pp][aws]['spec'] = f'{mdl_spec}{inf_zero}'

    def _regx_model_rs(self):
        fun.log_stderr(f'\nRegression model (R)')
        obs_trip, obs_resw = self.obs_trip, self.obs_resw
        self.unw_trip, self.reg_trip, self.reg_stat = [], [], []
        tfn_ttype = self.tfn_ttype + ['tfn_at']
        col_grby = tfn_ttype + ['purpose']
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_hbase}\\analysis'
        fun.mkdir(out_fldr)
        for pp in self.ppx_list:
            reg_stat = []
            for aws in self.aws_list:
                mdl_form, mdl_spec = self.mdl_form[pp][aws]['form'], self.mdl_form[pp][aws]['spec']
                fun.log_stderr(f' .. purpose {pp} / aws {aws} ({mdl_form.upper()})')
                dfr = (obs_trip['purpose'] == pp) & (obs_trip['aws'] == aws)
                dfr = obs_trip.loc[dfr].reset_index(drop=True)
                # old method
                old = self._regx_engine_rs(mdl_form, mdl_spec, dfr, tfn_ttype, 'old')
                old = old.groupby(tfn_ttype + ['surveyyear', 'purpose'], observed=False)[['trip_rates']].mean()
                old = pd.merge(old.reset_index(), obs_resw, how='left', on=['surveyyear', 'purpose'])
                yrs_fact = old['w1'].div(old.groupby(col_grby, observed=True)['w1'].transform('sum')).fillna(1)
                res_fact = old['trips_nts'].div(old['w2']).fillna(1)
                old['trip_rates'] = old['trip_rates'].mul(yrs_fact).mul(res_fact)
                old = old.groupby(col_grby, observed=True)[['trip_rates']].sum()
                # new method
                df1 = self._regx_engine_rs_nw(mdl_form, mdl_spec, dfr, tfn_ttype, 'new')
                df1['trip_rates'] = df1['trip_rates'].mul(df1['w2_weights'])
                agg_func = {'w2': 'sum', 'w2_weights': 'sum', 'weekly_trips': 'sum', 'trips_nts': 'sum',
                            'trip_rates': 'sum'}
                df1 = df1.groupby(col_grby, observed=True).agg(agg_func)
                df1['trip_rates'] = df1['trip_rates'].div(df1['w2_weights'])
                df1.drop(columns=['w2', 'w2_weights', 'weekly_trips', 'trips_nts'], inplace=True)
                # new method 2
                dfr = self._regx_engine_rs(mdl_form, mdl_spec, dfr, tfn_ttype, 'new')
                self.unw_trip.append(dfr.groupby(['purpose', 'aws'])[['trip_rates']].mean())
                dfr['trip_rates'] = dfr['trip_rates'].mul(dfr['w2_weights'])
                agg_func = {'w2': 'sum', 'w2_weights': 'sum', 'weekly_trips': 'sum', 'trips_nts': 'sum',
                            'trip_rates': 'sum'}
                dfr = dfr.groupby(col_grby, observed=True).agg(agg_func)
                dfr['trip_rates'] = dfr['trip_rates'].div(dfr['w2_weights'])
                # combine data
                dfr = pd.merge(dfr, df1, how='left', on=col_grby, suffixes=('', '_df1'))
                dfr = pd.merge(dfr, old, how='left', on=col_grby, suffixes=('', '_old'))
                self.reg_trip.append(dfr[['trip_rates', 'trip_rates_df1', 'trip_rates_old']])
                dfr['trips_new'] = dfr['trip_rates'].mul(dfr['w2'])
                dfr['trips_df1'] = dfr['trip_rates_df1'].mul(dfr['w2'])
                dfr['trips_old'] = dfr['trip_rates_old'].mul(dfr['w2'])
                reg_stat.append(dfr.drop(columns='w2_weights'))

            # print data
            reg_stat = pd.concat(reg_stat, axis=0)
            fun.dfr_to_csv(reg_stat, out_fldr, f'Regression_p{pp}', True)
            fun.plt_scatter(f'Regression_p{pp}', reg_stat['trips_nts'], reg_stat['trips_old'],
                            'nts', 'old', out_fldr)
            fun.plt_scatter(f'Regression_p{pp}', reg_stat['trips_nts'], reg_stat['trips_df1'],
                            'nts', 'df1', out_fldr)
            fun.plt_scatter(f'Regression_p{pp}', reg_stat['trips_nts'], reg_stat['trips_new'],
                            'nts', 'new', out_fldr)
            self.reg_stat.append(reg_stat)
        self.reg_stat = pd.concat(self.reg_stat, axis=0)
        col_item = ['w2', 'weekly_trips', 'trips_nts', 'trips_new', 'trips_df1', 'trips_old']
        self.reg_stat = self.reg_stat.groupby(['purpose', 'aws'], observed=False)[col_item].sum()
        fun.dfr_to_csv(self.reg_stat, out_fldr, 'Regression_summary')

    def _regx_model_py(self):
        fun.log_stderr(f'\nRegression model (python)')
        obs_trip, obs_resw = self.obs_trip, self.obs_resw
        self.unw_trip, self.reg_trip, tfn_ttype = [], [], self.tfn_ttype + ['tfn_at']
        for pp in self.ppx_list:
            for aws in self.aws_list:
                mdl_form, mdl_spec = self.mdl_form[pp][aws]['form'], self.mdl_form[pp][aws]['spec']
                fun.log_stderr(f' .. purpose {pp} / aws {aws} ({mdl_form.upper()})')
                dfr = obs_trip.loc[(obs_trip['purpose'] == pp) & (obs_trip['aws'] == aws)]
                dfr = dfr.reset_index(drop=True)
                dfr = self._regx_engine_py(mdl_form, mdl_spec, dfr, tfn_ttype)
                # un-weighted trip-rates
                self.unw_trip.append(dfr.groupby(['purpose', 'aws'], observed=True)[['trips']].mean())
                # add in response weight
                dfr = pd.merge(dfr, obs_resw, how='left', on=['purpose', 'surveyyear'])
                dfr[['count', 'r_weights']] = dfr[['count', 'r_weights']].fillna(1)
                dfr_temp = dfr.groupby(tfn_ttype + ['purpose'], observed=True)['count'].transform('sum')
                dfr['count'] = dfr['count'].div(dfr_temp)
                dfr['trips'] = dfr['trips'].mul(dfr['r_weights']).mul(dfr['count'])
                dfr = dfr.groupby(tfn_ttype + ['purpose'], observed=True)[['trips']].sum()
                self.reg_trip.append(dfr)

    def _regx_output(self, reg_type: str = None):
        fun.log_stderr(f'\nWrite output trip-rates')
        # weighted & unweighted trip-rates
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_hbase}'
        out_fldr = f'{out_fldr}\\{reg_type}' if reg_type not in [None, ''] else out_fldr
        fun.mkdir(out_fldr)
        tfn_grby = self.tfn_ttype + ['tfn_at', 'purpose']
        self.unw_trip = pd.concat(self.unw_trip, axis=0).reset_index()
        self.unw_trip = self.unw_trip.pivot(columns='aws', index='purpose', values='trip_rates')
        self.unw_trip.rename(columns=self.luk.aws()['out'], inplace=True)
        fun.dfr_to_csv(self.unw_trip, out_fldr, 'trip_rates_hb_unweighted')
        self.reg_trip = pd.concat(self.reg_trip, axis=0).reset_index()
        self.reg_trip[tfn_grby] = self.reg_trip[tfn_grby].astype(int)
        self.reg_trip['tt'] = fun.dfr_to_tuple(self.reg_trip, self.tfn_ttype)
        dct = self.luk.tt_tfn(self.tfn_ttype, self.cfg.def_ttype)
        self.reg_trip['tt'] = self.reg_trip['tt'].apply(lambda x: dct.get(x, 0))
        self.reg_trip = fun.dfr_filter_zero(self.reg_trip, 'tt')
        self.reg_trip = self.reg_trip.set_index(['tt'] + tfn_grby).sort_index()
        fun.dfr_to_csv(self.reg_trip.rename(columns={0: 'trip_rates'}), out_fldr, 'trip_rates_hb_weighted')

    def _analysis(self, reg_type: str = None):
        fun.log_stderr(f'\nAnalysis')
        tfn_atyp = self.cfg.tfn_atype
        tfn_ttype = self.tfn_ttype + ['tfn_at', 'purpose']
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_hbase}\\{self.cfg.fld_rates}'
        nts_trip = fun.csv_to_dfr(f'{out_fldr}\\trip_rates_hb_long.csv', tfn_ttype + ['w2', 'trip_rates'])
        cte_trip = fun.csv_to_dfr(f'{out_fldr}\\trip_rates_hb_ctripend.csv', tfn_ttype + ['trips'])

        # back to original codes
        out_fldr = f'{out_fldr}\\{reg_type}' if reg_type not in [None, ''] else out_fldr
        reg_trip = fun.csv_to_dfr(f'{out_fldr}\\trip_rates_hb_weighted.csv',
                                  tfn_ttype + ['trip_rates', 'trip_rates_df1', 'trip_rates_old'])
        fun.mkdir(f'{out_fldr}\\{self.cfg.fld_graph}')
        # merge database
        nts_trip.rename(columns={'trip_rates': 'trips'}, inplace=True)
        reg_trip.rename(columns={'trip_rates': 'trips', 'trip_rates_df1': 'trips_df1', 'trip_rates_old': 'trips_old'},
                        inplace=True)
        all_trip = pd.merge(nts_trip, reg_trip, how='outer', on=tfn_ttype, suffixes=('_nts', '_reg')).fillna(0)
        all_trip.rename(columns={'w2': 'pop'}, inplace=True)
        if tfn_atyp == 'ntem':
            all_trip = pd.merge(all_trip, cte_trip, how='left', on=tfn_ttype).fillna(0)
            all_trip.rename(columns={'trips': 'trips_cte'}, inplace=True)
        # output scatter plots
        # col_list = ('cte', 'reg') if tfn_atyp == 'ntem' else ('nts', 'reg')  # x, y
        # col_trip = [f'trips_{col.lower()}' for col in col_list]
        for pp in all_trip['purpose'].unique():
            dfr_pp = all_trip.loc[all_trip['purpose'] == pp].reset_index(drop=True)
            col_list = ('cte', 'reg') if tfn_atyp == 'ntem' else ('nts', 'reg')
            col_trip = [f'trips_{col.lower()}' for col in col_list]
            fun.plt_scatter(f'TripRate_p{pp}_new', dfr_pp[col_trip[0]], dfr_pp[col_trip[1]], *col_list,
                            f'{out_fldr}\\{self.cfg.fld_graph}')
            col_list = ('cte', 'old') if tfn_atyp == 'ntem' else ('nts', 'old')
            col_trip = [f'trips_{col.lower()}' for col in col_list]
            fun.plt_scatter(f'TripRate_p{pp}_old', dfr_pp[col_trip[0]], dfr_pp[col_trip[1]], *col_list,
                            f'{out_fldr}\\{self.cfg.fld_graph}')
            col_list = ('cte', 'df1') if tfn_atyp == 'ntem' else ('nts', 'df1')
            col_trip = [f'trips_{col.lower()}' for col in col_list]
            fun.plt_scatter(f'TripRate_p{pp}_df1', dfr_pp[col_trip[0]], dfr_pp[col_trip[1]], *col_list,
                            f'{out_fldr}\\{self.cfg.fld_graph}')

            # for col in col_trip:
            #     dfr_pp[col] = dfr_pp['pop'].mul(dfr_pp[col], fill_value=0).fillna(0)
            #
            # # aggregate by hh_type
            # dfr = dfr_pp.groupby(['gender', 'aws', 'hh_type'])[['pop'] + col_trip].sum()
            # for col in col_trip:
            #     dfr[col] = dfr[col].div(dfr['pop'], fill_value=0).fillna(0)
            # fun.plt_scatter(f'TripRate_p{pp}_hh', dfr[col_trip[0]], dfr[col_trip[1]], *col_list,
            #                 f'{out_fldr}\\{self.cfg.fld_graph}')
            # # aggregate by area_type
            # dfr = dfr_pp.groupby(['gender', 'aws', 'tfn_at'])[['pop'] + col_trip].sum()
            # for col in col_trip:
            #     dfr[col] = dfr[col].div(dfr['pop'], fill_value=0).fillna(0)
            # fun.plt_scatter(f'TripRate_p{pp}_at', dfr[col_trip[0]], dfr[col_trip[1]], *col_list,
            #                 f'{out_fldr}\\{self.cfg.fld_graph}')

    def _compare_python_vs_r(self):
        fun.log_stderr(f'\nAnalysis')
        tfn_ttype = self.tfn_ttype + ['tfn_at', 'purpose']
        out_fldr = f'{self.cfg.dir_output}\\{self.cfg.fld_hbase}'
        pyx_trip = fun.csv_to_dfr(f'{out_fldr}\\python\\trip_rates_hb_weighted.csv', tfn_ttype + ['trips'])
        rst_trip = fun.csv_to_dfr(f'{out_fldr}\\rstudio\\trip_rates_hb_weighted.csv', tfn_ttype + ['trips'])
        # merge database
        all_trip = pd.merge(pyx_trip, rst_trip, how='left', on=tfn_ttype, suffixes=('_py', '_rs'))
        # output scatter plots
        for pp in all_trip['purpose'].unique():
            dfr = all_trip.loc[all_trip['purpose'] == pp].reset_index(drop=True)
            pyx, rst = dfr['trips_py'].values, dfr['trips_rs'].values
            fun.plt_scatter(f'p{pp}_aws0', pyx, rst, 'PY', 'RS', f'{out_fldr}\\{self.cfg.fld_graph}')
            for aws in all_trip['aws'].unique():
                mdl_form = self.mdl_form[pp][aws]['form']
                dfr = all_trip.loc[(all_trip['purpose'] == pp) & (all_trip['aws'] == aws)].reset_index(drop=True)
                pyx, rst = dfr['trips_py'].values, dfr['trips_rs'].values
                fun.plt_scatter(f'p{pp}_aws{aws}_{mdl_form}', pyx, rst, 'PY', 'RS',
                                f'{out_fldr}\\{self.cfg.fld_graph}')

    def _regx_engine_rs_nw(self, form: str, formula: str, dfr: pd.DataFrame, tfn_ttype: List,
                           method: str = 'new') -> pd.DataFrame:
        # R model setup with no weights applied
        # independent variables - categorical, dependent variables - continuous
        robj.r('''
            run_model <- function(form, formula, data) {
                if(form == "nb") {
                    glm.nb(formula = as.formula(formula), data = data,
                    control =glm.control(epsilon = 1e-09, maxit = 999))
                } else if(form == "zip") {
                    zeroinfl(formula = as.formula(formula), data = data,
                    dist = "poisson", control = zeroinfl.control(method = 'BFGS', maxit = 99999))
                } else if(form == "zinb") {
                    zeroinfl(formula = as.formula(formula), data = data,
                    dist = "negbin", control = zeroinfl.control(method = 'BFGS', maxit = 99999))
                }
            }
        ''')
        robj.r('''
            prediction <- function(model, data) {
                predict(model, data, type = "response")
            }
        ''')
        # run regression model: trip_rate = sum(w2*w5xhh*jjxsc)/sum(w2)
        method = method.lower()
        _, col_grby = self._reg_to_list(formula)
        dfr = dfr.copy()
        dfr[col_grby] = dfr[col_grby].astype(str).astype('category')
        reg = robj.r['run_model'](form, formula, pd2r.py2rpy(dfr))
        col_year = ['purpose', 'surveyyear'] if 'surveyyear' in col_grby else ['purpose']
        dfr['w1'] = 1
        dfr['w2_weights'] = dfr['w2'].mul(dfr['weekly_trips'])  # individual weights
        dfr['w5_weights'] = dfr['w2'].mul(dfr['w5xhh'])  # trip weights
        agg_func = {'w1': 'sum', 'w2': 'sum', 'w5xhh': 'sum', 'weekly_trips': 'sum', 'trips_nts': 'sum',
                    'w2_weights': 'sum', 'w5_weights': 'sum'}
        dfr = dfr.groupby(tfn_ttype + col_year, observed=False).agg(agg_func).reset_index()
        w5x_fill = self._nan_fill(dfr, col_grby, 'w5_weights', 'w2_weights')
        dfr['w5_weights'] = dfr['w5_weights'].div(dfr['w2_weights']).fillna(w5x_fill)  # response weights
        w2x_fill = self._nan_fill(dfr, col_grby, 'w2', 'w1')
        dfr['w2_weights'] = dfr['w2'].div(dfr['w1']).fillna(w2x_fill)
        dfr['trip_rates'] = robj.r['prediction'](reg, pd2r.py2rpy(dfr))
        dfr['trip_rates'] = dfr['trip_rates'].mul(dfr['w5_weights'] if method != 'old' else 1)
        dfr['w2_weights'] = np.where(dfr['w2'] > 0, dfr['w2'], 0.1 * dfr['w2_weights'])
        dfr[col_grby] = dfr[col_grby].astype(int)
        dfr['surveyyear'] = dfr['surveyyear'].astype(int) if 'surveyyear' in col_grby else 0
        return dfr.drop(columns='w5_weights')

    def _regx_engine_rs(self, form: str, formula: str, dfr: pd.DataFrame, tfn_ttype: List,
                        method: str = 'new') -> pd.DataFrame:
        # R model setup with offset = w5xhh/jjxsc and weights = w2
        # independent variables - categorical, dependent variables - continuous
        robj.r('''
            run_model <- function(form, formula, data) {
                if(form == "nb") {
                    glm.nb(formula = as.formula(formula), data = data, weights = w2_weights,
                    control =glm.control(epsilon = 1e-09, maxit = 999))
                } else if(form == "zip") {
                    zeroinfl(formula = as.formula(formula), data = data, weights = w2_weights,
                    dist = "poisson", control = zeroinfl.control(method = 'BFGS', maxit = 99999))
                } else if(form == "zinb") {
                    zeroinfl(formula = as.formula(formula), data = data, weights = w2_weights,
                    dist = "negbin", control = zeroinfl.control(method = 'BFGS', maxit = 99999))
                }
            }
        ''')
        robj.r('''
            prediction <- function(model, data) {
                predict(model, data, type = "response")
            }
        ''')
        # run regression model
        method = method.lower()
        _, col_grby = self._reg_to_list(formula)
        formula = formula.split('|')
        formula = f'{formula[0]} + offset(log(w5_weights))' + (f' | {formula[1]}' if len(formula) > 1 else '')
        dfr = dfr.copy()

        # back to codes
        dfr[col_grby] = dfr[col_grby].astype(str).astype('category')
        if method == 'old':  # set offset and weight to 1
            dfr['w5xhh'], dfr['w2'] = dfr['weekly_trips'], 1
        dfr['w2_weights'], dfr['w1'] = dfr['w2'], 1
        w5x_fill = self._nan_fill(dfr, col_grby, 'w5xhh', 'weekly_trips')
        dfr['w5_weights'] = dfr['w5xhh'].div(dfr['weekly_trips']).fillna(w5x_fill)  # offset
        reg = robj.r['run_model'](form, formula, pd2r.py2rpy(dfr))
        col_year = ['purpose', 'surveyyear'] if 'surveyyear' in col_grby else ['purpose']
        agg_func = {'w5xhh': 'sum', 'w2': 'sum', 'weekly_trips': 'sum', 'trips_nts': 'sum', 'w1': 'sum'}
        dfr = dfr.groupby(tfn_ttype + col_year, observed=False).agg(agg_func).reset_index()
        w2x_fill = self._nan_fill(dfr, col_grby, 'w2', 'w1')
        dfr['w2_weights'] = dfr['w2'].div(dfr['w1']).fillna(w2x_fill)
        w5x_fill = self._nan_fill(dfr, col_grby, 'w5xhh', 'weekly_trips')
        dfr['w5_weights'] = dfr['w5xhh'].div(dfr['weekly_trips']).fillna(w5x_fill)
        dfr['trip_rates'] = robj.r['prediction'](reg, pd2r.py2rpy(dfr))
        dfr['trip_rates'] = dfr['trip_rates'].mul(dfr['w5_weights'])
        dfr['w2_weights'] = np.where(dfr['w2'] > 0, dfr['w2'], 0.1 * dfr['w2_weights'])
        dfr[col_grby] = dfr[col_grby].astype(int)
        dfr['surveyyear'] = dfr['surveyyear'].astype(int) if 'surveyyear' in col_grby else 0
        return dfr.drop(columns='w5_weights')

    def _regx_engine_py(self, form: str, formula: str, dfr: pd.DataFrame, tfn_ttype: List):
        # model setup
        def _model(endog, exog):
            if form.lower() == 'nb':
                # func = sm.NegativeBinomial(endog, exog, loglike_method='nb2')
                func = sm.NegativeBinomialP(endog, exog, p=2)
            elif form.lower() == 'zip':
                func = sm.ZeroInflatedPoisson(endog, exog)
            else:
                func = sm.ZeroInflatedNegativeBinomialP(endog, exog, p=1)
            return func.fit(maxiter=9999)

        # run regression model
        pp = dfr['purpose'].iloc[0]
        col_trip, col_grby = self._reg_to_list(formula)
        dfr[col_grby] = dfr[col_grby].astype('category')
        reg = _model(dfr[col_trip], self._dfr_to_dcat(dfr, col_grby))
        # add predictions
        dfr = dfr.groupby(tfn_ttype + (['surveyyear'] if 'surveyyear' in col_grby else []), observed=False
                          )[['weekly_trips']].sum().reset_index()
        dfr.drop(columns='weekly_trips', inplace=True)
        out = reg.predict(self._dfr_to_dcat(dfr, col_grby))
        dfr['surveyyear'] = dfr['surveyyear'].astype(int) if 'surveyyear' in col_grby else 0
        dfr['purpose'], dfr['trips'] = pp, out
        return dfr

    def _test_regression(self, dfr: pd.DataFrame, form: str, formula: str):
        # test 2
        _, col_grby = self._reg_to_list(formula)
        col_calc = ['w2', 'weekly_trips', 'w5xhh', 'trips_nts']

        a = dfr.copy()
        str_form = formula.split('|')
        str_form = f'{str_form[0]} + w2 + offset(log(w5_weights))' + (f' | {str_form[1]}' if len(str_form) > 1 else '')
        a = a.groupby(col_grby, observed=True)[col_calc].sum().reset_index()
        a[col_grby] = a[col_grby].astype(str).astype('category')
        a['w1'], a['w2_weights'], col_weight = 1, 1, 'trips_nts'
        w5x_fill = self._nan_fill(a, col_grby, col_weight, 'weekly_trips')
        a['w5_weights'] = a[col_weight].div(a['weekly_trips']).fillna(w5x_fill)  # offset
        reg = robj.r['run_model'](form, str_form, pd2r.py2rpy(a))
        a = a.groupby(col_grby, observed=False)[['w1'] + col_calc].sum().reset_index()
        a['w2_weights'] = 1
        w5x_fill = self._nan_fill(a, col_grby, col_weight, 'weekly_trips')
        a['w5_weights'] = a[col_weight].div(a['weekly_trips']).fillna(w5x_fill)
        a['trip_rates'] = robj.r['prediction'](reg, pd2r.py2rpy(a))
        a.to_csv('test2.csv', index=False)

        # test 4
        a = dfr.copy()
        str_form = formula.split('|')
        str_form = f'{str_form[0]} + offset(log(w5_weights))' + (f' | {str_form[1]}' if len(str_form) > 1 else '')
        a[col_grby] = a[col_grby].astype(str).astype('category')
        a['w1'], a['w2_weights'], col_weight = 1, a['w2'], 'w5xhh'
        w5x_fill = self._nan_fill(a, col_grby, col_weight, 'weekly_trips')
        a['w5_weights'] = a[col_weight].div(a['weekly_trips']).fillna(w5x_fill)  # offset
        reg = robj.r['run_model'](form, str_form, pd2r.py2rpy(a))
        a = a.groupby(col_grby, observed=False)[col_calc].sum().reset_index()
        w2x_fill = self._nan_fill(a, col_grby, 'w2', 'w1')
        a['w2_weights'] = a['w2'].div(a['w1']).fillna(w2x_fill)
        w5x_fill = self._nan_fill(a, col_grby, col_weight, 'weekly_trips')
        a['w5_weights'] = a[col_weight].div(a['weekly_trips']).fillna(w5x_fill)
        a['trip_rates'] = robj.r['prediction'](reg, pd2r.py2rpy(a))
        a['trip_rates'] = a['w5_weights'].mul(a['trip_rates']).mul(a['w2'])
        a.to_csv('test4.csv', index=False)

    @staticmethod
    def _dfr_to_dcat(dfr: pd.DataFrame, col_used: Union[str, List]) -> pd.DataFrame:
        # similar to patsy.dmatrices
        dfr = dfr[col_used].reset_index(drop=True)
        out = pd.DataFrame(data={'intercept': [1.] * len(dfr)}, index=dfr.index)
        for col in dfr.columns:
            tmp = pd.get_dummies(dfr[col], dtype=int)
            tmp.drop(columns=tmp.columns[0], errors='ignore', inplace=True)
            tmp.rename(columns={itm: f'{col}_{itm}' for itm in tmp.columns}, inplace=True)
            out = pd.merge(out, tmp, how='left', left_index=True, right_index=True)
        return out

    @staticmethod
    def _reg_to_list(reg_text: str) -> tuple:
        # convert regression model formula to list
        col_trip, col_spec = [col for col in reg_text.split('~')]
        col_spec = col_spec.split('|')[0]
        return col_trip.strip(), [col.strip() for col in col_spec.split('+')]

    @staticmethod
    def _nan_fill(dfr: pd.DataFrame, col_grby: List, col_enum: str, col_deno: str) -> pd.Series:
        out = dfr[col_enum]
        for lev, key in enumerate(col_grby):
            col_used = col_grby if lev == 0 else col_grby[:-lev]
            dfr_enum = dfr.groupby(col_used, observed=True)[col_enum].transform('sum')
            dfr_deno = dfr.groupby(col_used, observed=True)[col_deno].transform('sum')
            out = dfr_enum.div(dfr_deno) if lev == 0 else out.fillna(dfr_enum.div(dfr_deno))
        out = out.fillna(dfr[col_enum].div(dfr[col_deno]).fillna(1))
        return out

    @staticmethod
    def _test_aggregation(nts_trip: pd.DataFrame):
        # test aggregation
        dct_2agg = {}
        col_grby = ['purpose', 'gender', 'aws', 'soc', 'ns', 'hh_type', 'tfn_at']
        nts_trip[col_grby] = nts_trip[col_grby].astype(int)
        nts_trip['trips'] = nts_trip['trip_rate'].mul(nts_trip['w2']).fillna(0)
        dct_2agg['l1'] = nts_trip.groupby(col_grby)[['w2', 'trips']].sum()  # individually, 125 segments
        dct_2agg['l2'] = nts_trip.groupby(col_grby[:-1])[['w2', 'trips']].sum()  # all. area type
        dct_2agg['l3'] = nts_trip.groupby(col_grby[:-2])[['w2', 'trips']].sum()  # all. hh type
        dct_2agg['l4'] = nts_trip.groupby(col_grby[:-3])[['w2', 'trips']].sum()  # all. ns
        dct_2agg['l5'] = nts_trip.groupby(col_grby[:-4])[['w2', 'trips']].sum()  # all. soc
