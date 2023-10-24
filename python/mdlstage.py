import mdlconfig
import mdlfunction as fun
from typing import Union, List
import mdllookup as luk
import pandas as pd


class Stage:
    """ produce stage-related analysis:
        1. personal affordability
        2. etc.
    """

    def __init__(self, nts_fldr: str, cb_version: Union[str, int], over_write: bool = True):
        fun.log_stderr('\n***** NTS STAGE OUTPUTS *****')
        # read config
        self.cfg = mdlconfig.Config(nts_fldr)
        self.luk = luk.Lookup(self.cfg.nts_dtype)
        self.tfn_ttype, self.tfn_atype = self.cfg.tfn_ttype, self.cfg.tfn_atype
        self.cb_version, self.tfn_mode = cb_version, self.cfg.tfn_modes

        # read in cb data
        if over_write:
            fun.log_stderr('Import cb data')
            self.csv_fuel = fun.csv_to_dfr(f'{nts_fldr}\\fuel_cost_per_litre.csv')
            self.csv_fuel.set_index('year', inplace=True)
            nts_data = f'{self.cfg.fld_cbuild}\\{self.cfg.csv_cbuild}_stage_v{self.cb_version}.csv'
            nts_data = fun.csv_to_dfr(nts_data)
            self._affordability(nts_data, ['surveyyear', 'hholdgor_b02id', 'ns'])
        else:
            fun.log_stderr(f' .. skipped!')

    def _affordability(self, dfr: pd.DataFrame, seg_incl: Union[str, List]):
        fun.log_stderr('Personal affordability')
        # total household incomes
        col_grby = seg_incl if isinstance(seg_incl, list) else [seg_incl]
        col_hold = ['householdid'] if 'householdid' not in col_grby else []
        hhi = dfr.groupby(col_hold + col_grby)[['hh_income', 'w2']].mean().reset_index()
        hhi['weekly_income'] = hhi['hh_income'].apply(lambda x: self._net_income(x)) / 52
        # household weights
        hhi['weekly_income'] = hhi['weekly_income'].mul(dfr['w2'])
        hhi = hhi.groupby(col_grby).agg({'weekly_income': 'sum', 'w2': 'sum'}).reset_index()
        # car fuel costs
        dfr['stagefuel'], car_mask = 0., dfr['stagemode'].isin([3, 4])  # car & van
        dfr.loc[(car_mask) & (dfr['fueltype'] == 1), 'stagefuel'] = self._lpk_tag(dfr, 'petrol')
        dfr.loc[(car_mask) & (dfr['fueltype'] == 2), 'stagefuel'] = self._lpk_tag(dfr, 'diesel')
        dfr.loc[(car_mask) & (dfr['fueltype'] == 3), 'stagefuel'] = self._lpk_tag(dfr, 'electric')
        dfr.loc[(car_mask) & (dfr['fueltype'].isin([4, 5, 6, 7, 8])), 'stagefuel'] = self._lpk_tag(dfr, 'other')
        # car/pt fare costs
        col_grby = col_grby + ['stagemode']
        col_calc = ['stagedistance', 'stagefuel', 'stagecost']
        out = dfr[col_grby + col_calc + ['w2', 'w5xhh']].copy()
        for col in col_calc:
            out[col] = out[col].mul(out['w2'])  # .mul(out['w5xhh'])
        out = out.groupby(col_grby)[col_calc].sum()
        out = fun.dfr_complete(out, None, 'stagemode').reset_index()
        # merge with income data
        out = pd.merge(out, hhi, how='left', on=[col for col in col_grby if col != 'stagemode'])
        out = out.set_index('stagemode').rename(index=self.luk.mode('stage')['out']).reset_index()
        if 'hholdgor_b02id' in seg_incl:
            out = out.set_index('hholdgor_b02id').rename(index=self.luk.gor_02id).reset_index()
        # write result
        fun.dfr_to_csv(out, f'{self.cfg.fld_output}\\{self.cfg.fld_stage}', 'personal_affordability', False)

    # disposable income factor
    @staticmethod
    def _net_income(x: float) -> float:
        y0 = 10_000
        y1 = max((1 - 0.20 - 0.12) * (min(x, 50_000) - y0), 0)
        y2 = max((1 - 0.40 - 0.02) * (min(x, 150_000) - 50_000), 0)
        y3 = max((1 - 0.45 - 0.02) * (x - 150_000), 0)
        return y0 + y1 + y2 + y3

    # fuel consumption (TAG may 23 - 2015 base)
    def _lpk_tag(self, dfr: pd.DataFrame, fuel_type: str = 'other') -> pd.Series:
        # fuel prices
        fuel = self.csv_fuel[fuel_type].to_dict()
        # lpk = (a/v + b + c.v + d.v2)
        if fuel_type == 'petrol':
            a, b, c, d = 0.451946800, 0.096046026, -0.001094078, 0.000007246
        elif fuel_type == 'diesel':
            a, b, c, d = 0.481912969, 0.069094402, -0.000664707, 0.000005238
        elif fuel_type == 'electric':
            a, b, c, d = 0.000000000, 0.220656352, -0.000000000, 0.000000000
        else:
            a, b, c, d = 0.430405115, 0.077775873, -0.000835678, 0.000005878
        cst = dfr['surveyyear'].apply(lambda x: fuel[x]) / 100
        spd = dfr['stagedistance'].mul(self.cfg.m2k_fact).div(dfr['stagetime'].div(60))
        spd.loc[spd < 10], spd.loc[spd > 100] = 10, 100
        lpk = a / spd + b + c * spd + d * spd ** 2
        return lpk * dfr['stagedistance'] * self.cfg.m2k_fact * cst
