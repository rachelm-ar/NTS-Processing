"""
Module for calculating trip-rates from NTS data
"""

import pandas as pd
import numpy as np
import main_nts
# from sklearn import RandomForestRegressor
from sklearn.model_selection import cross_val_score




def trip_rates(dfr):
    dfr['trips'] = dfr['w2'] * dfr['w5xhh'] * dfr['jjxsc']
    filtered = dfr[["IndividualID", "p", "SurveyYear", "aws", "gender",
                     "hh_type", "soc", "ns", "trips"]]
    grouped = filtered.groupby(["IndividualID", "SurveyYear", "aws", "gender",
                     "hh_type", "soc", "ns", "p"])['trips'].sum()
    filled = grouped.unstack(fill_value=0).stack()

def response_weights(cb: pd.DataFrame):
    df = cb.loc[(cb['purpose'].isin(list(range(1,9)))) & (cb['w1']==1), ['individualid', 'purpose', 'surveyyear', 'w5xhh', 'jjxsc', 'w2']]
    df['trips'] = 1
    df['val_1'] = df['w5xhh'] * df['jjxsc'] * df['w2']
    df['val_2'] = df['trips'] * df['jjxsc'] * df['w2']
    grouped = df.groupby(['surveyyear', 'purpose'])[['val_1', 'val_2', 'trips']].sum()
    grouped['r_weights'] = grouped['val_1'] / grouped['val_2']
    grouped.rename(columns = {'trips':'count'}, inplace=True)
    weights = grouped.unstack(fill_value= 0).stack()
    return weights[['r_weights', 'count']]
    


def cross_val(model, x, y):
    rmses = cross_val_score(model, x, y, scoring ='neg_root_mean_squared_error', cv=10)



if __name__ == '__main__':
    nts = main_nts.NTS(r"E:\NTS\UKDA-7553-tab\tab", r"E:\NTS")
    data = nts._read_nts()
    data = nts._preprocess(data)
    weights = response_weights(data)
    weights.to_csv(r"I:\NTS\import\hb_trip_rates\is_refactor\r_weights.csv")
    
    print('debugging')