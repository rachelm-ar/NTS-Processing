from typing import Union, List, Dict, Any
import matplotlib.pyplot as plt
import pandas as pd
import subprocess
import itertools as itt
import numpy as np
import sys
import os


# filter zero from dataframe
def dfr_filter_zero(dfr: pd.DataFrame, col_used: Union[List, str]) -> pd.DataFrame:
    col_used = [col_used] if isinstance(col_used, str) else col_used
    return dfr.loc[(~dfr[col_used].isin([0, '0'])).all(axis=1)].reset_index(drop=True)


# filter mode values
def dfr_filter_mode(dfr: pd.DataFrame, inc_list: List, col_mode: str = 'mode') -> pd.DataFrame:
    return dfr.loc[dfr[col_mode].isin(inc_list)].reset_index(drop=True)


# create a complete set of index values
def dfr_complete(dfr: pd.DataFrame, col_index: Union[List, str, None], col_unstk: Union[List, str]
                 ) -> pd.DataFrame:
    col_index = [] if col_index is None else str_to_list(col_index)
    col_unstk = str_to_list(col_unstk)
    dfr = dfr.set_index(col_index) if len(col_index) > 0 else dfr
    for col in col_unstk:
        dfr = dfr.unstack(level=col, fill_value=0).stack()
    return dfr


# import csv to dataframe
def csv_to_dfr(csv_file: str, col_incl: Union[List, str] = None, nts_dtype: type = object
               ) -> Union[pd.DataFrame, bool]:
    log_stderr(f' .. read {csv_file}')
    _, _, _, csv_extn = split_file(csv_file)
    csv_extn = '\t' if csv_extn.lower() == '.tab' else ','
    if col_incl is not None:
        col_incl = [str_lower(col) for col in str_to_list(col_incl)]
    dfr = pd.read_csv(csv_file, sep=csv_extn, low_memory=False)
    dfr = dfr.rename(columns={key: key.lower().strip() for key in dfr.columns})
    try:
        dfr = dfr[col_incl] if col_incl is not None else dfr
    except KeyError as err:
        log_stderr(f'    !!! {split_file(csv_file)[2]} -> {err} !!!')
        dfr = dfr[[col for col in col_incl if col in dfr.columns]] if col_incl is not None else dfr

    dfr = dfr.fillna('0').replace(' ', '0')
    if nts_dtype is int:
        col_incl = ['w2', 'w5', 'w5xhh', 'tripdisincsw', 'triptravtime', 'hholdoslaua_b01id', 'hholdosward_b01id',
                    'settlement2011ew_b01id', 'stagedistance', 'stagetime', 'stagefarecost', 'stagecost',
                    'ticketcost', 'tickettripcost']
        col_incl = [col for col in dfr.columns if col not in col_incl]
        try:
            dfr[col_incl] = dfr[col_incl].astype('int64')
        except ValueError as err:
            log_stderr(f'    error with {csv_file}: {err}')

    return dfr


# create directory
def mkdir(sub_fldr: str):
    os.makedirs(sub_fldr, exist_ok=True)


# write dataframe to csv
def dfr_to_csv(dfr: pd.DataFrame, csv_path: str, csv_name: str, index: bool = True):
    csv_name = f'{csv_name}.csv' if '.csv' not in csv_name.lower() else csv_name
    dfr.to_csv(f'{csv_path}\\{csv_name}', index=index)


# convert string to list
def str_to_list(str_text: Union[str, float, int, List]) -> List:
    return [str_text] if not isinstance(str_text, list) else str_text


# convert list to dictionary
def list_to_dict(dct: Union[List, Dict]) -> Dict:
    if isinstance(dct, dict):
        dct = {str_lower(col): dct[col] for col in dct}
    else:
        dct = {str_lower(col): 1 for col in dct}
    return dct


def dfr_to_dict(dfr: pd.DataFrame, key: str, val: str) -> Dict:
    return dfr[[key, val]].set_index(key).to_dict()[val]


# create tuple from dataframe columns
def dfr_to_tuple(dfr: pd.DataFrame, col_used: List) -> pd.Series:
    # create tuple from dfr columns
    col_2zip = zip(*[dfr[col] for col in col_used])
    return dfr[col_used[0]] if len(col_used) == 1 else pd.Series([col for col in col_2zip])


# convert str to lower()
def str_lower(val: Any) -> Any:
    if isinstance(val, str):
        return val.lower()
    elif isinstance(val, (tuple, list, dict, set)):
        return tuple(itm.lower() if isinstance(itm, str) else itm for itm in val)
    else:
        return val


# scatter plots
def plt_scatter(title, x_val, y_val, x_label: str, y_label: str, out_fldr: str):
    x_label, y_label = x_label.upper(), y_label.upper()
    plt.rc('font', size=8)
    plt.grid(True)
    p_fit = np.polyfit(x_val, y_val, 1)
    l_fit = np.poly1d(p_fit)
    y_fit = l_fit(x_val)
    r_sqr = rsq_calc(y_val, y_fit)
    v_max = max(1.02 * x_val.max(), 1.02 * max(y_val.max(), y_fit.max()))
    plt.scatter(x_val, y_val, c='b', marker='.')
    plt.plot(x_val, y_fit, c='g')
    plt.ylabel(f'{y_label}', size=8)
    plt.xlabel(f'{x_label}', size=8)
    plt.xlim(0, v_max)
    plt.ylim(0, v_max)
    plt.title(f'{title} - {y_label} vs. {x_label}', size=10)
    plt.annotate(f'Slope = {p_fit[0]:.3f}\nIntercept = {p_fit[1]:.3f}\nR2 = {r_sqr:.3f}',
                 (0.02 * v_max, 0.98 * v_max), ha='left', va='top',
                 bbox=dict(boxstyle='round', fc='w'), size=8)
    plt.savefig(f'{out_fldr}\\{title}_{y_label}_vs_{x_label}.png', dpi=300)
    plt.close()


# calculate R2
def rsq_calc(obs: np.ndarray, est: np.ndarray) -> float:
    return max(1 - np.sum((est - obs) ** 2) / np.sum((obs - np.mean(obs)) ** 2), 0)


# print
def log_stderr(*args):
    print(*args, file=sys.stderr, flush=True)


# subprocess - single
def cmd_single(cmd_list: Union[List, str]):
    cmd_list = cmd_list if type(cmd_list) is list else [cmd_list]
    for ts in cmd_list:
        pr = subprocess.Popen(ts, creationflags=subprocess.CREATE_NEW_CONSOLE, shell=True)
        pr.wait()


# subprocess - multic
def cmd_multic(cmd_list: List, num_cpus: int = 999):
    num_bloc = int(len(cmd_list) / num_cpus) + 1
    for bl in range(num_bloc):
        min_bloc = min(bl * num_cpus, len(cmd_list))
        max_bloc = min(min_bloc + num_cpus, len(cmd_list))
        exe_list = [subprocess.Popen(pr, creationflags=subprocess.CREATE_NEW_CONSOLE, shell=True)
                    for pr in cmd_list[min_bloc:max_bloc]]
        for pr in exe_list:
            pr.wait()


# expand string
def str_xpanse(str_text: str) -> str:
    str_list = str(str_text).replace(';', ',').replace(' ', '').split(',')
    out_text = ''
    for dx in range(len(str_list)):
        if '-' not in str_list[dx]:
            out_text += f'{str_list[dx]},'
        else:
            tmp_list = str_list[dx].split('-')
            for dy in range(int(tmp_list[0]), int(tmp_list[1]) + 1):
                out_text += f'{dy},'
    return out_text[:-1]


# string to value
def str_to_value(str_text: Union[str, int, float]) -> Union[str, int, float]:
    if type(str_text) is str:
        try:
            out_value = int(str_text)
        except ValueError:
            try:
                out_value = float(str_text)
            except ValueError:
                out_value = str_text.strip()
    else:
        out_value = str_text
    return out_value


# list to dictionary
def list_to_dict_item(uni_list: Union[List, Dict], key_start: int = 0) -> Dict:
    if type(uni_list) is not dict:
        uni_dict = {key: item for key, item in enumerate(uni_list, key_start)}
    else:
        uni_dict = uni_list
    return uni_dict


# split filename to path, name, extn
def split_file(str_file: str) -> List:
    str_path = os.path.dirname(str_file)
    str_name, str_extn = os.path.splitext(str_file.replace('\\', '/').split('/')[-1])
    return [str_file, str_path, str_name, str_extn]


# file existence
def exist_file(str_file: str, err_index: bool = True) -> bool:
    if not os.path.isfile(str_file):
        err_index = False
        log_stderr(f'    >> file not found [{txt_truncate(str_file)}]')
    return err_index


# path existence
def exist_path(str_path: str, err_index: bool = True) -> bool:
    if not os.path.isdir(str_path):
        err_index = False
        log_stderr(f'    >> path not found [{txt_truncate(str_path)}]')
    return err_index


# reduced print
def txt_truncate(str_text: str, len_text: int = 50) -> str:
    out_text = str_text[-len_text:]
    pos_text = out_text.find('\\') if out_text.find('\\') > 0 else 0
    out_text = f"..{out_text[pos_text:] if len(str_text) > len_text else str_text}"
    return out_text


# add path
def add_path(cur_path: str, str_file: str) -> str:
    return f'{cur_path}\\{str_file}' if '\\' not in str_file else str_file


# derive distance band
def dist_band(max_dist: Union[List, float], pow_incr: float = 2.2) -> np.ndarray:
    if isinstance(max_dist, float):
        max_dist = int(max_dist + 1)
        num_band = int(max_dist ** 0.51)
        arr_dist = np.array([int(((0 if val == 0 else val + 1) / num_band) ** pow_incr * max_dist)
                             for val in range(num_band)])
    else:
        arr_dist = np.array(max_dist)
    return arr_dist


# product function
def product(*args) -> List:
    return list(itt.product(*args))


# fill data from aggregate level
def agg_fill(dfr: pd.DataFrame, col_grby: Union[List, str], col_segm: Union[List, str], col_calc: str,
             val_vmin: float = 0) -> pd.Series:
    # col_grby: list of columns to aggregate, hierarchical level from right to left
    # e.g. col_grby = [at, hh, tt] -> calculate %split by [at, hh, tt] first,
    # else aggregate to [at, hh] type -> calc %split, then aggregate to [at] -> calc %split
    # col_segm: further segmentation to calculate %split

    def _calc_split():
        enu = dfr.groupby(col_used + col_segm, observed=True)[col_calc].transform('sum')
        den = dfr.groupby(col_used, observed=True)[col_calc].transform('sum')
        out.loc[(out.isna()) & (enu > 0) & (den > val_vmin)] = enu.div(den)
        return out

    out = pd.Series(data=[np.nan] * len(dfr))
    col_grby, col_segm = str_to_list(col_grby), str_to_list(col_segm)
    dfr = dfr[col_grby + col_segm + [col_calc]].copy()
    for lev, key in enumerate(col_grby):
        # ite_last = True if lev == len(col_grby) - 1 else False
        col_used = col_grby if lev == 0 else col_grby[:-lev]
        _calc_split()

        # aggregate to hh & at subgroup
        if col_used[-1] == 'hh_type':
            dfr.loc[dfr['hh_type'].isin([1, 3, 6]), 'hh_type'] = 1  # 0 car
            dfr.loc[dfr['hh_type'].isin([4, 7]), 'hh_type'] = 2  # 1 car
            dfr.loc[dfr['hh_type'].isin([2, 5, 8]), 'hh_type'] = 3  # 2+ car
            _calc_split()

        # TODO: this needs updating with new at
        if col_used[-1] == 'tfn_at':
            dfr.loc[dfr['tfn_at'].isin([1, 2]), 'tfn_at'] = 1  # london
            dfr.loc[dfr['tfn_at'].isin([3, 4, 5]), 'tfn_at'] = 2  # major & minor
            dfr.loc[dfr['tfn_at'].isin([6, 7]), 'tfn_at'] = 3  # city (north)
            dfr.loc[dfr['tfn_at'].isin([8, 9]), 'tfn_at'] = 4  # city (midland)
            dfr.loc[dfr['tfn_at'].isin([10, 11, 12]), 'tfn_at'] = 5  # city (south)
            dfr.loc[dfr['tfn_at'].isin([13, 14]), 'tfn_at'] = 6  # rural
            _calc_split()

    return out.fillna(0)
