from typing import Union, List, Dict, Any
import pandas as pd
import subprocess
import itertools as itt
import numpy as np
import sys
import os


# filter zero from dataframe
def dfr_filter_zero(dfr: pd.DataFrame, col_used: Union[List, str]) -> pd.DataFrame:
    return dfr.loc[(~dfr[col_used].isin([0, '0'])).all(axis=1)].reset_index(drop=True)


# filter mode values
def dfr_filter_mode(dfr: pd.DataFrame, inc_list: List) -> pd.DataFrame:
    return dfr.loc[dfr['mode'].isin(inc_list)].reset_index(drop=True)


# create a complete set of index values
def dfr_complete(dfr: pd.DataFrame, col_index: Union[List, str, None], col_unstack: Union[List, str]
                 ) -> pd.DataFrame:
    if col_index is None or len(col_index) == 0:
        return dfr.unstack(level=col_unstack, fill_value=0).stack()
    else:
        return dfr.set_index(col_index).unstack(level=col_unstack, fill_value=0).stack()


# import csv to dataframe
def csv_to_dfr(csv_file: str, col_incl: Union[List, str] = None) -> Union[pd.DataFrame, bool]:
    log_stderr(f' .. read {csv_file}')
    _, _, _, csv_extn = split_file(csv_file)
    csv_extn = '\t' if csv_extn.lower() == '.tab' else ','
    if col_incl is not None:
        col_incl = [str_lower(col) for col in str_to_list(col_incl)]
    dfr = pd.read_csv(csv_file, sep=csv_extn, low_memory=False)
    dfr = dfr.rename(columns={key: key.lower().strip() for key in dfr.columns})
    try:
        dfr = dfr[col_incl] if col_incl is not None else dfr
        dfr = dfr.fillna('0')
    except KeyError as err:
        log_stderr(f'    error with {csv_file}: {err}')
        dfr = False
    return dfr


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


# PRINT STDERR
def log_stderr(*args):
    print(*args, file=sys.stderr, flush=True)


# SUBPROCESS - SINGLE
def cmd_single(cmd_list: Union[List, str]):
    cmd_list = cmd_list if type(cmd_list) is list else [cmd_list]
    for ts in cmd_list:
        pr = subprocess.Popen(ts, creationflags=subprocess.CREATE_NEW_CONSOLE, shell=True)
        pr.wait()


# SUBPROCESS - MULTI (USE WITH CAUTION AS MAY CLASH WITH MULTIPROCESSING)
def cmd_multic(cmd_list: List, num_cpus: int = 999):
    num_bloc = int(len(cmd_list) / num_cpus) + 1
    for bl in range(num_bloc):
        min_bloc = min(bl * num_cpus, len(cmd_list))
        max_bloc = min(min_bloc + num_cpus, len(cmd_list))
        exe_list = [subprocess.Popen(pr, creationflags=subprocess.CREATE_NEW_CONSOLE, shell=True)
                    for pr in cmd_list[min_bloc:max_bloc]]
        for pr in exe_list:
            pr.wait()


# EXPAND STRING
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


# CONVERT STRING TO VALUE
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


# CONVERT LIST TO DICTIONARY
def list_to_dict_item(uni_list: Union[List, Dict], key_start: int = 0) -> Dict:
    if type(uni_list) is not dict:
        uni_dict = {key: item for key, item in enumerate(uni_list, key_start)}
    else:
        uni_dict = uni_list
    return uni_dict


# SPLIT PATH, NAME AND EXTN
def split_file(str_file: str) -> List:
    str_path = os.path.dirname(str_file)
    str_name, str_extn = os.path.splitext(str_file.replace('\\', '/').split('/')[-1])
    return [str_file, str_path, str_name, str_extn]


# CHECK FILE EXISTENCE
def exist_file(str_file: str, err_index: bool = True) -> bool:
    if not os.path.isfile(str_file):
        err_index = False
        log_stderr(f'    >> file not found [{txt_truncate(str_file)}]')
    return err_index


# CHECK PATH EXISTENCE
def exist_path(str_path: str, err_index: bool = True) -> bool:
    if not os.path.isdir(str_path):
        err_index = False
        log_stderr(f'    >> path not found [{txt_truncate(str_path)}]')
    return err_index


# REDUCED PRINT
def txt_truncate(str_text: str, len_text: int = 50) -> str:
    out_text = str_text[-len_text:]
    pos_text = out_text.find('\\') if out_text.find('\\') > 0 else 0
    out_text = f"..{out_text[pos_text:] if len(str_text) > len_text else str_text}"
    return out_text


# ADD PATH
def add_path(cur_path: str, str_file: str) -> str:
    return f'{cur_path}\\{str_file}' if '\\' not in str_file else str_file


# DERIVE DISTANCE BAND
def dist_band(max_dist: float) -> np.ndarray:
    max_dist = int(max_dist + 1)
    num_band = int(max_dist ** 0.51)
    arr_dist = np.array([int(((0 if val == 0 else val + 1) / num_band) ** 2.2 * max_dist)
                         for val in range(num_band)])
    return arr_dist


# PRODUCT OF LIST ELEMENTS
def product(*args) -> List:
    return list(itt.product(*args))
