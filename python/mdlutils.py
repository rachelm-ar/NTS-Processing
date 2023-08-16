import subprocess
from typing import Union, Dict, List
import os
import sys
import numpy as np
import itertools as itt


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
def list_to_dict(uni_list: Union[List, Dict], key_start: int = 0) -> Dict:
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
