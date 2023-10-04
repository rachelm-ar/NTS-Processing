from mdlbuild import ClassifiedBuild
from mdloutput import Output
from mdltriprate import TripRate
import mdlconfig as cfg
import multiprocessing as mp
import matplotlib.pyplot as plt
plt.switch_backend('agg')
mp.freeze_support()


# main application
if __name__ == '__main__':
    nts_fldr = r'D:\NTS\7553tab_2021\tab'
    out_fldr = r'D:\NTS\NoTEM\nts_data\2002_2021'

    cfg = cfg.Config(out_fldr)
    cb_version = '5_test'

    # process NTS
    csv_build = f'{cfg.fld_cbuild}\\{cfg.csv_cbuild}_v{cb_version}.csv'
    ClassifiedBuild(nts_fldr, out_fldr, cb_version, True)

    # NTS outputs
    Output(out_fldr, cb_version)

    # trip-rate model
    TripRate(out_fldr, cb_version)
