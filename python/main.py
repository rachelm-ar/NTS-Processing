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

    # classified build
    ClassifiedBuild(nts_fldr, out_fldr, cb_version, True)

    # trip-rate model
    TripRate(out_fldr, cb_version)

    # NTS outputs
    Output(out_fldr, cb_version)
