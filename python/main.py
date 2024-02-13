from mdlbuild import ClassifiedBuild
from mdloutput import Output
from mdltriprate import TripRate
from mdlstage import Stage
from mdltour import TourModel
import mdlconfig as cfg
import multiprocessing as mp
import matplotlib.pyplot as plt
plt.switch_backend('agg')
mp.freeze_support()


# main application
if __name__ == '__main__':
    nts_fldr = r'E:\NTS\UKDA-7553-tab_22'
    # out_fldr = r'D:\NTS\NoTEM\nts_data\2002_2021'
    out_fldr = r'E:\NTS\analysis\22'

    cfg = cfg.Config(out_fldr)
    cb_version = '5_test'

    # classified build
    ClassifiedBuild(nts_fldr, out_fldr, cb_version, True)

    # trip-rate model
    TripRate(out_fldr, cb_version, True)

    # NTS outputs
    Output(out_fldr, cb_version, True)

    # Tour Model
    TourModel(out_fldr, 'region', True)

    # NTS stage analysis
    Stage(out_fldr, cb_version, True)
