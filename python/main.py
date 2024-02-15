from mdlbuild import ClassifiedBuild
from mdloutput import Output
from mdltriprate import TripRate
from mdlstage import Stage
from mdltour import TourModel
import mdlconfig as cfg
import multiprocessing as mp
import matplotlib.pyplot as plt

plt.switch_backend("agg")
mp.freeze_support()


# main application
if __name__ == "__main__":
    data_fldr = (
        r"E:\NTS\UKDA-7553-tab_22"  # This is the folder containing the tab files
    )
    # main_fldr = r'D:\NTS\NoTEM\nts_data\2002_2021'
    main_fldr = r"E:\NTS\analysis\22"  # This folder must contain imports, classified builds, and outputs

    cfg = cfg.Config(main_fldr)
    cb_version = "5_test"

    # classified build
    ClassifiedBuild(data_fldr, main_fldr, cb_version, False)

    # trip-rate model
    TripRate(main_fldr, cb_version, False, run_r=True, run_py=False)

    # NTS outputs
    Output(main_fldr, cb_version, True)

    # Tour Model
    TourModel(main_fldr, "region", True)

    # NTS stage analysis
    Stage(main_fldr, cb_version, True)
