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
    # This is the folder containing the tab files
    data_fldr = r"C:\Users\NhanNguyen\OneDrive - Transport for the North\Projects\NTS\UKDA-7553-tab"

    # This folder must contain imports
    main_fldr = r"C:\Projects\NTS_Analysis"

    cfg = cfg.Config(main_fldr)
    cb_version = "5_test"

    # classified build
    ClassifiedBuild(data_fldr, main_fldr, cb_version, False)

    # trip-rate model
    TripRate(main_fldr, cb_version, True, run_rs=True, run_py=False)

    # NTS outputs
    Output(main_fldr, cb_version, True)

    # Tour Model
    TourModel(main_fldr, "region", True)

    # NTS stage analysis
    Stage(main_fldr, cb_version, False)
