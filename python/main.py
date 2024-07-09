from mdlbuild import ClassifiedBuild
from mdloutput import Output
#from mdltriprate import TripRate  commented due to issue importing R functionality in mdltriprate
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
    data_fldr = r"F:\NTS\UKDA-7553-tab"

    # This folder must contain imports
    main_fldr = r"I:\NTS"

    cfg = cfg.Config(main_fldr)
    cb_version = "v_14"

    # classified build
    ClassifiedBuild(data_fldr, main_fldr, cb_version, False)

    # trip-rate model
    #TripRate(main_fldr, cb_version, True, run_rs=True, run_py=False)  commented due to line 3

    # NTS outputs
    Output(main_fldr, cb_version, False)

    # Tour Model
    TourModel(main_fldr, "v5", "county", True)

    # NTS stage analysis
    Stage(main_fldr, cb_version, False)
