from mdlbuild import ClassifiedBuild
from mdloutput import Output
from mdltriprate import TripRate
import mdlfunction as fun
import mdlconfig as cfg

# main application
if __name__ == '__main__':
    nts_fldr = r'D:\NTS\7553tab_2021\tab'
    out_fldr = r'D:\NTS\NoTEM\nts_data\2002_2021'

    cfg = cfg.Config(out_fldr)
    ver_build = 5

    # process NTS
    csv_build = f'{cfg.fld_build}\\{cfg.csv_build}_v{ver_build}.csv'
    ClassifiedBuild(nts_fldr, out_fldr, ver_build) if not fun.exist_file(csv_build) else None

    # NTS outputs
    Output(out_fldr, 5)

    # trip-rate model
    TripRate(out_fldr, 5)
