# -*- coding: utf-8 -*-
"""
Created on Tue Mar 17 17:34:04 2020

Define accessibility metrics for mode based regression of mode share
Import basemap - translate to MSOA - translate to ward

@author: genie
"""

import os

import pandas as pd
import numpy as np

_highway_data = 'Y:/Data Strategy/Data/BaseMap/DriveTimeFullResult_v3/DriveTimeFullResult_v2.csv'
_rail_data = 'Y:/Data Strategy/Data/BaseMap/PT20112017/PT20112017.csv'

# Import travel time by MSOA OD pair
highway = pd.read_csv(_highway_data)

singles = highway['Start Centroid ID'].drop_duplicates()

# Pivot to 8480 np arrays?

# Import jobs
# file:///Y:/Data Strategy/Data/HSL/2018/non_freight_msoa_2018.csv

# Pivot jobs to np array

# Filter x travel time (60 mins)

outputs = []
for msoa in zones:
    # filter
    outputs.append(subset)

# Get subsets x MSOA (jobs accessible in 60 mins)

# Translate to Ward (best fit for each MSOA to ward) flag ones where <40% maybe
# file:///Y:/NorMITs Synthesiser/Zone Translation/Export/uk_ward_msoa_pop_weighted_lookup.csv

# Repeat for rail