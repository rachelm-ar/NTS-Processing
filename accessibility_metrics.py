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

highway = pd.read_csv(_highway_data)

singles = highway['Start Centroid ID'].drop_duplicates()

# Pivot to 8480 np arrays?

# Import jobs

# Pivot jobs to np array

# Filter x travel time

# Get subsets x MSOA

# Translate to Ward

