# -*- coding: utf-8 -*-
"""
Created on Tue Mar 17 17:34:04 2020

Define accessibility metrics for mode based regression of mode share
Import basemap - translate to MSOA - translate to ward

@author: genie
"""

import os

import pandas as pd

_highway_data = 'Y:/Data Strategy/Data/BaseMap/DriveTimeFullResult_v3/DriveTimeFullResult_v2.csv'
_rail_data = 'Y:/Data Strategy/Data/BaseMap/PT20112017/PT20112017.csv'
_jobs_data = 'Y:/NTS/accessibility_metric/non_freight_msoa_2018.csv'
_msoa_to_ward = 'Y:/NTS/accessibility_metric/msoa_to_ward_bestfit.csv'

# Read total jobs by MSOA and MSOA to ward best fit lookup
jobs = pd.read_csv(_jobs_data, usecols= ['geo_code','Total'])
msoa_ward_lookup = pd.read_csv(_msoa_to_ward)

# Highway job accessibility
# Import travel time by MSOA OD pair, drop columns not required
highway = pd.read_csv(_highway_data, usecols=['Start Centroid ID', 'End Centroid ID', 'Total Travel Time'])

# Filter by maximal travel time (set to 60 minutes)
max_travel_time = 60
highway = highway[highway['Total Travel Time'] <= max_travel_time]

# Read total jobs by MSOA and join job figures to highway MSOA
highway = highway.join(jobs.set_index('geo_code'), on='End Centroid ID')

# Sum total jobs per MSOA
highway = highway.groupby(['Start Centroid ID'])['Total'].sum()
highway = pd.DataFrame(highway)

# Join best ward fit and flag for <40% match for each MSOA and export to csv
highway = highway.join(msoa_ward_lookup.set_index('msoa_area_code'), on='Start Centroid ID')
highway.to_csv('jobs_by_car.csv')

# PT job accessibility
# Import travel time by MSOA OD pair, drop columns not required
pt = pd.read_csv(_rail_data, usecols=['Start Centroid ID', 'End Centroid ID', 'Total Travel Time'])

# Filter by maximal travel time (set to 60 minutes)
max_travel_time = 60
pt = pt[pt['Total Travel Time'] <= max_travel_time]

# Read total jobs by MSOA and join job figures to highway MSOA
pt = pt.join(jobs.set_index('geo_code'), on='End Centroid ID')

# Sum total jobs per MSOA
pt = pt.groupby(['Start Centroid ID'])['Total'].sum()
pt = pd.DataFrame(pt)

# Join best ward fit and flag for <40% match for each MSOA and export to csv
pt = pt.join(msoa_ward_lookup.set_index('msoa_area_code'), on='Start Centroid ID')
pt.to_csv('jobs_by_pt.csv')




#############

#singles = highway['Start Centroid ID'].drop_duplicates()

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