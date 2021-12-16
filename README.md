![Transport for the North Logo](docs/TFN_Landscape_Colour_CMYK.png)

----

# NTS Processing

National Travel Survey (NTS) Processing  is Transport for the North's (TfN) suite of tools to build an NTS dataset from raw inputs via a special data licence supplied by the UK Data Service. Furthermore, there are a multitiude of Home Based (HB) and Non-Home Based (NHB) outputs these tools build, where the vast majority are inputs for the production model of TfN's Travel Marker Synthesiser within [NorMITs-Demand](https://github.com/Transport-for-the-North/NorMITs-Demand#travel-market-synthesiser)

#### Contents
 - [Summary](#summary) 
 - [Quick Start Guide](#quick-start-guide)
 - [Required Data](#required-data)
   - [NoTEM](#notem)
   - [TMS](#tms)
   - [EFS](#efs)
   - [Elasticity](#elasticity)
 - [Documentation](#documentation)
 - [Planned Improvements](#planned-improvements)
 - [Sharing](#sharing)
   - [Sharing Models](#sharing-models)
   - [Sharing Requests](#sharing-requests)
 - [Gory Details](#gory-details)
   - [Northern Trip End Model](#northern-trip-end-model)
   - [Travel Market Synthesiser](#travel-market-synthesiser)
   - [External Forecast System](#external-forecast-system)
   - [Elasticity](#elasticity-model)
   - [NorMITs Matrix Tools](#matrix-tools)

## [Summary](#contents)
NTS Processing is an ordered sequence, where the unclassified build becomes the input for the classified build which becomes the main input for all outputs. This is represented in the OP flow model below. A summary of each process is detailed in chronological order below:

1. [Unclassified Build](#unclassified-build) (**UB**) - Reads in raw NTS data and joins tables together by utilising the hierarchical nature of the NTS to flexibily create an 'unclassified build' by specifiying the tables and variables within each table to join.
2. [Classified Build](#classified-build) (**CB**) - 
3. [HB Trip Rates](#hb_trip-rates) - 
4. [HB Mode Time Splits](#hb-mts) - 
5. [HB Production Model](#hb-production-model) - 
6. [NHB Trip Rates](#nhb-trip-rates) - 
7. [NHB Time Splits](#nhb-time_splits) - 
8. [Car Occupancy](#car-occupancy) - 
9. [Trip Length Distributions](#tld)

One important requisite deserving explanation is the Lookup process implemented to flexibly adjust and/or add new variables.

![op_flow](docs/nts_op.png)

## [Quick Start Guide!](#contents)
In order to smoothly run the tools, follow the steps below:

1.	Open run_master.R and execute lines 1-12. If this is your first time, then this will create a folder 'NTS_C' and copy the required inputs from Y drive to your C drive within documents. For example: C:\Users\Pluto\Documents\NTS_C
2.	It is better to run with ‘drive = C’ whenever a function has the option as an argument due to slow speeds with the Y drive. This is particularly important for build_ub as raw NTS tsv’s are not saved on the Y drive.
3.	If granted access to raw NTS data, save within NTS_C. For example: C:\Users\Pluto\Documents\NTS_C\UKDA-7553-tab\tab

## [Required Data](#contents)
In order to run NTS Processing from start to finish, i.e. obtain all outputs, there are certain dependencies on inputs which are listed for each wrapper function.

### [Lookups](#contents)


![UB-op-flow](docs/nts_op_UB.png)
