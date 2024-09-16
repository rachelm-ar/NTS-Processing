# Existing packages
import pandas as pd
import os
import numpy as np
from datetime import datetime
from itertools import product

# TfN packages
from caf.distribute import furness

# Set model version
model_ver = 'v5'

# Set directories
inputs_dir = r'I:\NTS\imports\tour_adjust_imports'
msoa_dir = r'I:\NTS\imports'
tour_model_dir = r'I:\NTS\outputs\tour\reports'

# Set file names
odm_file = 'ODM_for_rdm_2022-23.csv'
msoa_county_file = 'msoa11cd_correspondence.csv'
stn_geo_file = 'station_attributes_on_TfN_geography.csv'
sector_file = 'bespoke_sectors_v1.1.csv'
lrtu_file = 'lrt0101.csv'
model_file = 'matrix_county_output.csv'

# Import data
odm_in_df = pd.read_csv(os.path.join(inputs_dir, odm_file))
msoa_county_in_df = pd.read_csv(os.path.join(msoa_dir, msoa_county_file))
stn_geo_in_df = pd.read_csv(os.path.join(inputs_dir, stn_geo_file))
sector_in_df = pd.read_csv(os.path.join(inputs_dir, sector_file))
lrtu_in_df = pd.read_csv(os.path.join(inputs_dir, lrtu_file), skiprows=7)
lrtu_in_df.columns = lrtu_in_df.columns.str.split('[').str[0].str.strip() # Some processing required here to make column names tidier
model_in_df = pd.read_csv(os.path.join(tour_model_dir, model_ver, model_file))


# Set light rail inputs
# Year for which to extract the Light Rail, Tramway and Underground data
lrtu_year_in = 2023
# Proportion of trips on the London Underground, London Trams and Docklands
# Light Railway that are considered to be "unique" (i.e. not double counted
# with another rail mode)
lrtu_london_scale_in = 0.25
# Proportion of trips on Light Rail, Tramway and Underground systems outside of
# London that are considered to be "unique" (i.e. not double counted with
# another rail mode)
lrtu_nonlondon_scale_in = 0.5

# For each Light Rail, Tramway or Underground system in GB,
# set the sector in which it is located.
# Done at sector level as some of these systems cross county borders
lrtu_systems_in = {
    'Docklands Light Railway': 'London',
    'London Trams': 'London',
    'Nottingham Express Transit': 'East Midlands North',
    'West Midlands Metro': 'West Midlands South',
    'Sheffield Supertram': 'South Yorkshire',
    'Tyne and Wear Metro': 'Tyne and Wear',
    'Manchester Metrolink': 'Greater Manchester',
    'Blackpool Tramway': 'Lancashire',
    'Edinburgh Trams': 'Scotland',
    'London Underground': 'London',
    'Glasgow Subway': 'Scotland'
}

# Set counties for stations that are located outside of the MSOA shapefile,
# so get missed off the correspondence. This is a table here in case the station
# shapefile is updated to add new stations

# Need to account for:
#  - Blackfriars (5112) - Bad join in the GIS as it's in the middle of the Thames
#  - Portsmouth Harbour (5540) - Bad join in the GIS as it's in the harbour
#  - Ryde Pier Head (5541) - Bad join as in the GIS as it's in the sea

# Counties to allocate stations to:
#  - Blackfriars -> Inner London (County 17)
#  - Portsmouth Harbour -> Hampshire (County 35)
#  - Ryde Pier Head -> Hampshire (County 35)

stn_county_infill_df = pd.DataFrame(
    columns=['National Location Code', 'county', 'county_nm'],
    data=[[5112, 17, 'Inner London'],
          [5540, 35, 'Hampshire'],
          [5541, 35, 'Hampshire']]
    )


def process_lrtu_data(
        lrtu_df, lrtu_year, lrtu_systems, lrtu_london_scale, lrtu_nonlondon_scale):
    """
    Process light rail, tramway and underground data to get an annual journey
    count (for "unique", i.e. not double counted with another rail mode) by
    sector

    Parameters
    ----------
    lrtu_df: pandas df
        Light Rail, Tramway and Underground annual journey data by system as
        read in by this script
    lrtu_year: int
        Year for which to extract the Light Rail, Tramway and Underground data
        It is the year in which the finacial year ends
        It should match the year for which the national rail odm is downloaded
    lrtu_systems: dict
        Dictionary relating each Light Rail, Tramway or Underground system in
        GB to the sector in which it is located
    lrtu_london_scale: float
        Expected range 0.0 to 1.0
        Proportion of trips on the London Underground, London Trams and
        Docklands Light Railway that are considered to be "unique" (i.e. not
        double counted with another rail mode)
    lrtu_nonlondon_scale: float
        Expected range 0.0 to 1.0
        Proportion of trips on Light Rail, Tramway and Underground systems
        outside of London that are considered to be "unique" (i.e. not double
        counted with another rail mode)

    Returns
    ----------
    lrtu_df: pandas df
        For the selected year, the estimate of the number of "unique" (i.e. not
        double counted with another rail mode) journeys by Light Rail, Tramway
        and Underground for the sectors in which such systems are located.
        This is an annual total
    """

    # Basic logic checks on inputs
    yearnow = datetime.now().year
    if (not 2013 < lrtu_year <= yearnow) or (type(lrtu_year) is not int):
        print('WARNING: Unexpected input year for Light Rail, Tramway and Underground data')
        print(f'Expected an interger year between 2014 and {yearnow}')
        print(f'Instead, got {lrtu_year}')
    if not 0 < lrtu_london_scale <= 1:
        print(
            'WARNING: London scaling factor expected to be greater than 0, less the or equal to 1')
        print(f'Instead got London scaling factor of {lrtu_london_scale}')
    if not 0 < lrtu_nonlondon_scale <= 1:
        print(
            'WARNING: Outside London scaling factor expected to be greater than 0, less the or equal to 1')
        print(f'Instead got outside London scaling factor of {lrtu_nonlondon_scale}')

    # Process to account for odd formatting of source
    lrtu_df = lrtu_df.dropna(axis=1, how='all')
    lrtu_df = lrtu_df.dropna(axis=0, how='all')
    lrtu_df = lrtu_df.rename(columns={'Financial year ending March': 'Year'})
    lrtu_df['Year'] = lrtu_df['Year'].astype(int)

    # Select data we are interested in and reformat to a system-based index
    lrtu_df = lrtu_df.loc[lrtu_df['Year'] == lrtu_year]
    lrtu_df = lrtu_df.set_index(['Year'])
    lrtu_df = lrtu_df.transpose().reset_index()
    lrtu_df = lrtu_df.rename_axis(None, axis=1)
    lrtu_df = lrtu_df.rename(
        columns={'index': 'System', lrtu_year: 'Yearly Journeys'})

    # Convert yearly journeys to absolutes (and make sure they are numeric!)
    # Note this bit will fall over if you pick a year before all systems
    #   were returning data (i.e. some cells are '[w]')
    lrtu_df['Yearly Journeys'] = lrtu_df['Yearly Journeys'].astype(str)
    lrtu_df['Yearly Journeys'] = lrtu_df['Yearly Journeys'].str.replace(
        ',', '')
    lrtu_df['Yearly Journeys'] = lrtu_df['Yearly Journeys'].astype(
        float) * 10  # Just clear float to minimise rounding error risk
    lrtu_df['Yearly Journeys'] = lrtu_df['Yearly Journeys'].astype(int)
    lrtu_df['Yearly Journeys'] = lrtu_df[
                                     'Yearly Journeys'] * 100000  # Not 1 million as we've times by 10 about to get out of float

    # Apply sectors to data
    lrtu_df['Sector'] = lrtu_df['System'].map(lrtu_systems)
    lrtu_df = lrtu_df.dropna(
        axis=0)  # Drop rows where system name is not found (expected to be some total rows like all of GB)
    if lrtu_df.shape[0] != len(lrtu_systems):
        print(
            'WARNING: The systems you have specified sectors for and the systems in the input file do not match!')
    lrtu_df = lrtu_df.groupby(
        ['Sector'])['Yearly Journeys'].sum().reset_index()

    # Apply scaling factors to account for overlap with other rail modes
    # (e.g. national rail, other light rail systems)
    lrtu_df['Yearly Journeys'] = lrtu_df['Yearly Journeys'] * np.where(
        lrtu_df['Sector'] == 'London', lrtu_london_scale, lrtu_nonlondon_scale)
    lrtu_df['Yearly Journeys'] = lrtu_df['Yearly Journeys'].astype(int)

    return lrtu_df


def rationalise_inputs(odm_df, msoa_county_df, stn_geo_df):
    """
    Cut input dfs down to just the columns of interest

    Parameters
    ----------
    odm_df: pandas df
        Origin-destination matrix for journeys on the national rail network
        between station pairs. Annual data for 1 year
    msoa_county_df: pandas df
        Lookup table to get from MSOA to County
    stn_geo_df: pandas df
        National rail stations with MSOA attached

    Returns
    ----------
    odm_df: pandas df
        Origin-destination matrix for journeys on the national rail network
        between station pairs. Annual data for 1 year. Columns cut down to just
        those required by other functions
    msoa_county_df: pandas df
        Lookup table to get from MSOA to County. Columns cut down to just those
        required by other functions
    stn_geo_df: pandas df
        National rail stations with MSOA attached. Columns cut down to just
        those required by other functions
    """

    # nlc (National Location Code) is a unique numerical code for each station
    odm_df = odm_df[['origin_nlc',
                     'origin_station_name',
                     'destination_nlc',
                     'destination_station_name',
                     'journeys']]
    msoa_county_df = msoa_county_df[['msoa11cd',
                                     'county',
                                     'county_nm']]
    stn_geo_df = stn_geo_df[['National Location Code', 'msoa11cd']]

    return odm_df, msoa_county_df, stn_geo_df


def process_station_geography(msoa_county_df, stn_geo_df, stn_infill_df):
    """
    Join each national rail station to the county in which they lie

    Parameters
    ----------
    msoa_county_df: pandas df
        Lookup table to get from MSOA to County. Columns cut down to just those
        required by this function
    stn_geo_df: pandas df
        National rail stations with MSOA attached. Columns cut down to just
        those required by this function
    stn_infill_df: pandas df
        Table assigning stations outside of MSOAs to their counties

    Returns
    ----------
    stn_geo_df: pandas df
        Table relating all active national rail stations to their county
    """

    # Assign counties to stations that are allocated MSOAs by the geospatial
    # processing
    stn_geo_df = stn_geo_df.merge(msoa_county_df, how='left', on='msoa11cd')
    stn_geo_df = stn_geo_df.drop(columns=['msoa11cd'], axis=1)

    # Add on the stations that exist outside of the MSOA shapefile
    # Drop rows containing nulls
    if stn_geo_df[stn_geo_df.isnull().any(axis=1)].shape == stn_county_infill_df.shape:
        # We are infilling something the same size as the NULL rows,
        # which we want to do
        # Drop the NULL rows, then append the replacements
        stn_geo_df = stn_geo_df.dropna(how='any', axis=0)
        stn_geo_df = pd.concat([stn_geo_df, stn_county_infill_df])
        stn_geo_df.reset_index(inplace=True, drop=True)
    else:
        print(
            'WARNING: The NULL infilling table you are trying to append is not the same dimensions as the NULL rows in the table')
        print('Operation therefore not attempted and NULL rows are still in place')

    # Rename the National Location Code to make it a bit less unweildly
    stn_geo_df = stn_geo_df.rename(columns={'National Location Code': 'nlc'})

    return stn_geo_df


def make_sector_rail_odm(process_odm_df, stn_geo_df, sector_df):
    """
    Make the sectorised national rail odm

    Parameters
    ----------
    process_odm_df: pandas df
        Origin-destination matrix for journeys on the national rail network
        between station pairs. Annual data for 1 year. Columns cut down to just
        those required by this function
    stn_geo_df: pandas df
        Table relating all active national rail stations to their county
    sector_df: pandas df
        County to sector correspondence

    Returns
    ----------
    sector_odm_df: pandas df
        Column matrix of all sector origin-destination movemnets from the
        national rail ticketing data
    county_rows: int
        Row count of county level matrix
    sector_rows: int
        Row count of sector level matrix
    """

    # Join geography to ODM
    process_odm_df = process_odm_df.merge(
        stn_geo_df, how='left', left_on='origin_nlc', right_on='nlc')
    process_odm_df = process_odm_df.drop(columns=['nlc'], axis=1)
    process_odm_df = process_odm_df.rename(
        columns={'county': 'origin_county_code',
                 'county_nm': 'origin_county_name'})

    process_odm_df = process_odm_df.merge(
        stn_geo_df, how='left', left_on='destination_nlc', right_on='nlc')
    process_odm_df = process_odm_df.drop(columns=['nlc'], axis=1)
    process_odm_df = process_odm_df.rename(
        columns={'county': 'destination_county_code',
                 'county_nm': 'destination_county_name'})

    # Groupby on county level geographies, summing journeys and dropping the
    # station details
    process_odm_df = process_odm_df.groupby(
        ['origin_county_code',
         'origin_county_name',
         'destination_county_code',
         'destination_county_name']
    )['journeys'].sum().reset_index()

    # Now aggregate to Sector level using the Tour Model Output County to
    # sector correspondence
    sector_df = sector_df[['county', 'Sector_ID', 'Sector']]
    sector_odm_df = process_odm_df.merge(
        sector_df, how='left', left_on='origin_county_name', right_on='county')
    sector_odm_df = sector_odm_df.drop(columns=['county'], axis=1)
    sector_odm_df = sector_odm_df.rename(
        columns={'Sector_ID': 'origin_sector_id',
                 'Sector': 'origin_sector_name'})

    sector_odm_df = sector_odm_df.merge(
        sector_df,
        how='left',
        left_on='destination_county_name',
        right_on='county'
    )
    sector_odm_df = sector_odm_df.drop(columns=['county'], axis=1)
    sector_odm_df = sector_odm_df.rename(
        columns={'Sector_ID': 'destination_sector_id',
                 'Sector': 'destination_sector_name'})

    # Groupby on sector level geographies, summing journeys and dropping the
    # county details
    sector_odm_df = sector_odm_df.groupby(
        ['origin_sector_id',
         'origin_sector_name',
         'destination_sector_id',
         'destination_sector_name']
    )['journeys'].sum().reset_index()

    # Calculate row counts for checking
    county_rows = process_odm_df.shape[0]
    sector_rows = sector_odm_df.shape[0]

    return sector_odm_df, county_rows, sector_rows


def check_odm_processing(
        county_rows, sector_rows, msoa_county_df, sector_df, sector_odm_df, odm_df):
    """
    Check the odm processing worked correctly

    Parameters
    ----------
    county_rows: int
        Row count of county level matrix
    sector_rows: int
        Row count of sector level matrix
    msoa_county_df: pandas df
        Lookup table to get from MSOA to County
    sector_odm_df: pandas df
        Column matrix of all sector origin-destination movemnets from the
        national rail ticketing data
    odm_df: pandas df
        Origin-destination matrix for journeys on the national rail network
        between station pairs. Annual data for 1 year

    Returns
    ----------
    None
    """

    # Check if we've got all o/d movements at the county level
    # df should have number of counties x number of counties as the row count
    expected_rows = msoa_county_df['county'].nunique() ** 2
    if expected_rows == county_rows:
        print('County table dimensions are as expected')
    else:
        print(
            f'WARNING: Expected {str(expected_rows)} rows in the county table, got {str(county_rows)} rows')

    # Check if we've got all o/d movements at the sector level
    # df should have number of sectors x number of sectors as the row count
    expected_rows = sector_df['Sector_ID'].nunique() ** 2
    if expected_rows == sector_rows:
        print('Sector table dimensions are as expected')
    else:
        print(
            f'WARNING: Expected {str(expected_rows)} rows in the sector table, got {str(sector_rows)} rows')

    # Check we've not dropped any journeys
    input_journeys = odm_df['journeys'].sum()
    output_journeys = sector_odm_df['journeys'].sum()
    if input_journeys == output_journeys:
        print(f'The {str(input_journeys)} National Rail journeys input are all accounted for')
    else:
        print(
            f'WARNING: {str(input_journeys)} were input, but {str(output_journeys)} were output in the sector table!')


def add_lrtu_to_national_rail(
        sector_odm_hlr_df, lrtu_df):
    """
    Add Light Rail, Tramway and Underground trips to intrasector cells of
    the main o/d sector matrix

    Parameters
    ----------
    sector_odm_hlr_df: pandas df
        Column matrix of all sector origin-destination movemnets from the
        national rail ticketing data
    lrtu_df: pandas df
        For the selected year, the estimate of the number of "unique" (i.e. not
        double counted with another rail mode) journeys by Light Rail, Tramway
        and Underground for the sectors in which such systems are located.
        This is an annual total

    Returns
    ----------
    sector_odm_hlr_df: pandas df
        Column matrix of all sector origin-destination movemnets from the
        national rail ticketing data, with the light rail, tramway and
        underground data joined on the related intrasector movements
    square_s_odm_df: pandas df
        Square matrix of all sector origin-destination movemnets from the
        national rail ticketing data, with the light rail, tramway and
        underground data joined on the related intrasector movements
    square_s_weekly_odm_df: pandas df
        Square matrix of all sector origin-destination movemnets from the
        national rail ticketing data, with the light rail, tramway and
        underground data joined on the related intrasector movements, scaled to
        be weekly to match the Tour Model outputs
    """
    hr_total = sector_odm_hlr_df['journeys'].sum()
    lrtu_total = lrtu_df['Yearly Journeys'].sum()

    sector_odm_hlr_df['Sector'] = np.where(
        sector_odm_hlr_df['origin_sector_id'] == sector_odm_hlr_df['destination_sector_id'],
        sector_odm_hlr_df['origin_sector_name'], '-')
    sector_odm_hlr_df = sector_odm_hlr_df.merge(
        lrtu_df, how='left', on='Sector')
    sector_odm_hlr_df['Yearly Journeys'] = sector_odm_hlr_df['Yearly Journeys'].fillna(0).astype(
        int)
    sector_odm_hlr_df['journeys'] = (sector_odm_hlr_df['journeys'] +
                                     sector_odm_hlr_df['Yearly Journeys'])
    sector_odm_hlr_df = sector_odm_hlr_df.drop(
        ['Sector', 'Yearly Journeys'], axis=1)

    hrlrtu_total = sector_odm_hlr_df['journeys'].sum()
    if hr_total + lrtu_total != hrlrtu_total:
        print(
            'WARNING: Unexpected mismatch between National Rail and other rail totals with their sum!')

    # Make square
    square_s_odm_df = sector_odm_hlr_df.pivot(
        index=['origin_sector_id', 'origin_sector_name'],
        columns=['destination_sector_id', 'destination_sector_name'],
        values='journeys'
    )

    # Make square matrix weekly (to match Tour Model Output)
    square_s_weekly_odm_df = square_s_odm_df / 52

    return sector_odm_hlr_df, square_s_odm_df, square_s_weekly_odm_df


def make_model_rail_mat(model_df, sector_df):
    """
    Process the model outputs to get a square matrix that matches the format and
    dimensions of the rail ticketing data matrix. Also produce a matrix for
    scaling to subsets of the overall df later.

    Parameters
    ----------
    model_input: pandas df
        The county level o/d matrix in stack format exported by the tour model.
        This contains additional modes, as well as trip purposes and time
        periods
    sector_df: pandas df
        County to sector correspondence

    Outputs
    ----------
    rail_mat: pandas df
        Square rail matrix derived from the tour model. Set to the county level
    pdt_props_df: pandas df
        For each purpose, direction and time period combination, this df lists
        the proportion of rail trips relative to all rail trips
    """
    # Filter to rail only
    rail_mat = model_df[model_df['mode'] == 'Rail']
    sector_df = sector_df[['county', 'Sector', 'Sector_ID']]

    # Make purpose, directrion and time period proportions
    pdt_props_df = rail_mat.groupby(['purpose',
                                     'direction',
                                     'period']
                                    )['trips'].sum().reset_index()
    rail_trip_tot = pdt_props_df['trips'].sum()
    pdt_props_df['proportion'] = pdt_props_df['trips'] / rail_trip_tot
    pdt_props_df = pdt_props_df.drop(columns=['trips'])

    # Make square, county level tour model output rail matrix
    rail_mat = rail_mat.rename(
        columns={'tmz_o': 'county_origin', 'tmz_d': 'county_destination'})
    for d in ['destination', 'origin']:
        rail_mat = rail_mat.merge(sector_df,
                                  left_on='_'.join(['county', d]),
                                  right_on='county', how='left')
        colname_s = '_'.join([d, 'sector_name'])
        colname_si = '_'.join([d, 'sector_id'])
        rail_mat = rail_mat.rename(columns={'Sector': colname_s,
                                            'Sector_ID': colname_si})
        rail_mat = rail_mat.drop(columns=['county'])
    rail_mat = rail_mat[['destination_sector_name',
                         'destination_sector_id',
                         'origin_sector_name',
                         'origin_sector_id',
                         'trips']]
    rail_mat = rail_mat.groupby(['destination_sector_name',
                                 'destination_sector_id',
                                 'origin_sector_name',
                                 'origin_sector_id']
                                )['trips'].sum().reset_index()
    rail_mat = rail_mat.pivot(
        index=['origin_sector_id', 'origin_sector_name'],
        columns=['destination_sector_id', 'destination_sector_name'],
        values='trips'
    )

    rail_mat = rail_mat.fillna(0)  # Fill missing data with 0 (i.e. no trips)
    rail_mat = rail_mat.reindex(sorted(rail_mat.columns), axis=1)

    return rail_mat, pdt_props_df


furness_params = {
    'tolerance': 1e-9,
    'max_iterations': 5000,
    'warning': True
}


def furness_rail(ticket_mat, rail_mat, furness_params):
    """
    Furness the tour model output rail matrix to get the patterns from the
    ticketing data. Expect this to fill in the blanks (0s) in the rail matrix

    Parameters
    ----------
    ticket_mat: pandas df
        Target matrix for the furness process. Extracted from rail ticketing
        data
    rail_mat: pandas df
        Seed data for the furness process. These are the tour model output rail
        trips
    furness_params: dict
        Contains tolerance, maximum iterations and whether warnings are shown by
        the furness process

    Outputs
    ----------
    f_mat_df: pandas df
        Matrix output from the Furness process
    ticket_mat: pandas df
        The ticketing data matrix as seen by the furness process to enable
        before and after comparison
    rail_mat: pandas df
        The tour model rail outputs as seen by the furness process to enable
        before and after comparison
    """
    # Scale ticketing data to same total as tour model output
    ticket_tot = ticket_mat.sum().sum()
    tour_tot = rail_mat.sum().sum()
    ticket_mat = sq_s_weekly_odm_df * (tour_tot / ticket_tot)

    # Initialise furness process
    # Check this is the correct way around...
    row_targets = ticket_mat.sum(axis=0).reset_index(drop=True).to_numpy()
    col_targets = ticket_mat.sum(axis=1).reset_index(drop=True).to_numpy()
    seed_vals = rail_mat.replace(0, 1e-10).to_numpy()

    tol = furness_params['tolerance']
    max_iters = furness_params['max_iterations']
    warning = furness_params['warning']

    f_mat, iteration, rmse = furness.doubly_constrained_furness(seed_vals,
                                                                row_targets,
                                                                col_targets,
                                                                tol,
                                                                max_iters,
                                                                warning)
    print(f'Reached iteration {iteration} of {max_iters}')
    print(f'With an RMSE of {rmse}')

    f_mat_df = pd.DataFrame(data=f_mat,
                            index=ticket_mat.index,
                            columns=ticket_mat.columns)

    return f_mat_df, ticket_mat, rail_mat


def p_d_t_disagg(f_mat_lst, pdt_prop_mat):
    """
    Convert the square matrix output from furness process to a column matrix.
    Then apply propotions by purpose, direction and time period for each
    purpose, direction and time period combination to calculate trips by
    origin, destination, purpose, direction and time period.

    Parameters
    ----------
    f_mat_lst: pandas df
        Matrix output from the furness (in square format)
    pdt_props_mat: pandas df
        Proportions for each unique p, d, t combination output earlier in the
        process to be allocated to the trips by o and d.

    Outputs
    ----------
    dim_df: pandas df
        Column matrix of every o, d, p, d, t combination with the trips by o, d
        only; the p, d, t proportions; and the trips by o, d, p, d, t as
        columns.
    """
    in_trip_count = sq_furnessed_rail_matrix.sum().sum()

    # Return to column matrix
    f_mat_lst = f_mat_lst.stack(
        list(range(sq_furnessed_rail_matrix.columns.nlevels))).reset_index()
    f_mat_lst = f_mat_lst.rename(columns={0: 'trips'})

    # Make a df containing all possible o, d, p, d, t combinations for m = rail
    o_sec_names = list(f_mat_lst['origin_sector_name'].unique())
    d_sec_names = list(f_mat_lst['destination_sector_name'].unique())
    purps = list(pdt_prop_mat['purpose'].unique())
    dirs = list(pdt_prop_mat['direction'].unique())
    tps = list(pdt_prop_mat['period'].unique())

    dim_df = pd.DataFrame(product(o_sec_names, d_sec_names, purps, dirs, tps))
    dim_df = dim_df.rename(columns={
        0: 'origin_sector_name',
        1: 'destination_sector_name',
        2: 'purpose',
        3: 'direction',
        4: 'period'
    })
    # Output of this should have 97,344 rows, the product of:
    # 26 origin sectors
    # 26 destination sectors
    # 8 purposes
    # 3 directions
    # 6 periods

    # Join furnessed rail data back to the df made above
    dim_df = dim_df.merge(f_mat_lst,
                          on=['origin_sector_name', 'destination_sector_name'],
                          how='left'
                          )
    dim_df = dim_df[['origin_sector_id',
                     'origin_sector_name',
                     'destination_sector_id',
                     'destination_sector_name',
                     'purpose',
                     'direction',
                     'period',
                     'trips']]
    dim_df = dim_df.rename(columns={'trips': 'o_d_trips'})

    # Join p, d, t proportions to df made above
    dim_df = dim_df.merge(pdt_prop_mat,
                          on=['purpose', 'direction', 'period'],
                          how='left'
                          )

    # Add a column for o, d, p, d, t trips
    dim_df['o_d_p_d_t_trips'] = dim_df['o_d_trips'] * dim_df['proportion']

    # Check trip count
    out_trip_count = dim_df['o_d_p_d_t_trips'].sum()
    print(f'{in_trip_count} trips were input in the square matrix by o and d only')
    print(f'{out_trip_count} trips were output in the stack matrix by o, d, p, d and t')
    print(
        'An exact match here is difficult to obtain with float operations, so test for a diff of 1e-8 or less')
    if abs(in_trip_count - out_trip_count) < 1e-8:
        print(
            'Trips input and output to the p, d ,t diagreation process match within the tolerance')
    else:
        print(
            'WARNING: Trips input and output to the p, d ,t diagreation process match within the tolerance!')

    return dim_df

if __name__ == "__main__":
    lrtu_df_processed = process_lrtu_data(
        lrtu_in_df,
        lrtu_year_in,
        lrtu_systems_in,
        lrtu_london_scale_in,
        lrtu_nonlondon_scale_in
    )
    odm_df_rat, msoa_county_df_rat, stn_geo_df_rat = rationalise_inputs(
        odm_in_df,
        msoa_county_in_df,
        stn_geo_in_df
    )
    stn_geo_df_processed = process_station_geography(
        msoa_county_df_rat,
        stn_geo_df_rat,
        stn_county_infill_df
    )
    sector_odm_df_processed, c_rows, s_rows = make_sector_rail_odm(
        odm_df_rat,
        stn_geo_df_processed,
        sector_in_df
    )
    check_odm_processing(
        c_rows,
        s_rows,
        msoa_county_df_rat,
        sector_in_df,
        sector_odm_df_processed,
        odm_df_rat
    )
    s_odm_hlr_df, sq_s_odm_df, sq_s_weekly_odm_df = add_lrtu_to_national_rail(
        sector_odm_df_processed,
        lrtu_df_processed
    )
    sq_rail_mat, pdt_props = make_model_rail_mat(
        model_in_df,
        sector_in_df
    )
    sq_furnessed_rail_matrix, sq_ticket_mat, sq_rail_mat = furness_rail(
        sq_s_weekly_odm_df,
        sq_rail_mat,
        furness_params
    )
    cl_furnessed_rail_matrix_by_pdt = p_d_t_disagg(
        sq_furnessed_rail_matrix,
        pdt_props
    )

    sq_furnessed_rail_matrix.to_csv(
        r'I:\NTS\outputs\tour\constrain_prototype_data\rail\v1\01_Furnessed_Square_Rail_Matrix.csv')
    sq_ticket_mat.to_csv(
        r'I:\NTS\outputs\tour\constrain_prototype_data\rail\v1\00_Square_Weekly_Ticket+_Data.csv')
    sq_rail_mat.to_csv(
        r'I:\NTS\outputs\tour\constrain_prototype_data\rail\v1\00_Square_Tour_Model_Rail_Furness_Input.csv')
    cl_furnessed_rail_matrix_by_pdt.to_csv(
        r'I:\NTS\outputs\tour\constrain_prototype_data\rail\v1\01_Furnessed_Column_Rail_Matrix_by_p_d_t.csv')

    print('Done')