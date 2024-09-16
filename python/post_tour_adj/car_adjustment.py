import pandas as pd
import os
import numpy as np

# Set model version
model_ver = 'v5'

# Set directories
inputs_dir = r'I:\NTS\imports\tour_adjust_imports'
normits_dir = r'I:\NorMITs Demand\Distribution Model\iter9.10.5\car_and_passenger\Upper Model\Matrices'
msoa_dir = r'I:\NTS\imports'
tour_model_dir = r'I:\NTS\outputs\tour\reports'

# Set file names
msoa_county_file = 'msoa11cd_correspondence.csv'
sector_file = 'bespoke_sectors_v1.1.csv'
normits_file = 'hb_synthetic_pa_yr2018_p1_m3.csv.bz2'
tour_model_file = 'matrix_county_output.csv'

# Import data
msoa_county_in_df = pd.read_csv(os.path.join(msoa_dir, msoa_county_file))
sector_in_df = pd.read_csv(os.path.join(inputs_dir, sector_file))
normits_in_df = pd.read_csv(os.path.join(normits_dir, normits_file), compression='bz2', sep=',')
tour_in_df = pd.read_csv(os.path.join(tour_model_dir, model_ver, tour_model_file))

# Set purpose, mode, direction, and time period for prototype example filtering
# This is set to Car, Commuting, home-based from, AM period
filter_dct = {'purpose': 'Commuting',
             'mode': 'Car',
             'direction': 'hb_fr',
             'period': 1}

# Set output version and folder locations
adj_ver = 'v2'
output_dir = r'I:\NTS\outputs\tour\constrain_prototype_data\car'
output_file = '01_adjusted_car_trips.csv'

if not os.path.exists(os.path.join(output_dir, adj_ver)):
    os.makedirs(os.path.join(output_dir, adj_ver))


def geo_agg(df, agg_df, left_dict, right_dict, right_rename, o_d, group_dict, group_list=[]):
    """
    Aggregate up geospatially to a higher level based on
    onto a dataframe df via column col_name,
    labelling as either origin merge or destination merge

    Parameters
    ----------
    df: pandas df
        Data at lower geospatial granularity than geo_agg level,
        to be aggregated
    agg_df: pandas df
        Correspondence to aggregate via
    left_dict: dict
        Dictionary of join columns for df
    right_dict: dict
        Dictionary of join columns for agg_df
    right_rename: st
        Field to rename from right table post-join
    o_d: str
        Can be either 'o', 'd', 'both' to represent origin agg,
        destination agg, or aggregation via both respectively
    group_dict: dict
        Dictionary of aggregation for groupby
    group_list: list
        List of fields to groupby, default is empty list
    """

    # Check that o_d is valid value, if not raise a warning
    if not o_d in ['o', 'd', 'both']:
        print('WARNING: Unexpected value for o_d parameter, function likely to error')

    # Get totals for checks
    check_in = df.agg(group_dict)

    # Merge on correspondence to higher geospatial granularity
    if o_d == 'both':
        # Reassign the origin zones to agg level
        df = pd.merge(df, agg_df, left_on=left_dict["o"], right_on=right_dict["o"], how='left')
        df.rename(columns={right_rename: right_rename + '_o'}, inplace=True)
        df.drop([right_dict["o"]], axis=1, inplace=True)

        # Reassign the destination zones to agg level
        df = pd.merge(df, agg_df, left_on=left_dict["d"], right_on=right_dict["d"], how='left')
        df.rename(columns={right_rename: right_rename + '_d'}, inplace=True)
        df.drop([right_dict["d"]], axis=1, inplace=True)

        # Groupby agg level
        group_list.append(right_rename + '_o')
        group_list.append(right_rename + '_d')
        df = df.groupby(group_list).agg(group_dict).reset_index()

    else:
        # Reassign the o_d zones to agg level, dependent on use input
        df = pd.merge(df, agg_df, left_on=left_dict[o_d], right_on=right_dict[o_d], how='left')
        df.rename(columns={right_rename: right_rename + '_' + o_d}, inplace=True)
        df.drop([right_dict[o_d]], axis=1, inplace=True)

        # Groupby agg level
        group_list.append(right_rename + '_' + o_d)
        df = df.groupby(group_list).agg(group_dict).reset_index()

    # Calculate check to ensure input totals meet output totals within acceptable tolerance level
    check_out = df.agg(group_dict)
    for i in check_in.index:
        if abs(check_in.loc[i] - check_out.loc[i]) < 1e-3:
            print(f'SUCCESS: geo_agg has aggregated {i} correctly')
        else:
            print(
                f'WARNING: geo_agg input versus output for aggregation of {i} not matching, recommend investigation')

    return df


def calc_props(df, dirr):
    """
    Calculate the proportion of O-D trips per origin for dirr = hb_fr, nhb
    and per destination for dirr = hb_to
    Calculated per direction, mode, purpose, period at the sector level

    Parameters
    ----------
    df: pandas df
        Tour model data for row-wise / col-wise proportions to be calculated
        (where row-wise / col-wise refers to if the matrices were represented
        in square format rather than long & thin format)
    dirr: str
        Can either be 'hb_fr', 'nhb', 'hb_to', represents the direction
        for the proportion to be calculated over
    """

    # Check that dirr is valid value, if not raise a warning
    if not dirr in ['hb_fr', 'nhb', 'hb_to']:
        print('WARNING: Unexpected value for dirr parameter, function likely to error')

    if (dirr == 'hb_fr') | (dirr == 'nhb'):
        # Calculate row-wise proportion by calculating row-wise total trips,
        # then dividing trips by the total
        df_total = df.groupby(['direction', 'mode', 'purpose', 'period', 'Sector_o'])[
            'trips'].sum().reset_index()
        df_total.rename(columns={'trips': 'total_trips'}, inplace=True)
        df = pd.merge(df, df_total, on=['direction', 'mode', 'purpose', 'period', 'Sector_o'],
                      how='left')
        df['proportion'] = df.trips / df.total_trips

    else:
        # Calculate row-wise proportion by calculating col-wise total trips,
        # then dividing trips by the total
        df_total = df.groupby(['direction', 'mode', 'purpose', 'period', 'Sector_d'])[
            'trips'].sum().reset_index()
        df_total.rename(columns={'trips': 'total_trips'}, inplace=True)
        df = pd.merge(df, df_total, on=['direction', 'mode', 'purpose', 'period', 'Sector_d'],
                      how='left')
        df['proportion'] = df.trips / df.total_trips

    # Check to ensure proportions are <= 1
    if df['proportion'].max() > 1:
        print('WARNING: Some calculated proportions > 1, recommend investigation')

    print('Tour model row/col-wise trip proportions calculated')
    return df


def marker(row):
    """
    Adds a marker as follows:
        if the proportion is < 0.05, set marker = 'Rest'
        else set marker = Sector_d or Sector_o based on direction
    This allows detail of major movements (>5% of proportion) to be retained
    whilst minor and zero movements are considered as a single block

    Parameters
    ----------
    row: pandas df row
        Row-wise mapping function takes rows as standard input
    """

    # Implement marker logic for 'hb_fr','nhb' directions
    if row['direction'] in ['hb_fr', 'nhb']:
        if row['proportion'] < 0.05:
            return 'Rest'
        else:
            return row['Sector_d']

    # Implement marker logic for 'hb_to' directions
    elif row['direction'] == 'hb_to':
        if row['proportion'] < 0.05:
            return 'Rest'
        else:
            return row['Sector_o']


def agg_to_marker(df, dirr):
    """
    Aggregates data to Marker level, rather than sector level
    Applied direction-wise, as for hb_fr, nhb directions the aggregation is
    done at the Sector_o, Marker level whilst for hb_to direction the
    aggregation is done at the Sector_d, Marker level
    Calculated per direction, mode, purpose, period

    Parameters
    ----------
    df: pandas df
        Tour model data after marker() has been applied
    dirr: str
        Can be 'hb_fr', 'hb_to', 'nhb', is direction for data to
        be filtered by
    """

    # Check that dirr is valid value, if not raise a warning
    if not dirr in ['hb_fr', 'nhb', 'hb_to']:
        print('WARNING: Unexpected value for dirr parameter, function likely to error')

    # Filter df for only the considered direction
    df = df.loc[df['direction'] == dirr]

    if dirr in ['hb_fr', 'nhb']:
        # Groupby the sector field and relevant direction, summing proportion and trips
        # to aggregate to Marker level
        df = df.groupby(['Sector_o', 'Marker', 'direction', 'purpose', 'mode', 'period'],
                        as_index=False).agg({
            'trips': 'sum',
            'proportion': 'sum'
        })
        # Reassign the Marker field to either sector_d or sector_o, dependent on direction
        df['Sector_d'] = df['Marker']

        # Check proportions sum to 1 across Sector_o
        check = df.groupby(['Sector_o', 'purpose', 'mode', 'period', 'direction'])[
            'proportion'].sum().reset_index()
        check_count = check.loc[~check['proportion'].between(0.99999, 1.00001)].shape[0]

    else:
        # Groupby the sector field and relevant direction, summing proportion and trips
        # to aggregate to Marker level
        df = df.groupby(['Sector_d', 'Marker', 'direction', 'purpose', 'mode', 'period'],
                        as_index=False).agg({
            'trips': 'sum',
            'proportion': 'sum'
        })
        # Reassign the Marker field to either sector_d or sector_o, dependent on direction
        df['Sector_o'] = df['Marker']

        # Check proportions sum to 1 across Sector_d
        check = df.groupby(['Sector_d', 'purpose', 'mode', 'period', 'direction'])[
            'proportion'].sum().reset_index()
        check_count = check.loc[~check['proportion'].between(0.99999, 1.00001)].shape[0]

    # Check to ensure proportions are <= 1
    if df['proportion'].max() > 1:
        print('WARNING: Some calculated proportions > 1, recommend investigation')

    if check_count > 0:
        print('WARNING: Sum of proportions not equal to 1 per block, recommend investigation')

    print(f'{dirr}: Markers and associated proportions calculated')
    return df


def create_sector_marker_idx(df_tour, df_normits, filt_dct):
    """
    Generates a unique sector_marker index based on filtered tour model outputs
    to ensure this is relevant, as sector_marker index could change based on
    filtering re mode, purpose, direction, time period etc.

    Parameters
    ----------
    df_tour: pandas df
        Tour model data to generate index from
    df_normits: pandas df
        NorMITs Demand Distribution model data, used to get distinct list of all
        possible O-D pairs
    filt_dct: dict
        Any filters to be applied to tour model data prior to sector_marker
        index being calculated
    """

    # Get unique list of all possible O-D pairs from NorMITs outputs
    od_complete = df_normits[['Sector_o', 'Sector_d']].drop_duplicates()

    # Filter df_tour as specified in filt_dct
    for k in filt_dct.keys():
        df_tour = df_tour.loc[df_tour[k] == filt_dct[k]]
        df_tour.drop([k], axis=1, inplace=True)

    # Generate sector_marker index by joining filtered tour combinations to all possible O-D pairs
    # Infill any gaps as 'Rest', as this implies there were no trips in the tour model i.e. this is
    # a minor/nonexistent movement for filtered tour outputs
    tour_odm_idx = df_tour[['Sector_o', 'Sector_d', 'Marker']].drop_duplicates()
    tour_odm_idx = od_complete.merge(tour_odm_idx, on=['Sector_o', 'Sector_d'], how='left').fillna(
        'Rest')

    return df_tour, tour_odm_idx


def calculate_trips_adj(sector_marker_idx, df_tour, df_normits, dirr):
    """
    Calculates the adjusted NorMITs distribution trips using tour proportions
    Calculations are directional as 'hb_fr','nhb' index over origin whilst 'hb_to'
    indexes over destination

    Parameters
    ----------
    sector_marker_idx: pandas df
        Index of unique distinct sector_marker combinations for given filters
        defined in filter_dct
    df_tour: pandas df
        Filter tour data
    df_normits: pandas df
        Pre-processed and NorMITs data
    diff: str
        Direction for calculations to be run over
    """

    # Capture input trip volumes for check
    check_in = df_normits.synth_trips.sum()

    # Merge NorMITs data onto the sector_marker index
    df = pd.merge(sector_marker_idx, df_normits, on=['Sector_o', 'Sector_d'], how='left')

    # Calculations are origin-wise if direction = 'hb_fr','nhb'
    if dirr in ['hb_fr', 'nhb']:
        sector_col = 'Sector_o'
        sector_col_other = 'Sector_d'
    else:
        sector_col = 'Sector_d'
        sector_col_other = 'Sector_o'

    # Merge on filtered and 'markered' tour data
    df = pd.merge(df, df_tour, left_on=[sector_col, 'Marker'],
                  right_on=[sector_col, sector_col_other], how='left')
    df = df.drop(columns=[sector_col_other + '_y', 'Marker_y']) \
        .rename(columns={sector_col_other + '_x': sector_col_other,
                         'Marker_x': 'Marker'})

    # Calculate total synth_trips_scaled per sector_marker and join back onto df
    synth_trips_total = df.groupby([sector_col, 'Marker'])['synth_trips'].sum().reset_index()
    synth_trips_total = synth_trips_total.rename(columns={'synth_trips': 'synth_trips_total_odm'})
    df = pd.merge(df, synth_trips_total, on=[sector_col, 'Marker'], how='left')

    # Calculate total synth_trips_scaled trips by origin and join back onto df
    synth_trips_total = df.groupby([sector_col])['synth_trips'].sum().reset_index()
    synth_trips_total = synth_trips_total.rename(columns={'synth_trips': 'synth_trips_total_o_d'})
    df = pd.merge(df, synth_trips_total, on=[sector_col], how='left')

    # Calculate synth_prop, combination of NorMITs scaled data + tour proportions
    df['synth_prop'] = (df['synth_trips']
                        / df['synth_trips_total_odm']) * df['proportion']

    # Check proportions sum to 1 across sector_col
    check = df.groupby([sector_col])['synth_prop'].sum().reset_index()
    check_count = check.loc[~check['synth_prop'].between(0.99999, 1.00001)].shape[0]
    if check_count > 0:
        print('WARNING: Sum of proportions not equal to 1 per block, recommend investigation')

    # Calculate adjusted tour trips as scaled synthetic trips * updated proportions
    df['trips_adj'] = df['synth_trips_total_o_d'] * df['synth_prop']

    # Remove unwanted columns
    df = df[['Sector_o', 'Sector_d', 'Marker', 'trips_adj']]

    # Check input volumes = output volumes
    check_out = df.trips_adj.sum()
    if abs(check_in - check_out) < 1e-3:
        print(f'SUCCESS: Input vs output NorMITs trip volumes matching')
    else:
        print(
            f'WARNING: Input vs output NorMITs trip volumes not matching, recommend investigation')

    return df

if __name__ == "__main__":
    # Melt down the normits_in_df from square matrix format to long & thin format
    normits_proc = pd.melt(normits_in_df, id_vars=['Unnamed: 0'], var_name='zone_dest',
                           value_name='synth_trips')
    normits_proc.rename(columns={'Unnamed: 0': 'zone_orig'}, inplace=True)

    # Aggregate up from NorMITs zones (MSOAs) to county level
    normits_proc = geo_agg(normits_proc,
                           msoa_county_in_df[['msoa11cd', 'county_nm']],
                           {'o': 'zone_orig',
                            'd': 'zone_dest'},
                           {'o': 'msoa11cd',
                            'd': 'msoa11cd'},
                           'county_nm',
                           'both',
                           {'synth_trips': 'sum'},
                           []
                           )

    # Aggregate up from county level to bespoke sector level
    normits_proc = geo_agg(normits_proc,
                           sector_in_df[['county', 'Sector']],
                           {'o': 'county_nm_o',
                            'd': 'county_nm_d'},
                           {'o': 'county',
                            'd': 'county'},
                           'Sector',
                           'both',
                           {'synth_trips': 'sum'},
                           []
                           )

    # Aggregate up the tour model outputs from county level to sector level
    # Retain disaggregation by mode, purpose, direction, period
    tour_proc = geo_agg(tour_in_df,
                        sector_in_df[['county', 'Sector']],
                        {'o': 'tmz_o',
                         'd': 'tmz_d'},
                        {'o': 'county',
                         'd': 'county'},
                        'Sector',
                        'both',
                        {'trips': 'sum'},
                        ['mode', 'purpose', 'direction', 'period']
                        )

    # Call function to calculate row-wise and col-wise proportions of trips to identify major vs minor movements
    result_list = []
    for dirr in ['hb_fr', 'nhb', 'hb_to']:
        df_filter = tour_proc.loc[tour_proc['direction'] == dirr]
        df_out = calc_props(df_filter, dirr)
        result_list.append(df_out)
    # Append all filtered directions
    tour_proc = pd.concat(result_list, ignore_index=True)

    # Based on calculated proportions, apply marker to assign minor movements to 'Rest' category
    tour_proc['Marker'] = tour_proc.apply(marker, axis=1)

    # Aggregate up trips and proportions to sector-marker level by direction
    result_list = []
    for dirr in ['hb_fr', 'nhb', 'hb_to']:
        df_out = agg_to_marker(tour_proc, dirr)
        result_list.append(df_out)
    tour_proc = pd.concat(result_list, ignore_index=True)

    # Filter tour_proc for relevant purpose, mode, direction and generate unique sector_marker index associated with this
    tour_proc, tour_odm_idex = create_sector_marker_idx(tour_proc, normits_proc, filter_dct)

    # Calculate adjusted NorMITs Demand distribution model trips
    cons_proc = calculate_trips_adj(tour_odm_idex, tour_proc, normits_proc,
                                    filter_dct['direction'])

    # Merge on sector IDs for ease of analysing outputs
    cons_out = cons_proc.merge(sector_in_df[['Sector', 'Sector_ID']].drop_duplicates(),
                               left_on='Sector_o',
                               right_on='Sector',
                               how='left')
    cons_out.rename(columns={'Sector_ID': 'Sector_ID_o'}, inplace=True)

    cons_out = cons_out.merge(sector_in_df[['Sector', 'Sector_ID']].drop_duplicates(),
                              left_on='Sector_d',
                              right_on='Sector',
                              how='left')
    cons_out.rename(columns={'Sector_ID': 'Sector_ID_d'}, inplace=True)
    cons_out.drop(['Sector_x', 'Sector_y'], axis=1, inplace=True)

    # Output to .csv
    cons_out.to_csv(os.path.join(output_dir, adj_ver, output_file), index=False)
