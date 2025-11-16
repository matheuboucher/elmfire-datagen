#!/usr/bin/env python3
"""
ELMFIRE Parameter Setting Script
Sets parameters for running ELMFIRE based on config.yaml settings
"""

import sys
import numpy as np
import re
# import yaml
import json
import os
import shutil

def load_config(config_path='./configuration/config.json'):
    """Load configuration from json file."""
    try:
        with open(config_path, 'r') as f:
            config = json.load(f)
        return config
    except FileNotFoundError:
        print(f"Error: Config file {config_path} not found")
        sys.exit(1)

def load_available_fuel_models(config):
    return config['parameters']['fuel_models']['models']

def sample_realistic_moisture(config):
    """Sample fuel moisture using realistic D1-D4 x L1-L4 scenarios."""
    moisture_config = config['parameters']['moisture']
    
    # Randomly select dead and live scenarios
    dead_scenario = np.random.choice(list(moisture_config['dead_scenarios'].keys()))
    live_scenario = np.random.choice(list(moisture_config['live_scenarios'].keys()))
    
    # Get moisture values
    m1, m10, m100 = moisture_config['dead_scenarios'][dead_scenario]
    live_herbaceous, live_woody = moisture_config['live_scenarios'][live_scenario]
    
    return {
        'm1': float(m1),
        'm10': float(m10), 
        'm100': float(m100),
        'live_herbaceous': float(live_herbaceous),
        'live_woody': float(live_woody),
        'scenario': f"{dead_scenario}_{live_scenario}"
    }

def sample_continuous_moisture(config):
    """Sample fuel moisture continuously with physical constraints."""
    cont_config = config['parameters']['moisture']['continuous_sampling']
    
    # Sample 1-hr moisture from range
    m1 = np.random.uniform(*cont_config['m1_range'])
    # Maintain realistic gradients
    m10 = m1 + 1.0
    m100 = m1 + 2.0
    
    # Sample live fuels with constraint that woody >= herbaceous + offset
    live_herbaceous = np.random.uniform(*cont_config['live_herbaceous_range'])
    live_woody_min = max(live_herbaceous + 20, cont_config['live_woody_range'][0])
    live_woody = np.random.uniform(live_woody_min, cont_config['live_woody_range'][1])
    
    return {
        'm1': m1,
        'm10': m10,
        'm100': m100, 
        'live_herbaceous': live_herbaceous,
        'live_woody': live_woody,
        'scenario': 'continuous'
    }

def sample_random_moisture(config):
    """Sample fuel moisture randomly (for comparison dataset)."""
    # Broad ranges for random sampling
    m1 = np.random.uniform(2.0, 40.0)
    m10 = np.random.uniform(2.0, 40.0) 
    m100 = np.random.uniform(2.0, 40.0)
    live_herbaceous = np.random.uniform(30.0, 150.0)
    live_woody = np.random.uniform(30.0, 200.0)
    
    return {
        'm1': m1,
        'm10': m10,
        'm100': m100,
        'live_herbaceous': live_herbaceous,
        'live_woody': live_woody,
        'scenario': 'random'
    }

def set_parameters():
    """Main function to set ELMFIRE parameters."""
    # Parse command line arguments
    if len(sys.argv) < 2:
        print("Usage: python set_params.py <run_number>")
        sys.exit(1)
    
    # Load configuration file
    config = load_config()
    tracking_file = config['output']['tracking_file']
    fire_area_file = config['files']['fire_area']
    run_number = int(sys.argv[1])
    cases_dir = config['directories']['cases_dir']
    # read cleaned from configuration/fromrun_cleaned.txt
    fromrun_cleaned_path = './configuration/fromrun_cleaned.txt'
    with open(fromrun_cleaned_path, 'r') as f:
        cleaned = int(f.read().strip())
    print(cleaned)

    # get NONZERO_START from command line arguments
    if cleaned==0 and len(sys.argv) > 2 and int(sys.argv[2]):
        print("resetting fire_area and input_tracking.txt files")
        with open(fire_area_file, 'r') as f:
            fire_areas = f.readlines()
            fire_areas = fire_areas[:run_number]
        with open(fire_area_file, 'w') as f:
            f.writelines(fire_areas)
        # cut input_tracking.txt to the number of fire areas
        with open(tracking_file, 'r') as f:
            lines = f.readlines()
            lines = lines[:run_number+1]
        with open(tracking_file, 'w') as f:
            f.writelines(lines)
        # Delete cases from run_number onwards (keep cases 0 to run_number-1)
        for i in range(run_number, 100000):  # Assuming max 1000 cases, adjust as needed
            case_dir = f"{cases_dir}/case_{i}"
            if os.path.exists(case_dir):
                shutil.rmtree(case_dir)
                print(f"Deleted case_{i}")
            else:
                # Stop when we hit a case that doesn't exist
                break

    # set cleaned to 1
    with open(fromrun_cleaned_path, 'w') as f:
        f.write('1')
    
    # # Update most recent run in config
    # most_recent_run = config['most_recent_run']['recent_run']
    # if run_number == 0:
    #     most_recent_run['recent_run'] = int(run_number)
    # else:
    #     most_recent_run['recent_run'] = int(run_number) - 1
    # config['most_recent_run'] = most_recent_run
    # # get a start_num from the 4th command line argument and if it is 0, clear
    # # the input_tracking.txt file and just write in the header from the config file
    # # if int(run_number)==0:
    # #     print("Clearing input_tracking.txt and writing header...")
    # #     # replace input_tracking.txt with the header from the config file
    # #     header = config['output']['header']
    # #     with open("input_tracking.txt", "w") as f:
    # #         f.write(header + "\n")
    
    # Get parameters from config or command line
    tstop = config['domain']['simulation_tstop']
    domain_size = config['domain']['size']
    
    # Load available fuel models
    available_fuel_models = load_available_fuel_models(config)
    # print(f"Available fuel models: {available_fuel_models}")
    
    # Sample parameters based on configuration
    params = config['parameters']
    
    # Basic parameters
    fuel_model = 101  # Single fuel model for this dataset
    x_ign = np.random.uniform(*params['ignition']['x_range'])
    y_ign = np.random.uniform(*params['ignition']['y_range'])
    slope = np.random.randint(params['terrain']['slope_range'][0], 
                             params['terrain']['slope_range'][1] + 1)
    aspect = np.random.randint(params['terrain']['aspect_range'][0], 
                              params['terrain']['aspect_range'][1] + 1)
    wind_speed = np.random.uniform(*params['wind']['speed_range'])
    wind_direction = np.random.uniform(*params['wind']['direction_range'])
    
    # Canopy parameters (currently fixed)
    canopy_cover = params['canopy']['cover']
    canopy_height = params['canopy']['height']
    canopy_base_height = params['canopy']['base_height']
    canopy_bulk_density = params['canopy']['bulk_density']
    
    # Sample moisture based on strategy
    sampling_strategy = config['sampling']['strategy']
    
    if sampling_strategy == "realistic":
        moisture_params = sample_realistic_moisture(config)
    elif sampling_strategy == "continuous":
        if config['parameters']['moisture']['continuous_sampling']['enabled']:
            moisture_params = sample_continuous_moisture(config)
        else:
            print("Warning: Continuous sampling not enabled, using realistic")
            moisture_params = sample_realistic_moisture(config)
    else:  # random
        moisture_params = sample_random_moisture(config)
    
    print(f"Run {run_number}: Using {moisture_params['scenario']} moisture scenario")
    
    # Update 01-run.sh
    bash_file = config['files']['bash_script']
    with open(bash_file, 'r') as f:
        bash_content = f.read()
    
    # Domain and simulation settings
    bash_content = re.sub(r'DOMAINSIZE=[0-9.-]+', f'DOMAINSIZE={domain_size}', bash_content)
    bash_content = re.sub(r'SIMULATION_TSTOP=[0-9.-]+', f'SIMULATION_TSTOP={tstop}', bash_content)
    
    # Float rasters
    bash_content = re.sub(r'FLOAT_VAL\[1\]=[0-9.-]+', f'FLOAT_VAL[1]={wind_speed:.1f}', bash_content)
    bash_content = re.sub(r'FLOAT_VAL\[2\]=[0-9.-]+', f'FLOAT_VAL[2]={wind_direction:.1f}', bash_content)
    bash_content = re.sub(r'FLOAT_VAL\[3\]=[0-9.-]+', f'FLOAT_VAL[3]={moisture_params["m1"]:.1f}', bash_content)
    bash_content = re.sub(r'FLOAT_VAL\[4\]=[0-9.-]+', f'FLOAT_VAL[4]={moisture_params["m10"]:.1f}', bash_content)
    bash_content = re.sub(r'FLOAT_VAL\[5\]=[0-9.-]+', f'FLOAT_VAL[5]={moisture_params["m100"]:.1f}', bash_content)
    
    # Integer rasters
    bash_content = re.sub(r'INT_VAL\[1\]=\d+', f'INT_VAL[1]={slope}', bash_content)
    bash_content = re.sub(r'INT_VAL\[2\]=\d+', f'INT_VAL[2]={aspect}', bash_content)
    bash_content = re.sub(r'INT_VAL\[4\]=\d+', f'INT_VAL[4]={fuel_model}', bash_content)
    bash_content = re.sub(r'INT_VAL\[5\]=\d+', f'INT_VAL[5]={canopy_cover}', bash_content)
    bash_content = re.sub(r'INT_VAL\[6\]=\d+', f'INT_VAL[6]={canopy_height}', bash_content)
    bash_content = re.sub(r'INT_VAL\[7\]=\d+', f'INT_VAL[7]={canopy_base_height}', bash_content)
    bash_content = re.sub(r'INT_VAL\[8\]=\d+', f'INT_VAL[8]={canopy_bulk_density}', bash_content)
    
    # Live moisture content
    bash_content = re.sub(r'LH_MOISTURE_CONTENT=[0-9.-]+', 
                         f'LH_MOISTURE_CONTENT={moisture_params["live_herbaceous"]:.1f}', bash_content)
    bash_content = re.sub(r'LW_MOISTURE_CONTENT=[0-9.-]+', 
                         f'LW_MOISTURE_CONTENT={moisture_params["live_woody"]:.1f}', bash_content)
    
    with open(bash_file, 'w') as f:
        f.write(bash_content)
    
    # Update elmfire.data.in
    elmfire_file = config['files']['elmfire_config']
    with open(elmfire_file, 'r') as f:
        config_content = f.read()
    
    config_content = re.sub(r'X_IGN\(1\)\s*=\s*[0-9.-]+', f'X_IGN(1)      = {x_ign:.1f}', config_content)
    config_content = re.sub(r'Y_IGN\(1\)\s*=\s*[0-9.-]+', f'Y_IGN(1)      = {y_ign:.1f}', config_content)

    vars_to_track = config["vars_to_track"]
    for var in vars_to_track:
        # if it's set as true in config, set it to .TRUE. in elmfire.data.in file
        if vars_to_track[var]:
            config_content = re.sub(rf'{var}\s*=\s*\.(TRUE|FALSE)\.', f'{var} = .TRUE.', config_content)
        else:
            config_content = re.sub(rf'{var}\s*=\s*\.(TRUE|FALSE)\.', f'{var} = .FALSE.', config_content)

        with open(elmfire_file, 'w') as f:
            f.write(config_content)
    
    # Write to tracking file
    tracking_file = config['output']['tracking_file']
    if run_number == '0':
        with open(tracking_file, 'w') as f:
            # If run_number is 0, clear file and write header
            f.write(config['output']['header'] + "\n")
        # clear contents of fire_area_most_recent_run.txt
        fire_area_file = config['files']['fire_area']
        with open(fire_area_file, 'w') as f:
            f.write("")
    with open(tracking_file, 'a') as f:
        f.write(f"{run_number},{x_ign:.1f},{y_ign:.1f},{fuel_model},{slope},{aspect},"
                f"{wind_speed:.1f},{wind_direction:.1f},"
                f"{moisture_params['m1']:.1f},{moisture_params['m10']:.1f},{moisture_params['m100']:.1f},"
                f"{canopy_cover},{canopy_height},{canopy_base_height},{canopy_bulk_density},"
                f"{moisture_params['live_herbaceous']:.1f},{moisture_params['live_woody']:.1f},\n")
    
    print(f"Parameters set for run {run_number}")
    print(f"  Fuel model: {fuel_model}")
    print(f"  Ignition: ({x_ign:.1f}, {y_ign:.1f})")
    print(f"  Moisture scenario: {moisture_params['scenario']}")
    print(f"  Dead fuels: {moisture_params['m1']:.1f}, {moisture_params['m10']:.1f}, {moisture_params['m100']:.1f}")
    print(f"  Live fuels: {moisture_params['live_herbaceous']:.1f}, {moisture_params['live_woody']:.1f}")

if __name__ == "__main__":
    set_parameters()