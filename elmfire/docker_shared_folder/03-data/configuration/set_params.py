#!/usr/bin/env python3
"""
ELMFIRE Parameter Setting Script for Real-World Fuel Data
Sets parameters for running ELMFIRE with CloudFire fuel data
"""
import sys
import numpy as np
import re
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

def set_parameters():
    """Main function to set ELMFIRE parameters."""
    # Parse command line arguments
    if len(sys.argv) < 2:
        print("Usage: python set_params.py <run_number>")
        sys.exit(1)
    
    # Load configuration file
    config = load_config()
    tracking_file = config['output']['tracking_file']
    run_number = int(sys.argv[1])
    cases_dir = config['directories']['cases_dir']
    
    # Read cleaned from configuration/fromrun_cleaned.txt
    fromrun_cleaned_path = './configuration/fromrun_cleaned.txt'
    with open(fromrun_cleaned_path, 'r') as f:
        cleaned = int(f.read().strip())
    print(cleaned)

    # Handle cleanup if needed
    if cleaned==0 and len(sys.argv) > 2 and int(sys.argv[2]):
        # cut input_tracking.txt to the number of fire areas
        with open(tracking_file, 'r') as f:
            lines = f.readlines()
            lines = lines[:run_number+1]
        with open(tracking_file, 'w') as f:
            f.writelines(lines)
        # Delete cases from run_number onwards (keep cases 0 to run_number-1)
        for i in range(run_number, 100000):  # Assuming max 100000 cases, adjust as needed
            case_dir = f"{cases_dir}/case_{i}"
            if os.path.exists(case_dir):
                shutil.rmtree(case_dir)
                print(f"Deleted case_{i}")
            else:
                # Stop when we hit a case that doesn't exist
                break

    # Set cleaned to 1
    with open(fromrun_cleaned_path, 'w') as f:
        f.write('1')
    
    # Sample parameters based on configuration
    params = config['parameters']
    
    # For real-world fuel data, sample center coordinates within CA/NV region
    # Box: 43.50,-122.67; 38.49,-122.67; 43.50,-118.95; 38.49,-118.95
    center_lat = np.random.uniform(36.3, 42.2)
    center_lon = np.random.uniform(-122.3, -120.5)

    # center_lat = np.random.uniform(36.117, 42.576)
    # center_lon = np.random.uniform(-119, -115)
    
    wind_speed = np.random.uniform(*params['wind']['speed_range'])
    wind_direction = np.random.uniform(*params['wind']['direction_range'])
    
    # Sample moisture based on strategy
    moisture_params = sample_realistic_moisture(config)
    print(f"Run {run_number}: Using {moisture_params['scenario']} moisture scenario")
    
    # Update 01-run.sh
    bash_file = config['files']['bash_script']
    with open(bash_file, 'r') as f:
        bash_content = f.read()

    # Helper to replace a single top-of-file assignment line
    def replace_var(text, name, value):
        # e.g., ^\s*CENTER_LON=.*$  ->  CENTER_LON=-121.234
        pattern = rf'^\s*{name}=.*$'
        replacement = f'{name}={value}'
        return re.sub(pattern, replacement, text, count=1, flags=re.M)
    
    # Set parameters at top of bash script for use in fuel_wx_ign.py call
    # Add parameter definitions after the shebang
    # Parameters set by set_params.py
    # make floats
    CENTER_LAT = center_lat
    CENTER_LON=center_lon
    WINDSPD=wind_speed
    WIND_DIRECTION=wind_direction
    M1=moisture_params["m1"]
    M10=moisture_params["m10"]
    M100=moisture_params["m100"]
    LH_MOISTURE=moisture_params["live_herbaceous"]
    LW_MOISTURE=moisture_params["live_woody"]

    bash_content = replace_var(bash_content, 'CENTER_LON', f'{CENTER_LON:.6f}')
    bash_content = replace_var(bash_content, 'CENTER_LAT', f'{CENTER_LAT:.6f}')
    bash_content = replace_var(bash_content, 'WINDSPD',   f'{WINDSPD:.1f}')
    bash_content = replace_var(bash_content, 'WINDIR',    f'{WIND_DIRECTION:.1f}')
    bash_content = replace_var(bash_content, 'M1C',       f'{M1:.1f}')
    bash_content = replace_var(bash_content, 'M10C',      f'{M10:.1f}')
    bash_content = replace_var(bash_content, 'M100C',     f'{M100:.1f}')
    bash_content = replace_var(bash_content, 'LHC',       f'{LH_MOISTURE:.1f}')
    bash_content = replace_var(bash_content, 'LWC',       f'{LW_MOISTURE:.1f}')

    # WRITE THE CHANGES BACK
    with open(bash_file, 'w') as f:
        f.write(bash_content)

    # Write to tracking file - Note: fuel model is now determined by real-world data
    tracking_file = config['output']['tracking_file']
    if run_number == 0:
        with open(tracking_file, 'w') as f:
            # If run_number is 0, clear file and write header
            header = "run,center_lat,center_lon,ws,wd,m1,m10,m100,lhc,lwc,firearea"
            f.write(header + "\n")
    
    with open(tracking_file, 'a') as f:
        f.write(f"{run_number},{center_lat:.6f},{center_lon:.6f},"
                f"{wind_speed:.1f},{wind_direction:.1f},"
                f"{moisture_params['m1']:.1f},{moisture_params['m10']:.1f},{moisture_params['m100']:.1f},"
                f"{moisture_params['live_herbaceous']:.1f},{moisture_params['live_woody']:.1f},\n")
    
    print(f"Parameters set for run {run_number}")
    print(f"  Center coordinates: ({center_lon:.3f}, {center_lat:.3f})")
    # print(f"  Ignition offset: ({x_ign_offset:.1f}, {y_ign_offset:.1f}) meters from center")
    print(f"  Moisture scenario: {moisture_params['scenario']}")
    print(f"  Dead fuels: {moisture_params['m1']:.1f}, {moisture_params['m10']:.1f}, {moisture_params['m100']:.1f}")
    print(f"  Live fuels: {moisture_params['live_herbaceous']:.1f}, {moisture_params['live_woody']:.1f}")

if __name__ == "__main__":
    set_parameters()