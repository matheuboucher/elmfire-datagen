#!/usr/bin/env python3
"""
ELMFIRE Output Postprocessor - Simplified Version

This script processes ELMFIRE simulation outputs to create timestep-based numpy arrays
for time of arrival, burn scar, flame length intensity (flin), and spread rate (vs).
"""

import sys
import glob
import numpy as np
import rasterio
import json
from pathlib import Path
from typing import List, Dict

# load config
def load_config(config_path='./dataset_config.json'):
    """Load configuration from json file."""
    try:
        with open(config_path, 'r') as f:
            config = json.load(f)
        return config
    except FileNotFoundError:
        print(f"Error: Config file {config_path} not found")
        sys.exit(1)

config = load_config('./dataset_config.json')
variables = [config["postprocess_vars"][var] for var in config["vars_to_track"] if config["vars_to_track"][var]]
print(variables)

case_dir = '../data/' + config['dataset']['name'] + '/' + config['directories']['cases_dir']

output_base_dir = config.get('output_dir', './processed_sims')
variables = config['postprocess']['variables']
timestep_minutes = config['domain']['tstep_minutes']
max_time_hours = config['domain']['simulation_tstop'] / 3600.0

def load_tif_as_array(filepath: str) -> np.ndarray:
    """Load a GeoTIFF file as a numpy array."""
    with rasterio.open(filepath) as src:
        data = src.read(1)
        if src.nodata is not None:
            data = np.where(data == src.nodata, -9999, data)
        return data

def burnscar_creation(toa_array: np.ndarray, timestep_seconds: float) -> np.ndarray:
    """Create burn scar array based on time of arrival."""
    burnscar = np.where(
        (toa_array != -9999) & (toa_array <= timestep_seconds), 
        1, 
        0
    )
    return burnscar.astype(np.int8)

def var_sim_from_toa(toa_array: np.ndarray, 
                     var_array: np.ndarray, 
                     timestep_seconds: float,
                     variable_name: str = 'toa') -> np.ndarray:
    """Extract variable state at a specific timestep based on time of arrival."""
    if variable_name.lower() == 'toa' or variable_name.lower() == 'time_of_arrival':
        result = np.where(
            (toa_array != -9999) & (toa_array <= timestep_seconds),
            toa_array,
            -9999
        )
    else:
        result = np.where(
            (toa_array != -9999) & (toa_array <= timestep_seconds),
            var_array,
            -9999
        )
    return result

def timesteps_from_toa_one_case(case_dir: str, 
                               variables: List[str] = ['toa', 'burnscar'],
                               timestep_minutes: int = 15,
                               max_time_hours: float = 72.0) -> Dict[str, List[np.ndarray]]:
    """Create timestep-based simulation arrays for one case."""
    case_path = Path(case_dir)
    case_num = case_path.name.split('_')[-1]
    
    # Find required files
    toa_files = glob.glob(str(case_path / 'time_of_arrival_*.tif'))
    flin_files = glob.glob(str(case_path / 'flin_*.tif'))
    vs_files = glob.glob(str(case_path / 'vs_*.tif'))
    
    if not toa_files:
        raise FileNotFoundError(f"No time_of_arrival files found in {case_dir}")
    
    # Load time of arrival (required for all variables)
    # print the toa_array
    toa_array = load_tif_as_array(toa_files[0])
    
    # Load other variable arrays if needed
    variable_arrays = {'toa': toa_array}
    
    if 'flin' in variables and flin_files:
        variable_arrays['flin'] = load_tif_as_array(flin_files[0])
    
    if 'vs' in variables and vs_files:
        variable_arrays['vs'] = load_tif_as_array(vs_files[0])
    
    # Generate timesteps
    timestep_seconds = timestep_minutes * 60
    max_time_seconds = max_time_hours * 3600
    timesteps = np.arange(0, max_time_seconds + timestep_seconds, timestep_seconds)
    
    # Process each variable
    results = {}
    
    for variable in variables:
        results[variable] = []
        
        for ts in timesteps:
            if variable == 'burnscar':
                array = burnscar_creation(toa_array, ts)
            elif variable in variable_arrays:
                array = var_sim_from_toa(
                    toa_array, 
                    variable_arrays[variable], 
                    ts, 
                    variable
                )
            else:
                print(f"Warning: Variable '{variable}' not available for case {case_num}")
                continue
            
            results[variable].append(array)
    
    return results

def save_case_arrays(case_dir: str, 
                    arrays_dict: Dict[str, List[np.ndarray]], 
                    timestep_minutes: int = 15,
                    output_base_dir: str = output_base_dir):
    """Save arrays to .npy files in elmfire_sims directory."""
    case_path = Path(case_dir)
    case_num = case_path.name.split('_')[-1]
    
    # Create output directory
    output_base = Path(output_base_dir)
    output_dir = output_base / f"case_{case_num}"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    timestep_seconds = timestep_minutes * 60
    
    for variable, array_list in arrays_dict.items():
        var_dir = output_dir / variable
        var_dir.mkdir(exist_ok=True)
        for i, array in enumerate(array_list):
            timestep = (i) * timestep_seconds
            filename = f"case_{case_num}_{variable}_{int(timestep)}.npy"
            filepath = output_dir / f"{variable}" / filename
            np.save(filepath, array)
    
    print(f"  Saved {len([item for sublist in arrays_dict.values() for item in sublist])} files to {output_dir}")

def create_sims_from_toa_all_cases(cases_dir: str = case_dir,
                                  variables: List[str] = ['toa', 'burnscar'],
                                  timestep_minutes: int = 15,
                                  max_time_hours: float = 72.0,
                                  output_base_dir: str = output_base_dir):
    """Process all cases and save to elmfire_sims."""
    cases_path = Path(cases_dir)
    output_path = Path(output_base_dir)
    
    if not cases_path.exists():
        raise FileNotFoundError(f"Cases directory not found: {cases_dir}")
    
    # Create output base directory
    output_path.mkdir(exist_ok=True)
    
    # Find all case directories
    case_dirs = [d for d in cases_path.iterdir() if d.is_dir() and d.name.startswith('case_')]
    case_dirs.sort(key=lambda x: int(x.name.split('_')[-1]))
    
    if not case_dirs:
        raise FileNotFoundError(f"No case directories found in {cases_dir}")
    
    print("ELMFIRE Postprocessor")
    print(f"Found {len(case_dirs)} cases to process")
    print(f"Variables: {variables}")
    print(f"Timestep: {timestep_minutes} minutes")
    print(f"Max time: {max_time_hours} hours")
    print(f"Output: {output_base_dir}")
    print("-" * 50)
    
    success_count = 0
    for case_dir in case_dirs:
        try:
            print(f"Processing {case_dir.name}...")
            
            # Generate arrays for this case
            arrays_dict = timesteps_from_toa_one_case(
                str(case_dir), 
                variables, 
                timestep_minutes, 
                max_time_hours
            )
            
            # Save arrays to files
            save_case_arrays(str(case_dir), arrays_dict, timestep_minutes, output_base_dir)
            success_count += 1
            
        except Exception as e:
            print(f"  Error processing {case_dir.name}: {e}")
            continue
    
    print("-" * 50)
    print(f"Processing complete! {success_count}/{len(case_dirs)} cases successful")
    print(f"Results saved to: {output_base_dir}")

def verify_case_outputs(case_dir: str, timestep_minutes: int = 15, output_base_dir: str = output_base_dir) -> Dict:
    """Verify outputs for a single case."""
    case_path = Path(case_dir)
    case_num = case_path.name.split('_')[-1]
    npy_dir = Path(output_base_dir) / f"case_{case_num}"
    
    if not npy_dir.exists():
        return f"No output directory found for case {case_num}"
    
    npy_files = list(npy_dir.glob("*.npy"))
    
    if not npy_files:
        return f"No .npy files found for case {case_num}"
    
    # Group files by variable
    variables = {}
    shapes = set()
    
    for filepath in npy_files:
        parts = filepath.stem.split('_')
        if len(parts) >= 4:
            variable = parts[2]
            timestep = int(parts[3])
            
            if variable not in variables:
                variables[variable] = []
            variables[variable].append(timestep)
            
            # Check array shape
            array = np.load(filepath)
            shapes.add(array.shape)
    
    return {
        "case": case_num,
        "variables": list(variables.keys()),
        "file_counts": {var: len(times) for var, times in variables.items()},
        "shapes": list(shapes),
        "total_files": len(npy_files)
    }

if __name__ == "__main__":
    # Default: process all cases with default variables
    create_sims_from_toa_all_cases(
        cases_dir=case_dir,
        variables=variables,
        timestep_minutes=timestep_minutes,
        max_time_hours=max_time_hours,
        output_base_dir=output_base_dir
    )
    