This directory will house the main postprocessing script for processing the outputs
from ELMFIRE. The ELMFIRE cases contain fireline intensities, time of arrival, and spread rate
tif file labeled like so:

time_of_arrival: time_of_arrival_0000001_0259220
flin: flin_0000001_0259220
vs: vs_0000001_0259220.tif

The output rasters in these files contain the final results for these dependent variables
(at the end simulation time which is 259200s or 72hr). We want a simple script that
will take the cases in the cases directory (in this directory at ./cases/case_#)
and save .npy files for each dependent variable in each case's directory in a new
directory called case_#_npy. We can ignore the isochrone files for now.

The script will be able to implemement a base case which will be to save the time of
arrival raster as an npy at a given interval (we'll start with 15min) as well as
a new output, the burn scar which will just be 1's where the area is burnt and 0 elsewhere.
These npy files will be saved as case_#_variable_timestep.npy
For example, for case_1, this script will find the time_of_arrival_raster and then
get the grid up until a certain time. For a very simple example:

[-9999,-9999,-9999,-9999,-9999]
[-9999,-9999,0,-9999,-9999]
[-9999,15.9,15,-9999,-9999]
[30,35.5,30,-9999,-9999]
[-9999,-9999,-9999,-9999,-9999]

will translate to case_1_time_of_arrival_900:
[-9999,-9999,-9999,-9999,-9999]
[-9999,-9999,0,-9999,-9999]
[-9999,-9999,15,-9999,-9999]
[-9999,-9999,-9999,-9999,-9999]
[-9999,-9999,-9999,-9999,-9999]

and case_1_time_of_arrival_1800:
[-9999,-9999,-9999,-9999,-9999]
[-9999,-9999,0,-9999,-9999]
[-9999,15.9,15,-9999,-9999]
[30,-9999,30,-9999,-9999]
[-9999,-9999,-9999,-9999,-9999]

and case_1_burnscar_900:
[0,0,0,0,0]
[0,0,1,0,0]
[0,0,1,0,0]
[0,0,0,0,0]
[0,0,0,0,0]

and case_1_burnscar_1800:
[0,0,0,0,0]
[0,0,1,0,0]
[0,1,1,0,0]
[1,0,1,0,0]
[0,0,0,0,0]

A rough sketch of a few functions:
1. burnscar_creation
   1. this function calculates the burn scar as an npy based on the time of arrival raster
2. var_sim_from_toa
   1. this function should take the toa raster and another variable raster (maybe as strings so it could be toa itself with some condition checking that if it is specified as toa being the variable we want to build a simulation for from the toa, then it just uses the one raster) and get the state of that raster as a numpy array for the specified time based on the time of arrival raster
3. timesteps_from_toa_one_case (time of arrival and burn scar timesteps from time of arriavl raster for one case)
   1. this function should take in a list of strings denoting the variables that we want to create npy files for. It should default to just toa and burn scar, but if it includes others like vs or flin (the only two other possibilities) then it should build a simulation for that variable, i.e. convert it to a series of numpy arrays (maybe stored in a list or dict) separated by the timestep specified (default to 15 min)
4. create_sims_from_toa_all_cases (applies a previous function to all cases in case directory)

There may be other functionality that I'm missing, but I would like to keep it as tight and succinct/simple as possible, without worrying too much about wrong inputs or other errors apart from the occasional shape check (that all grids are the same shape).

## Output Structure

NOTE: cases were taken from 01-sub15 sims output

After processing, you'll get a separate `elmfire_sims` directory:
```
./elmfire_sims/
├── case_1/
│   ├── case_1_toa_900.npy      # 15 minutes (900 seconds)
│   # ELMFIRE Output Postprocessor

This directory contains scripts for processing ELMFIRE simulation outputs into timestep-based numpy arrays suitable for machine learning model training.

## Overview

The postprocessor takes ELMFIRE output TIF files and creates temporal sequences showing how wildfire variables evolve over time. It processes:

- **Time of Arrival (TOA)**: Shows when fire reaches each grid cell
- **Burn Scar**: Binary mask showing burned areas at each timestep
- **Fireline Intensity (flin)**: Fire intensity values over time
- **Spread Rate (vs)**: Fire spread velocity over time

## Files

- `elmfire_postprocessor.py` - Main processing functions
- `elmfire_postprocessor.py` - Command-line interface with configuration support
- `config_postprocess.yaml` - Configuration file for processing parameters
- `README.md` - This documentation

## Quick Start

1. **Basic processing** (using default 15-minute timesteps):
   ```bash
   python elmfire_postprocessor.py
   ```

2. **Custom configuration**:
   ```bash
   python elmfire_postprocessor.py --config my_config.yaml
   ```

3. **Process specific variables**:
   ```bash
   python elmfire_postprocessor.py --variables toa burnscar flin
   ```

4. **Process single case**:
   ```bash
   python elmfire_postprocessor.py --single-case case_1
   ```

## Input Structure

The script expects this directory structure:
```
./cases/
├── case_1/
│   ├── time_of_arrival_0000001_0259220.tif
│   ├── flin_0000001_0259220.tif
│   └── vs_0000001_0259220.tif
├── case_2/
│   ├── time_of_arrival_0000001_0259220.tif
│   ├── flin_0000001_0259220.tif
│   └── vs_0000001_0259220.tif
└── ...
```

## Output Structure

After processing, you'll get:
```
./cases/
├── case_1_npy/
│   ├── case_1_toa_900.npy      # 15 minutes (900 seconds)
│   ├── case_1_toa_1800.npy     # 30 minutes
│   ├── case_1_burnscar_900.npy
│   ├── case_1_burnscar_1800.npy
│   ├── case_1_flin_900.npy
│   └── ...
├── case_2_npy/
│   └── ...
└── ...
```

## Configuration

Edit `config.py` to customize:

```yaml
# Processing Parameters
timestep_minutes: 15        # Timestep interval (15, 30, 60 minutes)
max_time_hours: 72.0       # Maximum simulation time

# Variables to Process
variables:
  - 'toa'                  # Time of arrival
  - 'burnscar'           # Binary burn scar
  - 'flin'                # Fireline intensity
  - 'vs'                  # Spread rate

# Directories
cases_dir: './cases'       # Input directory
```

## Command Line Options

```bash
python elmfire_postprocessor.py [OPTIONS]

Options:
  --config, -c PATH        Configuration file (default: config_postprocess.yaml)
  --cases-dir PATH         Override cases directory
  --variables VAR [VAR...] Override variables list
  --timestep INT           Override timestep minutes
  --max-time FLOAT         Override max time hours
  --verify-only            Only verify existing outputs
  --single-case CASE       Process only specific case
```

## Examples

### Process with 30-minute timesteps:
```bash
python elmfire_postprocessor.py --timestep 30
```

### Process only time of arrival and burn scar:
```bash
python elmfire_postprocessor.py --variables toa burnscar
```

### Verify existing outputs:
```bash
python elmfire_postprocessor.py --verify
```

<!-- ### Process custom directory:
```bash
python elmfire_postprocessor.py --cases-dir /path/to/my/cases
``` -->

EXAMPLE: python elmfire_postprocessor.py --variables toa burnscar

## Output Format

Each `.npy` file contains a 2D numpy array with:
- **Time of Arrival**: Values in seconds, -9999 for unburned areas
- **Burn Scar**: Binary values (1 = burned, 0 = unburned)
- **Fireline Intensity**: Fire intensity values, -9999 where fire hasn't arrived
- **Spread Rate**: Velocity values, -9999 where fire hasn't arrived

## Integration with Diffusion Model

The output arrays are designed to work with your diffusion model training pipeline:

```python
# Load processed data
import numpy as np

# Load a timestep
toa_data = np.load('case_1_npy/case_1_toa_900.npy')
burnscar = np.load('case_1_npy/case_1_burnscar_900.npy')
flin_data = np.load('case_1_npy/case_1_flin_900.npy')

# Stack for model input
fire_state = np.stack([toa_data, burnscar, flin_data], axis=0)  # [3, H, W]
```

## Troubleshooting

### Common Issues:

1. **No TIF files found**: Check that case directories contain the expected TIF files
2. **Memory errors**: Use `--single-case` to process one case at a time
3. **Shape mismatches**: All TIF files should have the same spatial dimensions

### Logging:
Check `postprocessing.log` for detailed processing information.

## Performance Notes

- Processing ~1000 cases takes approximately 10-30 minutes depending on grid size
- Memory usage scales with grid resolution and number of timesteps
- Use `memory_efficient: true` in config for large datasets

## Next Steps

After postprocessing, your data will be ready for:
1. Integration with the wildfire diffusion model
2. Training dataset creation
3. Temporal sequence analysis
4. Grid-invariant model development