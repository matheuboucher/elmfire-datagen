#!/usr/bin/env python3
"""
Simple test script for ELMFIRE postprocessor.
"""

import numpy as np
import tempfile
from pathlib import Path
import rasterio
from rasterio.transform import from_bounds
from elmfire_postprocessor import (
    burnscar_creation,
    var_sim_from_toa,
    timesteps_from_toa_one_case
)

def create_test_tif(data_array, filepath):
    """Create a test GeoTIFF file."""
    height, width = data_array.shape
    transform = from_bounds(-1, -1, 1, 1, width, height)
    
    with rasterio.open(
        filepath, 'w', driver='GTiff', height=height, width=width,
        count=1, dtype=data_array.dtype, crs='EPSG:4326',
        transform=transform, nodata=-9999
    ) as dst:
        dst.write(data_array, 1)

def test_functions():
    """Test the core functions with your example data."""
    print("Testing ELMFIRE postprocessor functions...")
    
    # Your example data (converted from minutes to seconds)
    toa_data = np.array([
        [-9999, -9999, -9999, -9999, -9999],
        [-9999, -9999,     0, -9999, -9999],
        [-9999,  954,    900, -9999, -9999],  # 15.9 min * 60 = 954s, 15 min * 60 = 900s
        [ 1800, 2130,   1800, -9999, -9999],  # 30 min * 60 = 1800s, 35.5 min * 60 = 2130s
        [-9999, -9999, -9999, -9999, -9999]
    ], dtype=np.float32)
    
    # Test burn scar at 15 minutes (900 seconds)
    print("\n1. Testing burn scar creation at 15 minutes:")
    burnscar_900 = burnscar_creation(toa_data, 900)
    expected_900 = np.array([
        [0, 0, 0, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
    ])
    
    print("Expected:")
    print(expected_900)
    print("Got:")
    print(burnscar_900)
    print("✓ Match!" if np.array_equal(burnscar_900, expected_900) else "✗ No match")
    
    # Test burn scar at 30 minutes (1800 seconds)
    print("\n2. Testing burn scar creation at 30 minutes:")
    burnscar_1800 = burnscar_creation(toa_data, 1800)
    expected_1800 = np.array([
        [0, 0, 0, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 1, 1, 0, 0],
        [1, 0, 1, 0, 0],
        [0, 0, 0, 0, 0]
    ])
    
    print("Expected:")
    print(expected_1800)
    print("Got:")
    print(burnscar_1800)
    print("✓ Match!" if np.array_equal(burnscar_1800, expected_1800) else "✗ No match")
    
    # Test TOA simulation at 15 minutes
    print("\n3. Testing TOA simulation at 15 minutes:")
    toa_sim_900 = var_sim_from_toa(toa_data, toa_data, 900, 'toa')
    expected_toa_900 = np.array([
        [-9999, -9999, -9999, -9999, -9999],
        [-9999, -9999,     0, -9999, -9999],
        [-9999, -9999,   900, -9999, -9999],
        [-9999, -9999, -9999, -9999, -9999],
        [-9999, -9999, -9999, -9999, -9999]
    ])
    
    print("Expected:")
    print(expected_toa_900)
    print("Got:")
    print(toa_sim_900)
    print("✓ Match!" if np.array_equal(toa_sim_900, expected_toa_900) else "✗ No match")

def test_full_pipeline():
    """Test the full pipeline with temporary files."""
    print("\n" + "="*50)
    print("Testing full pipeline with temporary files...")
    
    with tempfile.TemporaryDirectory() as temp_dir:
        # Create test case directory
        case_dir = Path(temp_dir) / "case_1"
        case_dir.mkdir()
        
        # Create test data (values in seconds)
        toa_data = np.array([
            [-9999, -9999, -9999, -9999, -9999],
            [-9999, -9999,     0, -9999, -9999],
            [-9999,  954,    900, -9999, -9999],  # 15.9 min * 60, 15 min * 60
            [ 1800, 2130,   1800, -9999, -9999], # 30 min * 60, 35.5 min * 60
            [-9999, -9999, -9999, -9999, -9999]
        ], dtype=np.float32)
        
        flin_data = np.array([
            [-9999, -9999, -9999, -9999, -9999],
            [-9999, -9999,   100, -9999, -9999],
            [-9999,   200,   150, -9999, -9999],
            [  250,   300,   180, -9999, -9999],
            [-9999, -9999, -9999, -9999, -9999]
        ], dtype=np.float32)
        
        # Create TIF files
        create_test_tif(toa_data, case_dir / 'time_of_arrival_0000001_0259220.tif')
        create_test_tif(flin_data, case_dir / 'flin_0000001_0259220.tif')
        
        # Test processing
        print("Processing test case...")
        arrays_dict = timesteps_from_toa_one_case(
            str(case_dir),
            variables=['toa', 'burnscar', 'flin'],
            timestep_minutes=15,
            max_time_hours=1.0  # Just 1 hour for testing
        )
        
        print(f"Generated arrays for variables: {list(arrays_dict.keys())}")
        print(f"Number of timesteps per variable: {[len(arrays) for arrays in arrays_dict.values()]}")
        
        # Check first timestep burn scar
        burnscar_first = arrays_dict['burnscar'][1]
        expected_burnscar = burnscar_creation(toa_data, 900)
        
        print("\nFirst timestep burn scar:")
        print("Expected:")
        print(expected_burnscar)
        print("Got:")
        print(burnscar_first)
        print("✓ Pipeline test passed!" if np.array_equal(burnscar_first, expected_burnscar) else "✗ Pipeline test failed")

if __name__ == "__main__":
    print("ELMFIRE Postprocessor Test Suite")
    print("=" * 50)
    
    try:
        test_functions()
        test_full_pipeline()
        print("\n" + "=" * 50)
        print("✓ All tests completed!")
        
    except Exception as e:
        print(f"\n✗ Test failed: {e}")
        import traceback
        traceback.print_exc()