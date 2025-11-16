#!/usr/bin/env python3
"""
Robust test suite for ELMFIRE postprocessor.
Tests all variables, edge cases, and integration scenarios.
"""

import numpy as np
import tempfile
import shutil
from pathlib import Path
import rasterio
from rasterio.transform import from_bounds
from elmfire_postprocessor import (
    burnscar_creation,
    var_sim_from_toa,
    timesteps_from_toa_one_case,
    save_case_arrays,
    create_sims_from_toa_all_cases,
    verify_case_outputs
)

def create_test_tif(data_array, filepath, nodata_value=-9999):
    """Create a test GeoTIFF file."""
    height, width = data_array.shape
    transform = from_bounds(-1, -1, 1, 1, width, height)
    
    with rasterio.open(
        filepath, 'w', driver='GTiff', height=height, width=width,
        count=1, dtype=data_array.dtype, crs='EPSG:4326',
        transform=transform, nodata=nodata_value
    ) as dst:
        dst.write(data_array, 1)

class TestData:
    """Test data generator with various scenarios."""
    
    @staticmethod
    def basic_toa():
        return np.array([
            [-9999, -9999, -9999, -9999, -9999],
            [-9999, -9999,     0, -9999, -9999],
            [-9999,   954,   900, -9999, -9999],
            [ 1800,  2130,  1800, -9999, -9999],
            [-9999, -9999, -9999, -9999, -9999]
        ], dtype=np.float32)
    
    @staticmethod
    def complex_toa():
        """More complex fire spread pattern."""
        return np.array([
            [-9999,  3600,  2700,  3600, -9999],
            [ 2700,  1800,   900,  1800,  3600],
            [ 1800,   900,     0,   900,  1800],
            [ 3600,  1800,   900,  2700,  3600],
            [-9999,  3600,  3600, -9999, -9999]
        ], dtype=np.float32)
    
    @staticmethod
    def instant_ignition():
        """All cells ignite at t=0."""
        return np.zeros((4, 4), dtype=np.float32)
    
    @staticmethod
    def no_fire():
        """No fire spread (all nodata)."""
        return np.full((3, 3), -9999, dtype=np.float32)
    
    @staticmethod
    def flin_data():
        return np.array([
            [-9999,   400,   300,   350, -9999],
            [  300,   250,   200,   220,   280],
            [  200,   150,   100,   130,   180],
            [  280,   180,   120,   160,   200],
            [-9999,   300,   250, -9999, -9999]
        ], dtype=np.float32)
    
    @staticmethod
    def vs_data():
        return np.array([
            [-9999,   2.5,   2.0,   2.2, -9999],
            [  2.0,   1.8,   1.5,   1.6,   1.9],
            [  1.5,   1.2,   1.0,   1.1,   1.4],
            [  1.9,   1.4,   1.1,   1.3,   1.6],
            [-9999,   2.0,   1.8, -9999, -9999]
        ], dtype=np.float32)

def test_burnscar_scenarios():
    """Test burn scar creation with different scenarios."""
    tests_passed = 0
    total_tests = 0
    
    # Test 1: Basic scenario
    total_tests += 1
    toa = TestData.basic_toa()
    burn_900 = burnscar_creation(toa, 900)
    expected = (toa != -9999) & (toa <= 900)
    if np.array_equal(burn_900, expected.astype(np.int8)):
        tests_passed += 1
    
    # Test 2: Instant ignition
    total_tests += 1
    toa = TestData.instant_ignition()
    burn_0 = burnscar_creation(toa, 0)
    if np.all(burn_0 == 1):
        tests_passed += 1
    
    # Test 3: No fire
    total_tests += 1
    toa = TestData.no_fire()
    burn_any = burnscar_creation(toa, 10000)
    if np.all(burn_any == 0):
        tests_passed += 1
    
    # Test 4: Complex pattern
    total_tests += 1
    toa = TestData.complex_toa()
    burn_1800 = burnscar_creation(toa, 1800)
    expected_count = np.sum((toa != -9999) & (toa <= 1800))
    actual_count = np.sum(burn_1800)
    if expected_count == actual_count:
        tests_passed += 1
    
    return tests_passed, total_tests

def test_variable_simulation():
    """Test variable simulation for all variable types."""
    tests_passed = 0
    total_tests = 0
    
    toa = TestData.complex_toa()
    flin = TestData.flin_data()
    vs = TestData.vs_data()
    
    # Test TOA simulation
    total_tests += 1
    toa_sim = var_sim_from_toa(toa, toa, 1800, 'toa')
    mask = (toa != -9999) & (toa <= 1800)
    expected_toa = np.where(mask, toa, -9999)
    if np.array_equal(toa_sim, expected_toa):
        tests_passed += 1
    
    # Test FLIN simulation
    total_tests += 1
    flin_sim = var_sim_from_toa(toa, flin, 1800, 'flin')
    expected_flin = np.where(mask, flin, -9999)
    if np.array_equal(flin_sim, expected_flin):
        tests_passed += 1
    
    # Test VS simulation
    total_tests += 1
    vs_sim = var_sim_from_toa(toa, vs, 1800, 'vs')
    expected_vs = np.where(mask, vs, -9999)
    if np.array_equal(vs_sim, expected_vs):
        tests_passed += 1
    
    return tests_passed, total_tests

def test_timestep_generation():
    """Test timestep array generation with different parameters."""
    tests_passed = 0
    total_tests = 0
    
    with tempfile.TemporaryDirectory() as temp_dir:
        case_dir = Path(temp_dir) / "case_1"
        case_dir.mkdir()
        
        toa_data = TestData.basic_toa()
        create_test_tif(toa_data, case_dir / 'time_of_arrival_001_072000.tif')
        
        # Test different timestep intervals
        intervals = [15, 30, 60]
        max_hours = [1.0, 2.0]
        
        for interval in intervals:
            for max_h in max_hours:
                total_tests += 1
                try:
                    arrays_dict = timesteps_from_toa_one_case(
                        str(case_dir),
                        variables=['toa'],
                        timestep_minutes=interval,
                        max_time_hours=max_h
                    )
                    
                    expected_timesteps = int(max_h * 60 / interval) + 1  # +1 for t=0
                    actual_timesteps = len(arrays_dict['toa'])
                    
                    if actual_timesteps == expected_timesteps:
                        tests_passed += 1
                        
                except Exception:
                    pass
    
    return tests_passed, total_tests

def test_file_operations():
    """Test file saving and verification."""
    tests_passed = 0
    total_tests = 0
    
    with tempfile.TemporaryDirectory() as temp_dir:
        case_dir = Path(temp_dir) / "case_1"
        case_dir.mkdir()
        
        # Create test files
        toa_data = TestData.complex_toa()
        flin_data = TestData.flin_data()
        vs_data = TestData.vs_data()
        
        create_test_tif(toa_data, case_dir / 'time_of_arrival_001_072000.tif')
        create_test_tif(flin_data, case_dir / 'flin_001_072000.tif')
        create_test_tif(vs_data, case_dir / 'vs_001_072000.tif')
        
        # Test processing and saving
        total_tests += 1
        try:
            arrays_dict = timesteps_from_toa_one_case(
                str(case_dir),
                variables=['toa', 'burnscar', 'flin', 'vs'],
                timestep_minutes=30,
                max_time_hours=2.0
            )
            
            output_dir = Path(temp_dir) / 'elmfire_sims'
            save_case_arrays(str(case_dir), arrays_dict, 30, str(output_dir))
            
            # Check if files were created
            case_output = output_dir / 'case_1'
            npy_files = list(case_output.glob('*.npy'))
            
            expected_files = 4 * 5  # 4 variables * 5 timesteps (0, 30, 60, 90, 120 min)
            if len(npy_files) == expected_files:
                tests_passed += 1
                
        except Exception:
            pass
        
        # Test verification
        total_tests += 1
        try:
            verification = verify_case_outputs(str(case_dir), 30, str(output_dir))
            if isinstance(verification, dict) and verification.get('total_files', 0) > 0:
                tests_passed += 1
        except Exception:
            pass
    
    return tests_passed, total_tests

def test_edge_cases():
    """Test edge cases and error conditions."""
    tests_passed = 0
    total_tests = 0
    
    # Test large timestep values
    total_tests += 1
    toa = TestData.basic_toa()
    burn_large = burnscar_creation(toa, 999999)
    if np.sum(burn_large) == np.sum(toa != -9999):  # All non-nodata cells should burn
        tests_passed += 1
    
    # Test zero timestep
    total_tests += 1
    burn_zero = burnscar_creation(toa, 0)
    if np.sum(burn_zero) == np.sum(toa == 0):  # Only instant ignition
        tests_passed += 1
    
    # # Test negative values in TOA (should be treated as nodata)
    # total_tests += 1
    # toa_negative = toa.copy()
    # toa_negative[0, 0] = -100  # Add negative value
    # burn_neg = burnscar_creation(toa_negative, 1000)
    # if burn_neg[0, 0] == 0:  # Negative should not burn
    #     tests_passed += 1
    
    return tests_passed, total_tests

def test_integration():
    """Test full integration with multiple cases."""
    tests_passed = 0
    total_tests = 0
    
    with tempfile.TemporaryDirectory() as temp_dir:
        cases_dir = Path(temp_dir) / 'cases'
        cases_dir.mkdir()
        
        # Create multiple test cases
        test_scenarios = [
            ('case_1', TestData.basic_toa()),
            ('case_2', TestData.complex_toa()),
            ('case_3', TestData.instant_ignition())
        ]
        
        for case_name, toa_data in test_scenarios:
            case_dir = cases_dir / case_name
            case_dir.mkdir()
            create_test_tif(toa_data, case_dir / 'time_of_arrival_001_072000.tif')
            create_test_tif(TestData.flin_data()[:toa_data.shape[0], :toa_data.shape[1]], 
                          case_dir / 'flin_001_072000.tif')
        
        # Test batch processing
        total_tests += 1
        try:
            output_dir = Path(temp_dir) / 'elmfire_sims'
            create_sims_from_toa_all_cases(
                cases_dir=str(cases_dir),
                variables=['toa', 'burnscar'],
                timestep_minutes=60,
                max_time_hours=1.0,
                output_base_dir=str(output_dir)
            )
            
            # Check if all cases were processed
            case_outputs = list(output_dir.glob('case_*'))
            if len(case_outputs) == 3:  # Should have 3 case directories
                tests_passed += 1
                
        except Exception:
            pass
    
    return tests_passed, total_tests

def run_all_tests():
    """Run all test suites."""
    print("ELMFIRE Postprocessor - Robust Test Suite")
    print("=" * 45)
    
    test_suites = [
        ("Burn Scar Creation", test_burnscar_scenarios),
        ("Variable Simulation", test_variable_simulation),
        ("Timestep Generation", test_timestep_generation),
        ("File Operations", test_file_operations),
        ("Edge Cases", test_edge_cases),
        ("Integration", test_integration)
    ]
    
    total_passed = 0
    total_tests = 0
    
    for suite_name, test_func in test_suites:
        passed, tests = test_func()
        total_passed += passed
        total_tests += tests
        
        status = "✓" if passed == tests else "✗"
        print(f"{status} {suite_name}: {passed}/{tests}")
    
    print("-" * 45)
    print(f"Overall: {total_passed}/{total_tests} tests passed")
    
    if total_passed == total_tests:
        print("🎉 All tests passed!")
        return 0
    else:
        print(f"❌ {total_tests - total_passed} tests failed")
        return 1

if __name__ == "__main__":
    exit_code = run_all_tests()
    exit(exit_code)