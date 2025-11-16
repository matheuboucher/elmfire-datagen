import numpy as np
import torch
import os
import sys
import random
import pandas as pd
from typing import List, Optional, Tuple, Dict, Any
from torch.utils.data import Dataset
from tqdm.auto import tqdm
from pathlib import Path
from bisect import bisect_right
from torch import Tensor
import json
import glob

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

# cases_dir = './data/' + config['dataset']['name'] + '/' + config['directories']['cases_dir']
# print cases in case_dir
# PRACTICE CASE DIR:
cases_dir = './data-wildfire/processed_sims/'
cases = glob.glob(os.path.join(cases_dir, '*'))
print(f"Found {len(cases)} cases in {cases_dir}")

def sort_cases_by_number(cases):
    name_sorted = sorted(cases)
    length_sorted = sorted(name_sorted, key=lambda x: len(x))
    return length_sorted

cases = sort_cases_by_number(cases)
cases = cases[:10]

# get data in single case and output to structure: ts, h, w
def get_one_case_data(case_dir: Path, variables) -> Tuple[np.ndarray]:
    var_list = [] # dictionary of the stacked frames (ts, h, w) for each var
    for var in tqdm(variables, desc="stacking vars"):
        vardir = Path.joinpath(case_dir, var)
        frame_files = sort_cases_by_number(glob.glob(os.path.join(vardir, '*')))
        print(f"Found {len(frame_files)} files for variable {var} in {vardir}")
        frames = [np.load(f) for f in frame_files] # h, w
        stacked = np.stack(frames, axis=0) # ts, h, w
        var_list.append(stacked)

    case_data = np.stack(var_list, axis=1) # c (vars), ts, h, w
    return case_data

# test a case
test_case = cases[0]
# get_one_case_data(Path(test_case), ['toa', 'burnscar'])

df = pd.read_csv('./input_tracking.txt')

# run,xign,yign,fuel,slp,asp,ws,wd,m1,m10,m100,cc,ch,cbh,cbd,lhc,lwc,firearea
# 0,616.6,179.3,104,24,56,17.0,302.5,6.0,7.0,8.0,0,0,0,0,90.0,120.0,1464.7
# create 128 by 128 constant arrays
def create_constant_input_arrays(input_row: pd.Series, num_timesteps: int=289, reference_shape=[128, 128]) -> Dict[str, np.ndarray]:
    h, w = reference_shape
    input_vars = ['fuel', 'slp', 'asp', 'ws', 'wd', 'm1', 'm10', 'm100', 'lhc', 'lwc']
    input_arrays = {}
    
    for var in input_vars:
        if var in input_row.index:
            constant_value = input_row[var]
            # Shape: (timesteps, height, width)
            input_arrays[var] = np.full((num_timesteps, h, w), constant_value, dtype=np.float32)
    return input_arrays

# Example usage
input_row = df.iloc[0]  # Get the first row of the DataFrame
input_arrays = create_constant_input_arrays(input_row)

# Print the shapes of the created arrays
for var, arr in input_arrays.items():
    print(f"{var}: {arr.shape}")

def concatenate_input_outputs_for_case(case_idx):
    output_arrays = get_one_case_data(Path(cases[case_idx]), ['toa', 'burnscar'])
    input_dict = create_constant_input_arrays(df.iloc[case_idx])
    input_arrays_list = [np.expand_dims(array, axis=1) for var, array in input_dict.items()]  
    input_arrays = np.concatenate(input_arrays_list, axis=1)  # (c, ts, h, w)
    
    case_data = np.concatenate((output_arrays, input_arrays), axis=1)  # (c, ts, h, w)
    print(f"Input data shape: {case_data.shape}")
    return case_data

class CustomWildfireDataset(Dataset):
    def __init__(self, case_dirs, transform=None, target_transform=None):
        self.case_dirs=case_dirs
        self.transform=transform
        self.target_transform=target_transform
        self.load_data(case_dirs)
    
    def load_data(self, case_dirs: List[Path]):
        self.num_features = 0
        self.num_frames = []
        features = []
        case_ids = []

        for case_id, case_dir in enumerate(tqdm(case_dirs)):
            case_features = concatenate_input_outputs_for_case(case_id)  # (c, ts, h, w)
            ts, c, h, w = case_features.shape
            self.num_features = c
            self.num_frames.append(ts)
            case_ids.append(case_id)
            features.append(torch.tensor(case_features, dtype=torch.float32))
        
        self.features = torch.stack(features, dim=0)  # (num_cases, ts, c, h, w)
        self.case_ids = case_ids

        self.num_frames_before: List[int] = [
            sum(self.num_frames[: i + 1]) for i in range(len(self.num_frames))
        ]

    def idx_to_case_and_frame_idx(self, idx: int) -> Tuple[int, int]:
        case_idx = bisect_right(self.num_frames_before, idx)
        if case_idx==0:
            frame_idx = idx
        else:
            frame_idx = idx - self.num_frames_before[case_idx-1]
        return case_idx, frame_idx
    
    def __getitem__(self, idx):
        print(f"idx: {idx}, num_frames_before: {self.num_frames_before[:5]}")
        case_idx, frame_idx = self.idx_to_case_and_frame_idx(idx)
        print(f"case_id: {case_idx}, frame_idx: {frame_idx}")
        t = torch.tensor([frame_idx]).float()
        frame = self.features[case_idx][frame_idx]
        return t, frame
    
    def __len__(self) -> int:
        return self.num_frames_before[-1]
    

# Example usage
dataset = CustomWildfireDataset(cases, transform=None, target_transform=None)
# print(f"Number of cases: {len(dataset.case_ids)}")
# print(f"Number of frames in first case: {dataset.num_frames[0]}")
# print(f"Shape of first frame in first case: {dataset[0][1].shape}")  # Should be (c, h, w)
    
# get wildfire datasets
def get_wildfire_datasets(data_dir, seed: int = 0):
    case_dirs = sorted(glob.glob(os.path.join(data_dir, '*')))
    if not case_dirs:
        raise ValueError(f"No cases found in {data_dir}")
    
    case_dirs = case_dirs
    
    # Shuffle the cases for randomness
    random.seed(seed)
    random.shuffle(case_dirs)
    num_cases = len(case_dirs)
    num_train = int(0.8 * num_cases)
    num_val = int(0.1 * num_cases)

    train_cases = case_dirs[:num_train]
    val_cases = case_dirs[num_train:num_train + num_val]
    test_cases = case_dirs[num_train + num_val:]

    print(f"num_train, num_val, num_test:{len(train_cases), len(val_cases), len(test_cases)}")
    train_dataset = CustomWildfireDataset(train_cases)
    val_dataset = CustomWildfireDataset(val_cases)
    test_dataset = CustomWildfireDataset(test_cases)

    return train_dataset, val_dataset, test_dataset

train_dataset, val_dataset, test_dataset = get_wildfire_datasets(cases_dir)

# save datasets to ./data/train, ./data/val, ./data/test

os.makedirs('./data/train', exist_ok=True)
os.makedirs('./data/val', exist_ok=True)
os.makedirs('./data/test', exist_ok=True)

torch.save(train_dataset, './data/train/train_dataset.pt')
torch.save(val_dataset, './data/val/val_dataset.pt')
torch.save(test_dataset, './data/test/test_dataset.pt')

print(f"Train dataset size: {len(train_dataset)}")
print(f"Validation dataset size: {len(val_dataset)}")
print(f"Test dataset size: {len(test_dataset)}")

print(len(val_dataset[0][1]))  # Should print the shape of the first frame in the validation dataset