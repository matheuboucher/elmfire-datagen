import rasterio
import glob
from rasterio.plot import show
import matplotlib.pyplot as plt
from matplotlib import cm
import pandas as pd
import seaborn as sns

case_dir = '../cases'
input_file = '../configuration/input_tracking.txt'

# prints max and min for raster files of a case
def print_case_info(case_num):
    case_path = f'{case_dir}/case_{case_num}'
    print(f'Case {case_num} path: {case_path}')
    
    patterns = ["flin_*.tif", "time_of_arrival_*.tif", "vs_*.tif"]

    for pattern in patterns:
        for filepath in glob.glob(f"{case_path}/{pattern}"):
            with rasterio.open(filepath) as src:
                data = src.read(1)
                print(f"{filepath}: shape={data.shape}, min={data.min()}, max={data.max()}")

# prints case info for all cases
def print_all_cases_info(num_cases, max_cases=10):
    for i in range(1, min(num_cases, max_cases) + 1):
        print_case_info(i)

# plot rasters for one case
def plot_case(case_num):
    case_path = f'{case_dir}/case_{case_num}'
    # plot the three rasters
    flin_files = glob.glob(f"{case_path}/flin_*.tif")
    time_files = glob.glob(f"{case_path}/time_of_arrival_*.tif")
    vs_files = glob.glob(f"{case_path}/vs_*.tif")

    if flin_files and time_files and vs_files:
        flin_file = flin_files[0]
        time_file = time_files[0]
        vs_file = vs_files[0]

        with rasterio.open(flin_file) as src:
            flin_data = src.read(1)
        
        with rasterio.open(time_file) as src:
            time_data = src.read(1)
        
        with rasterio.open(vs_file) as src:
            vs_data = src.read(1)

        # Plotting the rasters
        fig, axs = plt.subplots(1, 3, figsize=(15, 5))
        
        axs[0].imshow(flin_data, cmap='hot')
        axs[0].set_title('Flame Length (flin)')
        
        axs[1].imshow(time_data, cmap='viridis')
        axs[1].set_title('Time of Arrival')
        
        axs[2].imshow(vs_data, cmap='coolwarm')
        axs[2].set_title('Spread Rate (vs)')
        
        plt.tight_layout()
        plt.show()
    else:
        print("One or more output files not found. Please check the output directory and file patterns.")

# plot all of one raster type for all cases in a grid
def plot_all_cases_raster(pattern, num_cases, max_cols=5):
    print(f'Plotting all cases for raster pattern: {pattern}')
    fig, axes = plt.subplots(nrows=1, ncols=min(num_cases, max_cols), figsize=(15, 5))
    for i in range(0, min(num_cases, max_cols)):
        case_path = f'{case_dir}/case_{i}'
        filepath = glob.glob(f"{case_path}/{pattern}")
        if filepath:
            with rasterio.open(filepath[0]) as src:
                data = src.read(1)
                ax = axes[i - 1]
                show(data, ax=ax, cmap=cm.viridis)
                ax.set_title(f"Case {i}")
                ax.set_axis_off()
    plt.tight_layout()
    plt.show()
    
# plot the distribution of all of the input parameters for all cases
# Updated header: run,xign,yign,fuel,slp,asp,ws,wd,m1,m10,m100,cc,ch,cbh,cbd,lhc,lwc,firearea
def plot_input_distribution():
    df = pd.read_csv(input_file)

    print(f'Input parameters distribution from {input_file}:')
    print(df.describe())

    # Plotting the distributions of each parameter (excluding run and firearea for this plot)
    params_to_plot = [col for col in df.columns if col not in ['firearea']]
    num_params = len(params_to_plot)
    
    # Calculate subplot grid dimensions
    cols = 4
    rows = (num_params + cols - 1) // cols
    
    plt.figure(figsize=(15, 4 * rows))
    for i, column in enumerate(params_to_plot, start=1):
        plt.subplot(rows, cols, i)
        sns.histplot(df[column], kde=True)
        plt.title(column)
        plt.xlabel('')
        plt.ylabel('Frequency')
    
    plt.tight_layout()
    plt.show()

# for each input, plot with run number on x-axis
def plot_input_vs_run():
    df = pd.read_csv(input_file)

    print(f'Input parameters vs Run number from {input_file}:')
    print(df.describe())

    # Plotting each parameter against run number (excluding run and firearea)
    params_to_plot = [col for col in df.columns if col not in ['run', 'firearea']]
    num_params = len(params_to_plot)
    
    # Calculate subplot grid dimensions
    cols = 4
    rows = (num_params + cols - 1) // cols

    plt.figure(figsize=(15, 4 * rows))
    for i, column in enumerate(params_to_plot, start=1):
        plt.subplot(rows, cols, i)
        sns.lineplot(x=df.index + 1, y=df[column])
        plt.title(column)
        plt.xlabel('Run Number')
        plt.ylabel(column)
    
    plt.tight_layout()
    plt.show()

# quick check to make sure all of the input parameters are within the expected ranges
# Updated header: run,xign,yign,fuel,slp,asp,ws,wd,m1,m10,m100,cc,ch,cbh,cbd,lhc,lwc,firearea
def check_input_ranges():
    df = pd.read_csv(input_file)

    print(f'Checking input parameter ranges from {input_file}:')
    print(df.columns)
    
    # Define expected ranges
    expected_ranges = {
        'ws': (0, 31),
        'wd': (0.0, 360.0),
        'm1': (1.0, 40.0),
        'm10': (1.0, 40.0),
        'm100': (1.0, 40.0),
        'lhc': (30.0, 120.0),
        'lwc': (60.0, 150.0),
        'cc': (0, 100),
        'ch': (0, 5),
        'cbh': (0, 2),  # Canopy base height should not exceed canopy height
        'cbd': (0, 40),
        'slp': (0, 45),
        'asp': (0, 360),
        'fuel': (101,204),
        'firearea': (0, 3840)  # Fire area should be non-negative
    }
    
    # Check all columns except 'run' and coordinates
    for column in df.columns:
        if column in ['run', 'xign', 'yign']:
            continue  # Skip run number and coordinates
            
        if column in expected_ranges:
            min_val, max_val = expected_ranges[column]
            if not df[column].between(min_val, max_val).all():
                print(f"Warning: {column} values out of range: {df[column].min()} to {df[column].max()}")
                # print value out of range
                out_of_range = df[~df[column].between(min_val, max_val)]
                print(f"Out of range values for {column}:\n{out_of_range[column]}")
            else:
                print(f"{column} values are within the expected range: {min_val} to {max_val}")
        else:
            print(f"{column} is not a recognized input parameter.")

# Updated to use the correct default input file name
def analyze_fire_area(input_file=input_file):
    df = pd.read_csv(input_file)

    print(f'Analyzing fire area from {input_file}:')
    
    # Check if 'firearea' column exists
    if 'firearea' in df.columns:
        print(f"Fire area statistics:\n{df['firearea'].describe()}")
        
        # Get input parameters (exclude run, coordinates, and firearea)
        input_params = [col for col in df.columns if col not in ['run', 'xign', 'yign', 'firearea']]
        num_params = len(input_params)
        
        # Calculate subplot grid dimensions
        cols = 4
        rows = (num_params + cols - 1) // cols
        
        # Plotting fire area against each input parameter
        plt.figure(figsize=(15, 4 * rows))
        for i, column in enumerate(input_params, start=1):
            plt.subplot(rows, cols, i)
            sns.scatterplot(x=df[column], y=df['firearea'])
            plt.title(f'Fire Area vs {column}')
            plt.xlabel(column)
            plt.ylabel('Fire Area')
        
        plt.tight_layout()
        plt.show()
    else:
        print("No 'firearea' column found in the input file.")

# Updated to use the correct default input file name
def fire_area_covariance(input_file=input_file):
    df = pd.read_csv(input_file)

    print(f'Calculating fire area covariance from {input_file}:')
    
    # Check if 'firearea' column exists
    if 'firearea' in df.columns:
        covariances = {}
        # Get input parameters (exclude run, coordinates, and firearea)
        input_params = [col for col in df.columns if col not in ['run', 'xign', 'yign', 'firearea']]
        
        for column in input_params:
            cov = df['firearea'].cov(df[column])
            covariances[column] = cov
            print(f"Covariance between fire area and {column}: {cov}")
        
        return covariances
    else:
        print("No 'firearea' column found in the input file.")
        return None

# Updated to use the correct default input file name
def fire_area_correlation(input_file=input_file):
    df = pd.read_csv(input_file)

    print(f'Calculating fire area correlation from {input_file}:')
    
    # Check if 'firearea' column exists
    if 'firearea' in df.columns:
        correlation_matrix = df.corr()
        fire_area_corr = correlation_matrix['firearea'].drop('firearea')
        
        print("Correlation coefficients with fire area:")
        print(fire_area_corr)
        
        # Plotting heatmap of the correlation coefficients
        plt.figure(figsize=(12, 10))
        sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', fmt='.2f')
        plt.title('Correlation Coefficients Heatmap')
        plt.show()
        
        return fire_area_corr
    else:
        print("No 'firearea' column found in the input file.")
        return None

# Updated to use the correct default input file name
def select_cases_with_most_fire_area(num_cases, input_file=input_file):
    df = pd.read_csv(input_file)

    print(f'Selecting top {num_cases} cases with the most fire area from {input_file}:')
    
    if 'firearea' in df.columns:
        top_cases = df.nlargest(num_cases, 'firearea')
        print(f"Top {num_cases} cases with the most fire area:\n{top_cases}")
        
        # Save to a new file
        top_cases.to_csv('top_fire_area_cases.csv', index=False)
        return top_cases
    else:
        print("No 'firearea' column found in the input file.")
        return None

# get the cases with the most fire area and plot their time of arrival on subplots
def plot_top_fire_area_cases(num_cases, exclude_firearea=3419.5):
    df = pd.read_csv(input_file)

    print(f'Plotting top {num_cases} cases with the most fire area from {input_file} (excluding fire area == {exclude_firearea}):')
    
    if 'firearea' in df.columns:
        # Filter out cases with specified firearea value if provided
        if exclude_firearea is not None:
            df_filtered = df[df['firearea'] != exclude_firearea]
        else:
            df_filtered = df
            
        top_cases = df_filtered.nlargest(num_cases, 'firearea')
        print(f"Top {num_cases} cases with the most fire area:\n{top_cases[['run', 'firearea']]}")
        
        # Set up subplots - adjust grid based on number of cases
        cols = min(5, num_cases)
        rows = (num_cases + cols - 1) // cols
        
        fig, axes = plt.subplots(nrows=rows, ncols=cols, figsize=(3.6 * cols, 3.5 * rows))
        if rows == 1:
            axes = axes.reshape(1, -1) if num_cases > 1 else [axes]
        axes = axes.flatten()
        
        for ax_idx, (i, row) in enumerate(top_cases.iterrows()):
            case_num = row['run']
            x_ign = row['xign']
            y_ign = row['yign']
            case_path = f'{case_dir}/case_{int(case_num)}'
            print(case_path)
            time_files = glob.glob(f"{case_path}/time_of_arrival_*.tif")
            
            if time_files and ax_idx < len(axes):
                with rasterio.open(time_files[0]) as src:
                    time_data = src.read(1)
                    ax = axes[ax_idx]
                    show(time_data, ax=ax, cmap=cm.viridis)
                    ax.set_title(
                        f"Case {case_num}\nFire Area: {row['firearea']:.1f}\n"
                        f"x_ign: {x_ign:.1f}, y_ign: {y_ign:.1f}"
                    )
            else:
                if ax_idx < len(axes):
                    print(f"No time of arrival file found for case {case_num}.")
        
        # Hide unused subplots if less than total grid
        for j in range(len(top_cases), len(axes)):
            axes[j].set_visible(False)
        
        plt.tight_layout()
        plt.show()
    else:
        print("No 'firearea' column found in the input file.")

# get all of the fire cases for which there is 0 fire area and plot the distribution of their x and y ignition points
def plot_zero_fire_cases(firearea_threshold=0.2):
    df = pd.read_csv(input_file)

    print(f'Plotting cases with zero fire area from {input_file}:')
    
    if 'firearea' in df.columns:
        zero_fire_cases = df[df['firearea'] <= firearea_threshold]
        print(f"Cases with <= {firearea_threshold} fire area:\n{zero_fire_cases[['run', 'xign', 'yign', 'firearea']]}")
        
        plt.figure(figsize=(10, 6))
        sns.scatterplot(data=zero_fire_cases, x='xign', y='yign', hue='run', palette='viridis', s=100)
        plt.title(f'Ignition Points of Cases with <= {firearea_threshold} Fire Area')
        plt.xlabel('X Ignition Point')
        plt.ylabel('Y Ignition Point')
        plt.legend(title='Run Number', bbox_to_anchor=(1.05, 1), loc='upper left')
        plt.tight_layout()
        plt.show()
    else:
        print("No 'firearea' column found in the input file.")

# plot fire area against each input parameter
def plot_fire_area_vs_inputs():
    df = pd.read_csv(input_file)

    print(f'Plotting fire area against each input parameter from {input_file}:')
    
    if 'firearea' in df.columns:
        # Get input parameters (exclude run, coordinates, and firearea)
        input_params = [col for col in df.columns if col not in ['run', 'xign', 'yign', 'firearea']]
        num_params = len(input_params)
        
        # Calculate subplot grid dimensions
        cols = 4
        rows = (num_params + cols - 1) // cols
        
        plt.figure(figsize=(15, 4 * rows))
        for i, column in enumerate(input_params, start=1):
            plt.subplot(rows, cols, i)
            sns.scatterplot(x=df[column], y=df['firearea'])
            plt.title(f'Fire Area vs {column}')
            plt.xlabel(column)
            plt.ylabel('Fire Area')
        
        plt.tight_layout()
        plt.show()
    else:
        print("No 'firearea' column found in the input file.")

# get the maximum fuel model for which there is fire area
def max_fuel_model_fire_area():
    df = pd.read_csv(input_file)

    print(f'Finding maximum fuel model with fire area from {input_file}:')
    
    if 'firearea' in df.columns and 'fuel' in df.columns:
        max_fuel_model = df[df['firearea'] > 0]['fuel'].max()
        print(f"Maximum fuel model with fire area > 0: {max_fuel_model}")
        return max_fuel_model
    else:
        print("No 'firearea' or 'fuel' column found in the input file.")
        return None
    
# plot the distribution of a given variable for all cases with fire area under a given threshold
def plot_var_dist_for_firearea_underthreshold(variable='yign', threshold=0.2):
    df = pd.read_csv(input_file)

    print(f'Plotting distribution of {variable} for cases with fire area under {threshold} from {input_file}:')
    
    if 'firearea' in df.columns and variable in df.columns:
        filtered_cases = df[df['firearea'] <= threshold]
        print(f"Number of cases with fire area <= {threshold}: {len(filtered_cases)}")
        
        plt.figure(figsize=(10, 6))
        sns.histplot(filtered_cases[variable], kde=True)
        plt.title(f'Distribution of {variable} for Cases with Fire Area <= {threshold}')
        plt.xlabel(variable)
        plt.ylabel('Frequency')
        plt.tight_layout()
        plt.show()
    else:
        print(f"No 'firearea' or '{variable}' column found in the input file.")