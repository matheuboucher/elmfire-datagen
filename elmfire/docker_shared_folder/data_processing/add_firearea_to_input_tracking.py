import json
import sys

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

fire_file = config['dataset']['name'] + "/fire_area_most_recent_run.txt"
output_file = "./configuration/input_tracking.txt"

with open(fire_file, "r") as f:
    fire_areas = [line.rstrip() for line in f]
    # f.write("")

# append the fire area to the end of each line in input_tracking.txt
with open(output_file, "r") as f:
    lines = f.readlines()

if len(fire_areas)+1 != len(lines):
    raise ValueError(f"Number of fire areas ({len(fire_areas)+1}) does not match number of lines in {output_file} ({len(lines)})")

# have to append the fire area to each line in input_tracking.txt but don't do anything
# to the header line
with open(output_file, "w") as f:
    f.write(lines[0])  # write the header line
    for i in range(len(fire_areas)):
        fire_area = fire_areas[i]  # skip header line
        line = lines[i+1]
        f.write(f"{line.strip()}{fire_area.strip()}\n")
