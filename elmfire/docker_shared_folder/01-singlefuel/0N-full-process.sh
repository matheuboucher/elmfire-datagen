#!/bin/bash

# This bash file will set a number of runs, a domain size, and a simulation stop time.
# It will 

if [ -z "$1" ]; then
    echo "Usage: $0 <number_of_runs>"
    exit 1
fi

NUM_RUNS=$1

# get a starting run number from command line arguments
if [ -z "$2" ]; then
    START_RUN=0  # Default starting run number
else
    START_RUN=$2
fi

# set the /configuration/fromrun_cleaned.txt file to 0
fromrun_cleaned_path='./configuration/fromrun_cleaned.txt'
if [ ! -f $fromrun_cleaned_path ]; then
    echo "0" > $fromrun_cleaned_path
else
    echo "0" > $fromrun_cleaned_path
fi

RUN_DIR="./cases"
SIM_DIR="./sims"

# start a timer for speed testing
START_TIME=$(date +%s)

# bash 0N-run.sh $NUM_RUNS $START_RUN
python3 ./postprocess/add_firearea_to_input_tracking.py

# start a timer for speed testing
START_TIME2=$(date +%s)
python3 ./postprocess/elmfire_postprocessor.py
# End of postprocessing
# End timer and calculate duration
END_TIME2=$(date +%s)
DURATION2=$((END_TIME2 - START_TIME2))
# send time to ./output_check/timings.csv, directory already made
echo "postprocessing_duration: $DURATION2 (s)" >> ./output_check/timings.csv

ELMFIRE_DIR="./elmfire_gen_files"
rm -f -r $ELMFIRE_DIR
mkdir $ELMFIRE_DIR
mkdir -p $ELMFIRE_DIR/inputs
mv ./inputs $ELMFIRE_DIR
mv ./outputs $ELMFIRE_DIR

# total duration time
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))
# send time to ./output_check/timings.csv, directory already made
echo "total_duration: $DURATION (s)" >> ./output_check/timings.csv