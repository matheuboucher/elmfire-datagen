#!/bin/bash
CENTER_LON=-121.180742
CENTER_LAT=41.829386
WINDSPD=16.1
WINDIR=29.9
M1C=9.0
M10C=10.0
M100C=11.0
LHC=120.0
LWC=150.0

# Get fuel data for a tile:
$ELMFIRE_BASE_DIR/cloudfire/fuel_wx_ign.py \
    --do_wx=False --do_ignition=False \
    --center_lon=$CENTER_LON --center_lat=$CENTER_LAT \
    --fuel_source='landfire' --fuel_version='2.4.0' \
    --outdir='./fuel' --name='data03' \
    --west_buffer=2 --east_buffer=2 \
    --north_buffer=2 --south_buffer=2 \

SIMULATION_TSTOP=44200.0 # Simulation stop time (seconds)

# CONSTANT CONDITIONS (like Tutorial 1)
NUM_FLOAT_RASTERS=5
FLOAT_RASTER[1]=ws   ; FLOAT_VAL[1]=$WINDSPD # Wind speed, mph
FLOAT_RASTER[2]=wd   ; FLOAT_VAL[2]=$WINDIR  # Wind direction, deg
FLOAT_RASTER[3]=m1   ; FLOAT_VAL[3]=$M1C  # 1-hr   dead moisture content, %
FLOAT_RASTER[4]=m10  ; FLOAT_VAL[4]=$M10C  # 10-hr  dead moisture content, %
FLOAT_RASTER[5]=m100 ; FLOAT_VAL[5]=$M100C  # 100-hr dead moisture content, %

LH_MOISTURE_CONTENT=$LHC # Live herbaceous moisture content, percent
LW_MOISTURE_CONTENT=$LWC # Live woody moisture content, percent

# End specifing inputs - no need to edit from here down

ELMFIRE_VER=${ELMFIRE_VER:-2025.0212}

. ../functions/functions.sh

SCRATCH=./scratch
INPUTS=./inputs
OUTPUTS=./outputs

rm -f -r $SCRATCH $INPUTS $OUTPUTS
mkdir $SCRATCH $INPUTS $OUTPUTS

# Copy tutorial3's elmfire.data.in and modify it for constant conditions
cp elmfire.data.in $INPUTS/elmfire.data

# Convert from transient to constant live fuel moisture
sed -i 's/USE_CONSTANT_LH.*= .FALSE./USE_CONSTANT_LH = .TRUE./' $INPUTS/elmfire.data
sed -i 's/USE_CONSTANT_LW.*= .FALSE./USE_CONSTANT_LW = .TRUE./' $INPUTS/elmfire.data
sed -i '/MLH_FILENAME/d' $INPUTS/elmfire.data
sed -i '/MLW_FILENAME/d' $INPUTS/elmfire.data

# Remove the NUM_METEOROLOGY_TIMES line (only needed for transient weather)
sed -i '/NUM_METEOROLOGY_TIMES/d' $INPUTS/elmfire.data

# Add constant live moisture content parameters in the INPUTS section
sed -i '/M100_FILENAME/a LH_MOISTURE_CONTENT = 30.0\nLW_MOISTURE_CONTENT = 60.0' $INPUTS/elmfire.data

tar -xvf ./fuel/data03.tar -C $INPUTS
rm -f $INPUTS/m*.tif $INPUTS/w*.tif $INPUTS/l*.tif $INPUTS/ignition*.tif $INPUTS/forecast_cycle.txt

XMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMIN=`gdalinfo $INPUTS/fbfm40.tif | grep 'Lower Left'  | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`
XMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f1 | xargs`
YMAX=`gdalinfo $INPUTS/fbfm40.tif | grep 'Upper Right' | cut -d'(' -f2 | cut -d, -f2 | cut -d')' -f1 | xargs`
XCEN=`echo "0.5*($XMIN + $XMAX)" | bc`
YCEN=`echo "0.5*($YMIN + $YMAX)" | bc`
A_SRS=`gdalsrsinfo $INPUTS/fbfm40.tif | grep PROJ.4 | cut -d: -f2 | xargs` # Spatial reference system
CELLSIZE=`gdalinfo $INPUTS/fbfm40.tif | grep 'Pixel Size' | cut -d'(' -f2 | cut -d, -f1` # Grid size in meters

# CREATE TEMPLATE RASTER with same dimensions as fuel data
gdal_calc.py -A $INPUTS/fbfm40.tif --NoDataValue=-9999 --type=Float32 --outfile="$SCRATCH/float.tif" --calc="A*0.0"

# Create constant float input rasters (like Tutorial 1)
for i in $(eval echo "{1..$NUM_FLOAT_RASTERS}"); do
   gdal_calc.py -A $SCRATCH/float.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --type=Float32 --outfile="$INPUTS/${FLOAT_RASTER[i]}.tif" --calc="A + ${FLOAT_VAL[i]}"
done

# Create phi and adj rasters (required by ELMFIRE)
gdal_calc.py -A $SCRATCH/float.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --type=Float32 --outfile="$INPUTS/phi.tif" --calc="A + 1.0"
gdal_calc.py -A $SCRATCH/float.tif --co="COMPRESS=DEFLATE" --co="ZLEVEL=9" --NoDataValue=-9999 --type=Float32 --outfile="$INPUTS/adj.tif" --calc="A + 1.0"

# Set inputs in elmfire.data
replace_line COMPUTATIONAL_DOMAIN_XLLCORNER $XMIN no
replace_line COMPUTATIONAL_DOMAIN_YLLCORNER $YMIN no
replace_line COMPUTATIONAL_DOMAIN_CELLSIZE $CELLSIZE no
replace_line SIMULATION_TSTOP $SIMULATION_TSTOP no
replace_line DTDUMP $SIMULATION_TSTOP no
replace_line A_SRS "$A_SRS" yes
replace_line LH_MOISTURE_CONTENT $LH_MOISTURE_CONTENT no
replace_line LW_MOISTURE_CONTENT $LW_MOISTURE_CONTENT no
replace_line 'X_IGN(1)' $XCEN no
replace_line 'Y_IGN(1)' $YCEN no

# Execute ELMFIRE
elmfire_$ELMFIRE_VER ./inputs/elmfire.data

# Postprocess
for f in ./outputs/*.bil; do
   gdal_translate -a_srs "$A_SRS" -co "COMPRESS=DEFLATE" -co "ZLEVEL=9" $f ./outputs/`basename $f | cut -d. -f1`.tif
done

# Check if time_of_arrival files exist before creating contours
if ls ./outputs/time_of_arrival*.tif 1> /dev/null 2>&1; then
    gdal_contour -i 3600 `ls ./outputs/time_of_arrival*.tif` ./outputs/hourly_isochrones.shp
else
    echo "Warning: No time_of_arrival files found for contour generation"
fi

# Clean up and exit:
rm -f -r ./outputs/*.csv ./outputs/*.bil ./outputs/*.hdr $SCRATCH

exit 0