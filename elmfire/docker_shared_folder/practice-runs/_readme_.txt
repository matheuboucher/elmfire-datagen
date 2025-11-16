Single Fuel Dataset

This dataset contains 500 simulations for a single fuel
(GR1: short, sparse dry climate grass). Other, non-canopy, input parameters were
uniformly within realistic ranges.

id,name,range
X_IGN,x_ignition,[-960,960]
Y_IGN,y_ignition,[-960,960]
SLP,slope,[0,45]
ASP,aspect,[0,360]
WS,wind_speed,[0,30]
WD,wind_direction,[0,360]
M1,m1_moisture, see allowed scenarios
M10,m10_moisture, see allowed scenarios
M100,m100_moisture, see allowed scenarios
CC,canopy_cover = 0
CH,canopy_height = 0
CBH,canopy_base_height = 0
CBD,canopy_bulk_density = 0
MLH,live_herbaceous, see allowed scenarios
MLW,live_woody, see allowed scenarios
DEM,elevation = 0

- 01-singlefuel-realistic:
  - Fuel model: GR1 (#101)
- 02-multifuel-realistic:
  - Fuel models:
    - 101-109, 121-124, 141-149, 161-165, 181-189, 201-204
  - LHC > 30%
  - LWC > 60%
  - Dead fuel moistures > 2% (maybe, tend not to go below 2-3% but they can)
  - Ignore canopy for now (set all variables to 0 at the moment, and address later on)
  - Live fuel moisture content scenarios L1-L4 [MLH,MLW]:
    - [30,60],[60,90],[90,120],[120,150]
  - Dead moisture content scenarios D1-D4 [M1,M10,M100]:
    - [3,4,5],[6,7,8],[9,10,11],[12,13,14]
