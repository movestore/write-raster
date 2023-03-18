# Write Raster
MoveApps

Github repository: *github.com/movestore/write-raster*

## Description
Transformation of the input data to a raster that can be downloaded.

## Documentation
The input Movement data set is transformed into a `st_sf` lines data set and projected in the 'area equal distance' projection (aeqd) centering in the midpoint of all data locations. For each individual track, then the line is rasterized into a grid of user-defined grid size and data-related bounding box. Rasters of all individuals are merged and returned as a raster file of user-selected data type. This file can be downloaded as an artefact in MoveApps. The original data set is also passed on as output to a possible next App. 

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
`data_raster.***`: Raster file for download in four possible formats: `.grd`, `asc`, `.nc` or `.tif.

### Settings 
**Value of your grid size (`grid`):** This parameter allows the user to decide on the grid cell size (in metres) that the raster shall be calculated in. One numeric value has to be entered.

**Raster file type (`typ`):** The user can select one of four different raster output file formats: `raster`, `ascii`, `netCDF` or `GeoTiff`.

### Null or error handling:
**Setting `grid`:** If no grid cell size is defined, a warning message is given and the default value of 100000 = 100 km used. This might not fit well with some data sets, please configure it properly. A numeric value is required, else will lead to errors.

**Setting `typ`:** If no raster file type is provided, a warning message is given and the default value of 'raster' leading to `.grd` files is applied.

**Data:** The full input data set is returned for further use in a next App and cannot be empty.
