## Data for two spatial configurations

To get you started with FUSE, we provide data for two test cases. They will enable you to test FUSE in its two spatial configurations: FUSE can be run for an individual catchment or on grid (see [Execution modes](../../../execution_modes/)).

The dimension of the NetCDF files will determine if FUSE is run at the catchment or grid-scale. FUSE will look for the variables `lat` and `lon` and if they are arrays, it will run on the grid they define.

## Catchment test case

Observed atmospheric forcing and streamflow estimates for the catchment [Middle Creek near Minturn](https://waterdata.usgs.gov/nwis/inventory/?site_no=09066300&agency_cd=USGS&) in Colorado, USA  - data available [here [0.5MB]](
https://dl.dropboxusercontent.com/s/f6omcgz8hsirlr0/fuse_catch.zip?dl=0) for download. This catchment is part of the CAMELS data set.  

## Grid test case

Atmospheric forcing simulated by a climate model on a 1/8th degree grid for a 58 x 28 grid cells domain - data available [here [42MB]](
https://dl.dropboxusercontent.com/s/g5193e0n01ao33d/fuse_grid.zip?dl=0) for download.
