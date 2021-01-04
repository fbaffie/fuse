The `input` directory must contain the following files (provided for the catchment case study):

## Forcing time series

**Purpose:**  The file `forcefile` (called `us_09066300_input.nc` in the catchment case study) contains the input data in a 2D (resp. 3D) arrays for modeling at the catchment (resp. grid) scale. The name of this file is made by appending the `suffix_forcing` defined in the `FILEMANAGER` (see B) to the basin ID (see E `$2`).

**Requirements:** The dimension of the NetCDF files will determine if FUSE is run at the catchment or grid-scale. FUSE will look for the variables `lat` and `lon` and if they are arrays, it will run on the grid they define. This means that NetCDF input files for a single catchment must also include the variables `lat` and `lon`.

**Format:** NetCDF

## Elevation bands

**Purpose:** The file `BFILE` (called `us_09066300_elev_bands.nc` in the catchment case study) describe the elevation bands required when the snow module is on. The dimensions of this file must match that of `forcefile`. The name of this file is made by appending the `suffix_elev_bands` defined in the `FILEMANAGER` (see B) to the basin ID (see E `$2`).

**Requirements:**
