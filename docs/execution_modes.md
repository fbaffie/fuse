## Spatial configurations

FUSE can be run for an individual catchment or on grid. The latter mode is designed to run FUSE over large domains and to conduct impact assessments using gridded simulations from climate models.

FUSE will automatically adapt to the spatial configuration of the input files: the dimension of the NetCDF files will determine if FUSE is run at the catchment or grid-scale. FUSE will look for the variables `lat` and `lon` and if they are arrays, it will run on the grid they define.



## Parameter estimation modes

Run can use three modes, all rely on the same syntax and the same three arguments:

`$1` is the file manager,
`$2` is the basin ID,
`$3` is the FUSE mode.

Run FUSE using default parameter values:
```
./fuse.exe fm_catch.txt us_09066300 run_def
```

then calibrate it:

```
./fuse.exe fm_catch.txt us_09066300 calib_sce
```

then run it with the best SCE parameter set:

```
./fuse.exe fm_catch.txt us_09066300 run_best
```

Note that because the gridded data does not contain streamflow, FUSE cannot be calibrated using SCE. Instead, FUSE can be run using pre-defined parameter sets using the `run_pre` mode (description to be added).   
