## Overview

Running a hydrological model means making a wide range of decisions, which will influence the simulations in different ways and to different extents. Our goal with FUSE is enable users to be in charge of these decisions, so that they can understand their effects, and thereby, develop and use better models. The modelling decisions FUSE enables can be split in four main groups:

**Model structure** FUSE is a master template for model generation. It enables users to create and run their own model structure(s) by combining pre-existing modules (described in the [module section](/fuse/modules/1_precip)).

**Parameter estimation** The parameters of any FUSE model structure can be estimated in different ways. One can for instance decide to use default parameter values or to run a calibration - the different options are described in the [Parameter estimation section](/fuse/modes/execution_modes).

**Spatial configuration** Any FUSE model structure can be run either for individual catchment or using a grid - see the [Spatial modes section](/fuse/modes/spatial_modes).

**Numerical scheme** The equation underpinning any FUSE model structure can be solved using a range of numerical methods - see the [Numerical methods section](/fuse/modes/numerical_methods).

## FUSE execution

To execute FUSE, provide the 3-4 following arguments (see example below):

  1. FUSE file manager, which sets the FUSE file system (see [here](/fuse/files/file_manager)),
  2. region ID, which will be use to load the forcing and to name the output files,
  3. parameter estimation mode (see [here](/fuse/modes/execution_modes)),
  4. parameter file, only when using the `run_pre_catch` and `run_pre_grid` modes (see [here](/fuse/modes/execution_modes)).

For example:

```
./fuse.exe fm_catch.txt us_09066300 run_def
```
