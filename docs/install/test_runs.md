## Run FUSE for the catchment test case

The three next steps will enable you to quickly test if FUSE is running as expected. The instruction were purposefully kept short, but each element is described in detail in the rest of the documentation.

1. If you haven't yet, download the data for the [catchment test case](https://dl.dropboxusercontent.com/s/f6omcgz8hsirlr0/fuse_catch.zip?dl=0) to a directory `$(data_catch)`.
1. Update the lines 3 to 5 of `fm_catch.txt` (this is the file manager) using the path of the `$(data_catch)`.
1. Run FUSE using default parameter values (note `run_def` below):

```
./fuse.exe path_to_data_catch/fm_catch.txt us_09066300 run_def
```

If FUSE runs and terminates with `Done`, you have setup FUSE correctly.

## Run FUSE for the grid case study

1. If you haven't yet, download the data for the [grid test case](https://dl.dropboxusercontent.com/s/f6omcgz8hsirlr0/fuse_catch.zip?dl=0) to a directory `$(data_grid)`.
1. Update the lines 3 to 5 of `fm_grid.txt` (this is the file manager) using the path of the `$(data_grid)`.
1. Run FUSE using default parameter values (note `run_def` below):

```
./fuse.exe path_to_data_grid/fm_grid.txt cesm1-cam5 run_def
```

If FUSE runs and terminates with `Done`, you have setup FUSE correctly.
