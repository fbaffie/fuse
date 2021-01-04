Run FUSE using default parameter values at the catchment scale:
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

where
`$1` is the file manager,
`$2` is the basin ID,
`$3` is the FUSE mode.
