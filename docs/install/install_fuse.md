## Notes on the computing environment

This page will guide you through the installation of FUSE. Before you get started, please note that:

1. below we assume that you will be compiling and running FUSE in a Linux/UNIX environment - for OS X/macOS, have a look at this [page](https://summa.readthedocs.io/en/latest/installation/SUMMA_on_OS_X/) of the SUMMA manual,
2. you will need a Fortran compiler - FUSE was developed and tested using `ifort`, which we recommend if you have no previous experience with Fortran compilers, note that to use `ifort` you might have to load the associated module (try `module avail` and then `module add`),
3. you will need access to the NetCDF and HDF libraries - use the libraries compiled with the compiler you selected above, you might have to load them (again, try `module avail` and `module add`), then try `which ncdump`, as this should give you an idea of the path to the NetCDF libraries, the path to the HDF libraries should be similar. Note that these paths are machine-dependent.

## 1. Fork the FUSE repository
1. Fork the [FUSE repository](https://github.com/naddor/fuse) to a directory `$(MASTER)` on your machine (see the [SUMMA manual](http://summa.readthedocs.io/en/latest/development/SUMMA_and_git/) for a step-by-step procedure)
2. Change directory to `$(MASTER)/build/` and edit the `Makefile` by:
    * defining the name of the master directory (line 11),
    * defining the fortran compiler (line 27),
    * defining the path to the NetCDF and HDF libraries (lines 34-35).

## 2. Compile SCE
1. FUSE relies on an algorithm called *shuffled complex evolution* (SCE) for automated parameter estimation. SCE code (`$(MASTER)/build/FUSE_SRC/FUSE_SCE/sce.f`) was written in Fortran 77, so it must be compiled separately, before FUSE is compiled. If you use `ifort`, try the following flags:

```
ifort -O2 -c -fixed sce_16plus.f
```

If necessary, rename the compiled file, so that it can be found by the `Makefile`, which by default will be looking for a file named `sce_16plus.o`.

## 3. Compile FUSE

4. Compile the FUSE code (type `make`).
5. Change to `$(MASTER)/bin/` and try running FUSE by typing `./fuse.exe`.

If the output is `1st command-line argument is missing (fileManager)`, you have probably compiled FUSE correctly. 

<a id="infile_file_formats"></a>
