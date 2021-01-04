The `settings` directory must contain the following files (provided for the catchment case study):

1. The file `M_DECISIONS` (called `fuse_zDecisions_902.txt` in the case studies) describes the different options available in the FUSE modeling framework. These modeling decisions are described in detail by [Clark et al. (WRR, 2008)](http://dx.doi.org/10.1029/2007WR006735), except decision 9 described in [Henn et al. (WRR, 2015)](http://dx.doi.org/10.1002/2014WR016736).

2. The file `CONSTRAINTS` (called `fuse_zConstraints_snow.txt` in the case studies) defines in particular the default parameter values and lower and upper parameter bounds. The list of parameters corresponds to those described in [Clark et al. (WRR, 2008)](http://dx.doi.org/10.1029/2007WR006735) and [Henn et al. (WRR, 2015)](http://dx.doi.org/10.1002/2014WR016736).

3. The file `MOD_NUMERIX` (called `fuse_zNumerix.txt` in the case studies) defines decisions regarding the numerical solution technique. Examples of the impact of these decisions are described by [Clark and Kavetski (WRR 2010)](http://dx.doi.org/10.1029/2009WR008894) and [Kavetski and Clark (WRR 2010)](http://dx.doi.org/10.1029/2009WR008896).

4. The file `FORCINGINFO` (called `input_info.txt` in the case studies) provides metadata for the NetCDF input file. It defines the name and units of the variables in the input file.
