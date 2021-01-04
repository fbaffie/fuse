## FUSE in a nutshell

The Framework for Understanding Structural Errors or [FUSE](https://github.com/naddor/fuse) is modular modelling framework (MMF) which enables the generation of a myriad of conceptual hydrological models by recombining elements from four commonly-used models.

FUSE was build from scratch to be modular, it offers several options for each important modelling decision and enables the addition of new modules. In contrast, most traditional hydrological models rely on a single model structure (most processes are simulated by a single set of equations). FUSE modularity makes it easier to i) understand differences between models, ii) run a large ensemble of models, iii) capture the spatial variability of hydrological processes and iv) develop and improve hydrological models in a coordinated fashion across the community.

## Description and credits

The FUSE initial implementation (FUSE1) is described in [Clark et al. (WRR, 2008)](http://dx.doi.org/10.1029/2007WR006735). This implementation (which will become FUSE2) was created with users in mind and significantly increases the usability and range of applicability of FUSE1. In particular, it involves four main additional features:

1. a snow module described in [Henn et al. (WRR, 2015)](http://dx.doi.org/10.1002/2014WR016736),
2. a calibration mode based on the shuffled complex evolution algorithm [(Duan et al., WRR, 1992)](http://dx.doi.org/10.1029/91WR02985),
3. a distributed mode enabling to run FUSE on a grid, and
4. all the input, output and parameter files are now NetCDF files.

## License

FUSE is distributed under the GNU Public License Version 3. For details see the file `LICENSE` in the FUSE root directory or visit the [online version](https://www.gnu.org/licenses/gpl-3.0.html).
