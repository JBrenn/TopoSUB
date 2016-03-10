
=======
# TopoSUB
R package for landscape clustering and land-surface modelling
* preprocessing (landscape clustering, running LSM in parallel)
* postprocessing (read output, mapping)
* data analysis (specific variable postprocessing, e.g. drought indices)

TopoSUB is a R project for sampling the most important  aspects of land surface heterogeneity through a lumped scheme, allowing for the application of numerical land surface models (LSMs), like e.g. [GEOtop](https://github.com/geotopmodel), over large areas in mountain regions or other heterogeneous environments (Fiddes et al. 2012). The project was initiated by [Joel Fiddes](https://www.researchgate.net/profile/Joel Fiddes), the source code is available via the [toposub GitHub repository](https://github.com/joelfiddes/toposub).

#### Reference
Fiddes, J., & Gruber, S. (2012). TopoSUB: A tool for efficient large area numerical modelling in complex topography at sub-grid scales. Geoscientific Model Development, 5(5), 1245–1257. [DOI: 10.5194/gmd-5-1245-2012](http://doi.org/10.5194/gmd-5-1245-2012)

# How to start

First install the package with:

```R
install.packages("devtools")
library(devtools)
install_github("JBrenn/TopoSUB")
```

and then import the library with:

```R
library(TopoSUB)
```

# Simulation Workflow

A TopoSUB simulation is started with the function TopoSUB_preprocessor. After preparing GEOtop input files (raster maps, meteorological stations, horizon files, ...), the geotop.inpts file which is configuring the point simulations and the TopoSUB configuration files (locations.txt, setup.txt), this function is carrying out landscape clustering and starting GEOtop point simulations, preferable on a HPC server. Postprocessing of the GEOtop output is perfomed by the functions

* TopoSUB_read: reading GEOtop output, specified by keywords in geotop.inpts
* TopoSUB_spread: speading the output data table of the function above for a specific variable
* TopoSUB_remap: maping of the spreaded output for different time/climatic periods and/or differences of time/climatic periods

An example simulation for the Montatschinig/Montecini region (LTER Matsch/Mazia) can be found [here](). Use this example as a template to create an TopoSUB-GEOtop simulation folder. See also the files locations.txt and setup.txt for parmeters defining your simulation properties, e.g. input/output folders, number of cluster centroids, switches, definitions for running the simulation on a HPC cluster, ...

Moreover, the package includes an easy to use shiny app. Run in the simulation folder it visualises the finally produced maps (still Beta-version, see folder _shiny_).
