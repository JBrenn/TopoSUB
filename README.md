
=======
# TopoSUB
R package for landscape clustering and land-surface modelling
* preprocessing (landscape clustering, running LSM in parallel)
* postprocessing (read output, mapping)
* data analysis (specific variable postprocessing, e.g. drought indices)


TopoSUB is a R project for sampling the most important  aspects of land surface heterogeneity through a lumped scheme, allowing for the application of numerical land surface models (LSMs), like e.g. [GEOtop](https://github.com/geotopmodel), over large areas in mountain regions or other heterogeneous environments (Fiddes et al. 2012). The project was initiated by [Joel Fiddes](http://www.wsl.ch/info/mitarbeitende/suche/index_EN/?search_name=Joel%20Caduff-Fiddes), the source code is available via the [toposub GitHub repository](https://github.com/joelfiddes/toposub).

=======

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

A TopoSUB simulation is started with the function TopoSUB_preprocessor. After preparing GEOtop input files (raster maps, meteorological input stations, horizon files, ...), the GEOtop configuration file (geotop.inpts) and the TopoSUB configuration files (locations.txt, setup.txt), use this function to carry out landscape clustering and GEOtop point simulations, for computational intensive simulations preferable parallelized on a HPC server. See the function documentation for a short description.

```R
?TopoSUB_preprocessor
```

Postprocessing of the GEOtop output is perfomed by the functions

* TopoSUB_read: reading GEOtop output, specified by keywords in geotop.inpts
<!-- * TopoSUB_spread: speading the output data table of the function above for a specific variable -->
* TopoSUB_remap: maping of the output data table of the function above for different time/climatic periods and/or differences of time/climatic periods

An example simulation for the Montatschinig/Montecini region (LTER Matsch/Mazia) can be found [here](https://cloud.scientificnet.org/index.php/s/Y6UwKt79pFZp2uR). Use this example as a template to create an TopoSUB-GEOtop simulation folder. See also the TopoSUB configuration files (_locations.txt_ and _setup.txt_) and define parmeters, e.g. input/output folders, number of cluster centroids, specific switches, definitions for running the simulation on a HPC cluster, ... For running the example use the R script main_pre.R after changing the root folder and GEOtop LSM exe Path in locations.txt   

Moreover, the package includes an easy to use shiny app for visualisation. Run in the simulation folder it visualises the finally produced maps (still Beta-version, see folder _shiny_).

The TopoSUB package includes vignettes on the topics

1. running a TopoSUB simulation
2. postprocess a TopoSUB simulation
3. visualisation of a TopoSUB output

You can install the package with the option of building the vignettes and list/open them in an HTML browser:

```R
library(devtools)
install_github("JBrenn/TopoSUB", build_vignettes = TRUE)

browseVignettes("TopoSUB")
```


