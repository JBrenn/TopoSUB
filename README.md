
=======
# TopoSUB
R package for the TopoSUB Tool
* preprocessing (landscape clustering, running LSM in parallel)
* postprocessing (read output, mapping)
* data analysis (specific variable postprocessing, e.g. crought indices)

TopoSUB is a R project for sampling the most important  aspects of land surface heterogeneity through a lumped scheme, allowing for the application of numerical land surface models (LSMs), like e.g. [GEOtop](https://github.com/skyglobe/geotop), over large areas in mountain regions or other heterogeneous environments (Fiddes et al. 2012).

#### Reference
Fiddes, J., & Gruber, S. (2012). TopoSUB: A tool for efficient large area numerical modelling in complex topography at sub-grid scales. Geoscientific Model Development, 5(5), 1245–1257. [DOI](http://doi.org/10.5194/gmd-5-1245-2012)

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
