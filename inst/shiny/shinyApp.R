library(shiny)

library(leaflet)
library(raster)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)

# get path to source file (only workin if sourcing file) 
# solution from hadley (http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script)
# only working if source file
# frame_files <- lapply(sys.frames(), function(x) x$ofile)
# frame_files <- Filter(Negate(is.null), frame_files)
# PATH <- dirname(frame_files[[length(frame_files)]])

runApp(getwd())