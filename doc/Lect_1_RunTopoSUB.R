#==============================================================================
# run TopoSUB
#==============================================================================

# install/source TopoSUB library

if (any(installed.packages()[,1]=="TopoSUB")) {
  library(TopoSUB)
} else {
  if(any(installed.packages()[,1]=="devtools")) {
    library(devtools)
  } else {
    install.packages("devtools")
    library(devtools)
  }
  install_github("JBrenn/TopoSUB")
  library(TopoSUB)
}

#  source code of TopoSUB (download from TopoSUB GitHub repo)
source("src/toposub_src.r")
source("src/toposub_src_BrJ.r")

# setup file
setupfile = "setup.txt"

# location file
locationfile = "locations.txt"

# topoSUB preprocessor
TopoSUB_preprocessor(location.file = locationfile, setup.file = setupfile,
                     PredNamesList=list(topo=c("dem", "slp", "asp", "svf"),
                                        clas=c("landcover", "soil")),
                     uniform_class = c(landcover=NA, soil=NA))
