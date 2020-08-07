##### Load packages ####

# List of packages used:

# - shinydashboard
# - shiny
# - shinyBS
# - ggplot2
# - methods
# - hexbin
# - DT
# - dplyr
# - d3heatmap
# - RColorBrewer
# - BBmisc
# - viridis
# - hrbrthemes
# - plotly
# - tidyr
# - dqshiny
# - haven
# - survey
# - stringr
# - interactions
# - gplots
# - lazyeval
# - tidyverse
# - treemap
# - sunburstR
# - d3r
# - rintrojs
# - jsonlite
# - sp
# - shinyjqui
# - geofacet
# - minimap
# - geofacet
# - gtsummary


dependencies <- c("shinydashboard" , "shiny" , "shinyBS" , "ggplot2" , "methods" , "hexbin" , "DT" , "dplyr" , "d3heatmap" , "RColorBrewer" , "BBmisc" , "viridis" , "hrbrthemes" , "plotly" , "tidyr" , "dqshiny" , "haven" , "survey" , "stringr" , "interactions" , "gplots" , "lazyeval" , "tidyverse" , 
                  "treemap" , "sunburstR" , "d3r" , "rintrojs" , "jsonlite" , "sp" , "shinyjqui" , "geofacet" , "minimap" , "formattable" , "gtsummary","data.table","kableExtra","DataCombine","knitr")

# Check and install packages not yet available
install.dependencies <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if (length(install.dependencies) > 0) {
  install.packages(install.dependencies)
}

# Load all packages
library(shinydashboard)
library(shiny)
library(shinyBS)
library(ggplot2)
library(methods)
library(hexbin)
library(DT)
library(dplyr)
library(d3heatmap)
library(RColorBrewer)
library(BBmisc)
library(viridis)
library(hrbrthemes)
library(plotly)
library(tidyr)
library(dqshiny)
library(haven)
library(survey)
library(stringr)
library(interactions)
library(gplots)
library(lazyeval)
library(tidyverse)
library(treemap)
library(sunburstR)
library(d3r)
library(rintrojs)
library(jsonlite)
library(sp)
library(shinyjqui)
library(geofacet)
library(minimap)
library(gtsummary)
library(data.table)
library(formattable)
library(kableExtra)
library(DataCombine)
library(knitr)

