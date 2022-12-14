
## =================================================
### Run this code to ...
#   Run distance metrics, R-indicators
#   All tables, graphics should pop up automatically.
##=================================================

rm(list = ls())

#H------------------------------
##> 1. Load functions
#H------------------------------

source("Functions/1_Functions.R")

source("Functions/2_Functions_R-indicators.R")

#H------------------------------
##> 2. Quality Measures
#H------------------------------

source("3_A_Distance_Metrics.R")

# Ignorable warnings. Just check.
warnings()

# Allow a minute to run.
# Computationally intensive procedures.
source("3_B_R-indicator.R")

### End ###