
##=============================================
#   Run this code to ...
#   Run distance metrics, R-indicators
#
#   Preliminary code files will be read automatically.
#   All tables, graphics should pop up automatically.
##=============================================

rm(list = ls())
getwd()

#H------------------------------
##> 1. Load functions
#H------------------------------

source("Functions/1_Functions.R")

#H------------------------------
##> 2. Quality Measures
#H------------------------------

source("2_Prep_Wtsample_Freq_Table.R")

source("3_A_Distance_Metrics.R")

# Ignorable warnings. Just check.
warnings()

# Allow a minute to run.
# Computationally intensive procedures.
source("3_B_R-indicator.R")

### End ###