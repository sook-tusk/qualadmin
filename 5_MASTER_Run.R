

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
##> 2. Auxiliary files
#H------------------------------
# Choose one
# source("2_A_Prep_auxiliary.R")
source("2_B_Prep_auxiliary_CreateRandomSample.R")


#H------------------------------
##> 3. Quality Measures
#H------------------------------

source("3_Distance_Metrics.R")

# Ignorable warnings. Just check.
warnings()

# Allow a minute to run.
# Computationally intensive procedures.
source("4_A_R-indicator.R")

source("4_B_Table-Based_R-indicator.R")

### End ###
