
#-----------------------------------
# Check library path
#-----------------------------------

.libPaths()

#-----------------------------
# Install packages (Run once)
#-----------------------------

# For R version > 4.0
install.packages("tidyverse")

# For R version, 3.5.1, "tidyverse" may not
# install as intended. Please install
# several packages below in stages, instead.
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("stringr")

install.packages("rlist")
install.packages("fastDummies")
install.packages("janitor")

install.packages("readxl")
install.packages("writexl")
install.packages("languageserver")

#-----------------------------------
# Backward compatibility
#-----------------------------------

# Using the latest R version over 4.0 is recommended.

# Code tested with the R version 3.5.1.
# Rtools version 35(3.5.0.4) is installed first.
# Then, above packages were installed.
# With R version 3.5.1, versions of installed packages:
    # dplyr   1.1.1   R (≥ 3.5)
    # ggplot2 3.4.1   R (≥ 3.3)
    # readr   1.3.1   R (≥ 3.5)
    # tidyr   1.3.0   R (≥ 3.4)
    # stringr 1.5.0   R (≥ 3.3)
    # rlist   0.4.6.2 R (≥ 2.15)
    # fastDummies 1.6.3 (R ≥ 2.10)
    # janitor 2.2.0   (R ≥ 3.1.2)

    # readxl  1.3.1   R (≥ 3.5)
    # writexl 1.2     Unsure
    # (released on 11/27/2019)
  # https://cran.r-project.org/src/contrib/Archive/writexl/

### End ###
