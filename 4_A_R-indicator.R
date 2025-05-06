
# Run Custom_Path.R if necessary.
rm(list = ls())
getwd()

source("Functions/1_Functions.R")

# Disable scientific notation.
options(scipen = 999)

    library("dplyr")     # data manipulation
    library("ggplot2")   # visualisation
    library("readr")     # read large csv file
    library("tidyr")     # data manipulation
    library("stringr")   # data manipulation
    library("readxl")    # read Excel file
    library("writexl")   # export to Excel
    library("janitor")   # cross-tabulation

#H-----------------------------------
##> Step 1: Prep ----
#H-----------------------------------

#H--------------------------------------
## > Prepare an Auxiliary data file ----
#H--------------------------------------
##  Use Weightedsample_freq_table

# Prep_auxiliary
#  |
#  |- 1A. Using an existing data
#  |  |- (pop + admin) frequency table
#  |  |  (for distance metrics)
#  |  |- pop_respmean (for R indicators)
#  |  |- (pop) bivariate counts (for R indicators)
#  |
#  |- 1B. Creating a weighted sample data
#  |  |- (pop + admin) frequency table
#  |  |- pop_respmean
#  |  |- (pop) bivariate counts

# Run one of the previous files as appropriate
# source("2_A_Prep_auxiliary.R")
source("2_B_Prep_auxiliary_CreateRandomSample.R")

# Declare popsize
popsize <- popcov[1,1]; popsize

#H-----------------------------------
##> Step 2: Read in the admin data ----
#H-----------------------------------

# User admin data file
aa  <- read_csv("public_release_admin.csv")

# rename, keep variables of interest and sort
aa <- aa %>%
 dplyr::select(-person_id) %>%
 arrange(geog1, sex, agecode1, eth_code5, econg)

head(aa)

#H-----------------------------------
##> Step 3: Compute R-indicators ----
#H-----------------------------------

# fn_RUN_R_indicator()

# Sub-routine of fn_R_indicator()
    fn_design_matrix()
    fn_des_pop_respmean()
    fn_gh()
    fn_R_indicator()
    fn_rindicatorall()
    fn_partial()

# Inspection
    # View(design_matrix[1:6, ])
    head(des_pop_respmean, n = 3)
    head(gh, n = 3)
    View(gh[1:3, ])
    # View(partial)
 partial[1:17, c(1:4, 6:8)]

#H-----------------------------------
## > Step 4: Save in Excel
#H-----------------------------------

# xlsxfile <- "R_indicator.xlsx"
xlsxfile <- "weightedsample_R_indicator.xlsx"
  fn_xlsx_path_file()
  write_xlsx(partial, xlsx_path_file)
  # fn_xlsx_open()

#H-------------------------------------
## > Step 5: Visualisation  ----
#H-------------------------------------

#H-----------------------------------
## >> Scatterplot 1 (Variable level) ----
#H-----------------------------------

# View(partial)
# partial[1:17, c(1:3, 5, 8:10)]

theme_set(theme_bw())

# Inspect level, level = 1 is var-level.
# level = 3 is cat-level.

p1 <- partial %>%
    filter(level == 1) %>%
    ggplot(aes(x = fct_domain, y = R_indicator_summary)) +
    geom_point(stroke = 1.2, colour = "darkblue") +
    labs( x = "Domain",
      y = "Variable-level Partial R-indicator") +
    geom_hline(yintercept = 0,
      linewidth = 1, color="lightblue3") +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
  )
plot(p1)

 # figfile <- "R_indicator_scatter_var.png"
 figfile <- "weightedsample_R_indicator_scatter_var.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

#H-----------------------------------
## >> Scatterplot 2 (Category level) ----
#H-----------------------------------

## cat-level (level = 3 is cat-level)
p2 <- partial %>%
    filter(level == 3) %>%
    ggplot (aes(x = fct_domain, y = R_indicator_summary )) +
    geom_point(stroke = 1.2, colour = "darkblue") +
    labs( x = "",
      y = "Category-level Partial R-indicator")  +
    geom_hline(yintercept = 0,
      linewidth = 1, color="lightblue3") +
   theme(
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
    axis.text.x=element_text(angle = 45, #size=10
    hjust=1, #right-justified, then turn
    vjust = 0.5) #middle of tickmark
  )
plot(p2)


 # figfile <- "R_indicator_scatter_cat.png"
 figfile <- "weightedsample_R_indicator_scatter_cat.png"
  fn_fig_path_file()
  ggsave(fig_path_file)


# More theme options:
  # panel.grid.major = element_blank(),
  # panel.grid.minor = element_blank(),

### End ###


