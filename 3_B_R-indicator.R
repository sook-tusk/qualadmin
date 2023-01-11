
# Run Custom_Path.R if necessary.
# rm(list = ls())
getwd()

# Run the code file with functions.
source("Functions/1_Functions.R")

source("Functions/2_Functions_R-indicators.R")

# Define output file folders, path
fn_output_folder_path()

# Disable scientific notation.
options(scipen = 999)

library("tidyverse")
library("ggplot2")
library("readxl")
library("writexl")
library("janitor")

#H--------------------------------------------
## > Step 1:  Benchmark mean population ready ----
#H--------------------------------------------
# See 2_Prep_Wtsample_Freq_Table.R
# PREP PART 2. Auxiliary file for R-indicator

# Run if the earlier code has changed.
# source("2_Prep_Wtsample_Freq_table.R")

#H--------------------------------------------
## >  Prep Auxiliary file ----
#H--------------------------------------------

# Load auxiliary file
load(file = "Output/04-RData/auxiliary.RData")
auxiliary[1:10, ]
# View(auxiliary)

# Obtain popsize from population (Census or benchmark data)
# To be used in programming later
popsize <- auxiliary %>%
           filter( seq == 1) %>%
           summarise(count)

popsize <- popsize[[1]]
popsize
# wtsample 1163650

# Keep necessary dummy variables only.
nrow(auxiliary)

col_auxiliary <- auxiliary %>%
             group_by(by1) %>%
             mutate(
              lastcat_ = ifelse(by2 == max(by2), 1, 0),
              lastcat = ifelse(seq == 1, 0, lastcat_)
            ) %>%
             filter(lastcat == 0) %>%
             dplyr::select(-c(lastcat_, lastcat)) %>%
             ungroup()
dim(col_auxiliary)
col_auxiliary[1:10, ]

# Identify the numcat & create a macro
nrow(col_auxiliary)
numcat <- nrow(col_auxiliary)
numcat

# Inspect dummy variables
  dummychk <- col_auxiliary
  dummychk %>% tabyl(by1, by2)

# table(dummychk$by1, dummychk$by2)

# Keep meanpop only
keep <- c("meanpop")
col_auxiliary <- col_auxiliary[keep]

# Print meanpop before transposing the data.
# To print all rows
print(col_auxiliary[1:nrow(col_auxiliary), "meanpop"],
  n = nrow(col_auxiliary))

    #  meanpop
    #    wtsample
    #  1       1
    #  2  0.0973
    #  3   0.128
    #  ...

#H---------------------------------------
## > Step 2: Reshape and save wtsample distributions as row vectors ----
#H---------------------------------------
# Recall meanpop = count / popsize

# Transpose to arrange in row vector format.
  temp <- t(col_auxiliary)
  temp <- as.data.frame(temp)
  row.names(temp) <- 1:nrow(temp)

# generate merge id, ttt.
  temp$ttt  <- 0

  # View(temp)

  # Rename variables "popmean1- popmean26"
  names(temp) <- c(paste0("popmean", 1:numcat),
                          "ttt")

# Check
  popmean_temp <- temp
  popmean_temp[, 1:5]
  names(popmean_temp)

# Save
  popmean_row_vector <- temp

  View(popmean_row_vector)

#H----------------------------------
##> Step 3: Declare variables in admin data ----
#H----------------------------------

# Declare variables to be used
# Customise as needed.
var <- c("geog1a", "sex", "agecode1",
         "eth_code5", "econg")
# End of custom variables.

#H-----------------------------------
##> Step 4: Define macro variables ----
#H-----------------------------------

variablenum <- length(var) # No need to customise
maxvar <- length(var)      # No need to customise

variablenum ; maxvar

# simsize
# (resppop_ <- fn_sim_obs())
#   resppop_[1] ; resppop_[20] ;

#H-----------------------------------
##> Step 5: Compute R-indicators ----
#H-----------------------------------

aa  <- read_csv("public_release_admin.csv")

ls()

    nrow(aa) # 1033664

    df      <- NULL
    between <- NULL
    partial <- NULL
    partialtemp <-  NULL
   fn_r_indicator_1_vvv()
     from <- ncol(vvv)-4
     vvv[1:10, from: ncol(vvv)]

   fn_r_indicator_2_pop_respmean()
     from <- ncol(pop_respmean)-4
     pop_respmean[, from: ncol(pop_respmean)]

   fn_r_indicator_3_des_pop_respmean()
     names(des_pop_respmean)
     from <- ncol(des_pop_respmean)-4
     des_pop_respmean[1:6, from: ncol(des_pop_respmean)]

   fn_r_indicator_4_gh()
     # names(gh)
     glimpse(gh)
     dim(gh)

   fn_R_indicators()
   R_indicators # e.g. 0.496 0.515

# Partial R-indicators
    prop <- prop_mix
    (R_indicator <- R_indicators[1]) # mixed
    fn_r_indicator_partialtemp()
    fn_r_indicator_domain_order_partial()

    # View(partial)

#H-------------------------------------
## > Step 5b: Tidy, Visualisation prep ----
#H-------------------------------------

levels(partial$fct_domain)

# Rescale
partial <- partial %>%
  rename(p2Zk  = p2Zk_ , cvp2k = cvp2k_)

partial <- partial %>%
  mutate(R_indicator = p2Zk) %>%
  mutate(Rindicator_x1000 = R_indicator * 1000) %>%
  relocate(R_indicator, Rindicator_x1000, p2Zk,
    .after = fct_domain)

#H-----------------------------------
## > Step 6: Open in Excel
#H-----------------------------------

xlsxfile <- "R_indicator.xlsx"
  fn_xlsx_path_file()
  write_xlsx(partial, xlsx_path_file)
  fn_xlsx_open()

# H-----------------------------------
## > Step 6b: Inspect
# H-----------------------------------

View(partial)
partial[1:17, c(1:2, 4, 8:10)]

#H-----------------------------------
## > Step 7: Scatterplot
#H-----------------------------------

View(partial)

theme_set(theme_bw())

# Overall R-indicator only
# Variable level

# More theme options:
  # panel.grid.major = element_blank(),
  # panel.grid.minor = element_blank(),

p1 <- partial %>%
    filter(seq %in% (4:8)) %>%
    ggplot(aes(x = fct_domain, y = R_indicator )) +
    geom_point(stroke = 1.2, colour = "darkblue") +
    labs( x = "Domain",
      y = "Variable-level Partial R-indicator") +
    geom_hline(yintercept = 0,
      size = 1, color="lightblue3") +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
  )
plot(p1)

 figfile <- "R_indicator_scatter_Var_level.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

# cat-level
p2 <- partial %>%
    filter(seq %in% (10:50)) %>%
    ggplot (aes(x = fct_domain, y = R_indicator )) +
    geom_point(stroke = 1.2, colour = "darkblue") +
    labs( x = "",
      y = "Category-level Partial R-indicator")  +
    geom_hline(yintercept = 0,
      size = 1, color="lightblue3") +
   theme(
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
    axis.text.x=element_text(angle = 45, #size=10
    hjust=1, #right-justified, then turn
    vjust = 0.5) #middle of tickmark
  )
plot(p2)

 figfile <- "R_indicator_scatter_Category-level.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

### End ###
