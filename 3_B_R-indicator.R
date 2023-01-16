
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
## > Step 1: : Prepare an Auxiliary data file ----
#H--------------------------------------------
## Auxiliary file for R-indicator 
##  Use Weightedsample_freq_table

# Run if the earlier code has changed.
# source("2_Prep_Wtsample_Freq_table.R")

#H--------------------------------------------
##>> Prep Auxiliary file 
#H--------------------------------------------

load(file = "Output/04-RData/Weightedsample_freq_table.RData")
load(file="Output/04-RData/var.RData")

head(Weightedsample_freq_table)

var

# create popsize (to be used for auxiliary file)
# Popsize: oneway total of the first variable declared.
# Please do not use manuallay (e.g. popsize <- 1163650).
# The programme may not run as intended.
popsize <- Weightedsample_freq_table %>%
          filter(oneway == 1 & by1 == var[1]) %>%
          summarise(popsize = sum(wtsample_n))
popsize <- as.numeric(popsize)
popsize
class(popsize)

freq_table <- data.frame(Weightedsample_freq_table, popsize)

head(freq_table)
glimpse(freq_table)
# wtsample 1163650  # Census obs = 1163659

# Compute meanpop using freq_table,
# Create auxiliary data
  fn_meanpop_auxiliary()

# Output
print(auxiliary)
# View(auxiliary)

# (Optional) Save intermediate RData
  save(auxiliary, file = "Output/04-RData/auxiliary.RData")

  write_xlsx(auxiliary,
    "Output/03-ExcelOutput/auxiliary.xlsx")

  # Launches the Excel file (Windows PC)
  # shell.exec("Output\\03-ExcelOutput\\auxiliary.xlsx")

# prepare the data to build design matrix
# using dummy variables
# Keep necessary dummy variables only.
# Remove rows if lastcat
nrow(auxiliary)

#H---------------------------------------
## >> Calculate meanpop 
#H---------------------------------------

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

# table(dummychk$by1, dummychk$by2) # alternatively

# Keep meanpop only, # Recall meanpop = count / popsize
keep <- c("meanpop")
col_auxiliary <- data.frame(col_auxiliary[keep])

# Print the column vector, meanpop() before transposing the data.
print(col_auxiliary)

    #  meanpop
    #  1       1
    #  2  0.0973
    #  3   0.128
    #  ...

#H---------------------------------------
## > Step 2: Reshape and save wtsample distributions ----
## as row vectors 
#H---------------------------------------

# Transpose to arrange in the row vector format.
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
## Skip as appropriate
#H----------------------------------

# Declare variables to be used
# Customise as needed.
# var <- c("geog1", "sex", "agecode1",
#          "eth_code5", "econg")
# End of custom variables.

#H-----------------------------------
##> Step 4: Define macro variables ----
#H-----------------------------------

var
variablenum <- length(var) # No need to customise
maxvar <- length(var)      # No need to customise

variablenum ; maxvar

#H-----------------------------------
##> Step 5: Compute R-indicators ----
#H-----------------------------------

aa  <- read_csv("public_release_admin.csv")
head(aa)
ls()

    nrow(aa) # 1033664

    df      <- NULL
    between <- NULL
    partial <- NULL
    partialtemp <-  NULL

# Overall R-indicators
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
     R_indicators
      prop_mix_based_R_indicator
      prop_pop_based_R_indicator

# Partial R-indicators
    prop <- prop_mix
    # prop <- prop_pop
    R_indicator <- prop_mix_based_R_indicator # mixed
    R_indicator
    fn_r_indicator_partialtemp()
    fn_r_indicator_domain_order_partial()

# H-----------------------------------
## > Step 6a: Inspect ----
# H-----------------------------------

View(partial)
# partial[1:17, c(1:2, 4, 8:10)]

#H-----------------------------------
## > Step 6b: Open in Excel
#H-----------------------------------

xlsxfile <- "R_indicator.xlsx"
  fn_xlsx_path_file()
  write_xlsx(partial, xlsx_path_file)
  fn_xlsx_open()

#H-------------------------------------
## > Step 7a: Tidy, Visualisation prep ----
#H-------------------------------------

levels(partial$fct_domain)

# Rescale
partial <- partial %>%
  rename(p2Zk  = p2Zk_ , cvp2k = cvp2k_)

partial <- partial %>%
  mutate(R_indicator = p2Zk) %>%
  mutate(Rindicator_x1000 = R_indicator * 1000) %>%
  relocate(R_indicator_summary, Rindicator_x1000, p2Zk,
    .after = fct_domain)

#H-----------------------------------
## > Step 7b: Scatterplot ----
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
