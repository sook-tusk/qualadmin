
# Run Custom_Path.R if necessary.
rm(list = ls())
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
## > Step 1: Load auxiliary file ----
#H--------------------------------------------
# See 2_Prep_Wtsample_Freq_Table.R
# PREP PART 2. Auxiliary file for R-indicator

# We load auxiliary.
load(file = "Output/04-RData/auxiliary.RData")

# Run if the earlier code has changed.
# source("2_Prep_Wtsample_Freq_table.R")

#H--------------------------------------------
## >  Step 1:  Prep Auxiliary file ----
#H--------------------------------------------

# Obtain popsize from Population (Census)

## Obtain popsize
head(auxiliary)
# View(auxiliary)

popsize <- auxiliary %>%
           filter( meanpop == 1) %>%
           summarise(count)
(popsize <- popsize[[1]] )
# wtsample 1163650

# Keep necessary dummy variables only.
nrow(auxiliary)

auxiliary_temp <- auxiliary
col_auxiliary <- auxiliary_temp %>%
             group_by(by1) %>%
             mutate(
              lastcat_ = ifelse(by2 == max(by2),
                    1, 0),
              lastcat = ifelse(seq == 1, 0, lastcat_)
            ) %>%
             filter(lastcat == 0) %>%
             dplyr::select(-c(lastcat_, lastcat)) %>%
             ungroup()
dim(col_auxiliary) ; head(col_auxiliary)
nrow(col_auxiliary)

# Print meanpop
print(col_auxiliary[1:nrow(col_auxiliary), "meanpop"],
  n = nrow(col_auxiliary))
    #  meanpop
    #    wtsample
    #  1       1
    #  2  0.0973
    #  3   0.128
    #  ...

# Rename col_auxiliary 
auxiliary <- col_auxiliary

#H---------------------------------------
## > Step 2: wtsample distributions as row vectors ----
#H---------------------------------------
# Recall meanpop = count / popsize

# Identify the numcat
(numcat <- nrow(auxiliary))

# Transpose to arrange in row vector format.
auxiliary <- t(auxiliary)
auxiliary <- as.data.frame(auxiliary)
is.data.frame(auxiliary)

auxiliary$reshaped <- row.names(auxiliary)
row.names(auxiliary) <- 1:nrow(auxiliary)

names(auxiliary)

# Tidy & generate merge id, ttt.
auxiliary <- auxiliary %>%
             filter(reshaped == "meanpop") %>%
             mutate( ttt = 0 ) %>%
             dplyr::select( -reshaped)

# Rename variables "respmean",
colnames(auxiliary)  <- c(paste0("popmean", 1:numcat), "ttt")
names(auxiliary)
str(auxiliary)

View(auxiliary)

# SAVE
popmean_row_vector <- auxiliary

ls()

#H----------------------------------
##> Step 3a: Declare variables ----
#H----------------------------------

# Declare variables to be used
# Customise as needed.
var <- c("geog1a", "sex", "agecode1",
          "eth_code5", "econg")
(variablenum <- length(var))
(maxvar <- length(var))

# simsize
# (resppop_ <- fn_sim_obs())
#   resppop_[1] ; resppop_[20] ;

#H-----------------------------------
##> Step 3b: Compute R-indicators ----
#H-----------------------------------

aa  <- read_csv("public_release_admin.csv")

ls()

    nrow(aa) # 1033664

    df      <- NULL
    between <- NULL
    partial <- NULL
    partialtemp <-  NULL
   fn_overall_r_indicator_1()
   fn_overall_r_indicator_2()
   fn_overall_r_indicator_3()
   fn_overall_r_indicator_4()
   R_indicators # e.g. 0.6705086 0.6759111

    # Mixed
    prop <- prop_mix
    (R_indicator <- R_indicators[1]) # mixed
    fn_r_indicator_partialtemp()
    fn_r_indicator_domain_order_partial()

    # View(partial)

#H-------------------------------------
## > Step 4: Tidy, Visualisation prep ----
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
## > Step 4: Open in Excel
#H-----------------------------------

xlsxfile <- "R_indicator.xlsx"
  fn_xlsx_path_file()
  write_xlsx(partial, xlsx_path_file)
  fn_xlsx_open()

# H-----------------------------------
## > Step 4: Inspect
# H-----------------------------------

View(partial)
partial[1:17, c(1:2, 4, 8:10)] 

#H-----------------------------------
## > Step 5: Scatterplot
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
