
# Run Custom_Path.R if necessary.
rm(list = ls())
getwd()

source("Functions/1_Functions.R")


##H ------------------------------
##> fn_meanpop_auxiliary ----
##H ------------------------------

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

#H--------------------------------------
## > Step 1: Prepare an Auxiliary data file ----
#H--------------------------------------
## Auxiliary file for R-indicator
##  Use Weightedsample_freq_table

# Run if the earlier code has changed.
# source("2_Prep_Wtsample_Freq_table.R")

#H-----------------------------------
### >> Obtain population size
#H-----------------------------------

load(file = "Output/04-RData/Weightedsample_freq_table.RData")
# View(Weightedsample_freq_table)

head(Weightedsample_freq_table)

# create popsize (to be used for auxiliary file)
# Popsize: oneway total of the first variable.
# Please do not use manuallay (e.g. popsize <- 1163650).
# The programme may not run as intended.
popsize <- Weightedsample_freq_table %>%
           filter(twdigits < 200) %>%
          summarise(popsize = sum(wtsample_n))
popsize <- as.numeric(popsize)
popsize ; class(popsize)

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

#H-----------------------------------
## >> Calculate meanpop
#H-----------------------------------
# prepare the data to build design matrix
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
# Remove rows if lastcat
nrow(col_auxiliary)
numcat <- nrow(col_auxiliary) ; numcat

# Inspect dummy variables
  dummychk <- col_auxiliary
  dummychk %>% tabyl(by1, by2)

# Keep meanpop only,
# Recall meanpop = count / popsize
keep <- c("meanpop")
col_auxiliary <- data.frame(col_auxiliary[keep])

# Print the column vector, meanpop() before transposing the data.
print(col_auxiliary)

#H---------------------------------------
## >> Reshape and save wtsample distributions ----
## as row vectors
#H---------------------------------------

# Transpose to arrange in the row vector format.
  temp <- t(col_auxiliary)
  temp <- as.data.frame(temp)
  row.names(temp) <- 1:nrow(temp)

# generate merge id, ttt.
  temp$ttt  <- 0

  # Rename variables "popmean1- popmean26"
  names(temp) <- c(paste0("popmean", 1:numcat),
                          "ttt")

# Save
  popmean_row_vector <- temp

# Check
  # View(popmean_row_vector)

  names(popmean_row_vector)
  popmean_row_vector[, 1:5]

#H-----------------------------------
##> Step 2: Prepare the admin data ----
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

fn_RUN_table_based_R_indicator()

# Sub-routine of fn_RUN_table_based_R_indicators()
     fn_t_design_matrix()
     fn_freq()
     fn_t_respmean()
     fn_des_pop_respmean()
     fn_gh ()
     fn_t_R_indicator()
     fn_t_rindicatorall()
     fn_partial()

# Inspection
 partial[1:17, c(1:4, 6:8)]
      # View(partial)

#H-----------------------------------
## > Step 4: Save in Excel
#H-----------------------------------

xlsxfile <- "Using_TableBased_R_indicator.xlsx"
  fn_xlsx_path_file()
  write_xlsx(partial, xlsx_path_file)
  # fn_xlsx_open()

#H-------------------------------------
## > Step 5: Visualisation  ----
#H-------------------------------------
# Prep
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
## >> Scatterplot ----
#H-----------------------------------

# View(partial)

theme_set(theme_bw())

# More theme options:
  # panel.grid.major = element_blank(),
  # panel.grid.minor = element_blank(),

# Overall R-indicator only
# Variable level

# Inspect level, level = 1 is var-level.
# level = 3 is cat-level.

# partial[1:17, c(1:3, 5, 8:10)]

p1 <- partial %>%
    filter(level == 1) %>%
    ggplot(aes(x = fct_domain, y = R_indicator)) +
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

 figfile <- "t_R_indicator_scatter_Var_level.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

# cat-level (level = 3 is cat-level)
p2 <- partial %>%
    filter(level == 3) %>%
    ggplot (aes(x = fct_domain, y = R_indicator )) +
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

 figfile <- "t_R_indicator_scatter_Category-level.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

### End ###

