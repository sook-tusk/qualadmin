
# Run Custom_Path.R if necessary.
    rm(list = ls())    # Remove objects
    getwd()

    # Run the code file with functions.
    source("Functions/1_Functions.R")

    # Disable scientific notation.
    options(scipen = 999)

    library("dplyr")     # data manipulation
    library("ggplot2")   # visualisation
    library("readr")     # read large csv file
    library("readxl")    # read Excel file
    library("writexl")   # export to Excel
    library("janitor")   # cross-tabulation

# If the Census data or a weighted sample data
# are available, users consult Part 1A.
# If these data are unavailable, see
# Part 1B, Generate a weighted sample data.

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


#===============================================
# Part 1A, Use an existing weighted sample data

#H========================================
## > Step 1. Read in the data ----
#========================================

# Read in the custom data
# df  <- read_csv("custom_wtsample.csv")

# For demonstration
load("pop_u_short_public_release_5vars.RData")
df  <- pop_u_short_public_release_5vars

# rename, keep variables of interest and sort
df <- df %>%
 arrange(geog1, sex, agecode1, eth_code5, econg)
    # head(df); dim(df)

#========================================
## > Step 2. Obtain the frequency table ----
#========================================

#H---------------------------------
## >> a. Benchmark data ----
#H---------------------------------

fn_freq_table(df)

pop_table <- freq_table

  # Examine
  head(pop_table)
  # View(pop_table)


#H---------------------------------
## >> b. Admin data ----
#H---------------------------------

aa  <- read_csv("public_release_admin.csv")

# rename, keep variables of interest and sort
aa <- aa %>%
 dplyr::select(-person_id) %>%
 arrange(geog1, sex, agecode1, eth_code5, econg)

#H------------------------------------------
## >> Initial look on admin data
#H------------------------------------------

    head(aa)
    # View(aa[1:100, ])

# Check, first and second variables
    var  <- names(aa) ; var
    aa %>% tabyl(var[1]) %>% adorn_totals()
    aa %>% tabyl(var[2]) %>% adorn_totals()

    # cross-tabulation
    addmargins(table(aa[, c(var[1], var[2])]))

#H------------------------------------------
##>> Obtain admin freq tables ----
#H------------------------------------------

fn_freq_table(aa)
  # View(freq_table)

admin_table <- freq_table %>%
    rename(admin_n = n, admin_p = p)

## > Inspect (Elaborate in manual)
  head(admin_table)
  # View(admin_table)

#H----------------------------------------------
## >  c. Merge pop + admin freq table ----
#H----------------------------------------------

tt <- full_join(pop_table, admin_table)
pop_admin_freq_table <- tt %>%
    dplyr::select(seq, des_seq, oneway_by,
      notlast, n, p, everything())

  dim(pop_admin_freq_table)
# print(pop_admin_freq_table, n = 10)
#   print(pop_admin_freq_table[c(1:10),
#    c(3,5:8,10,11,12,14:15)])
# print(pop_admin_freq_table[c(32:43),
#   c(3,5:8,10,11,12,14:15)])
#   View(pop_admin_freq_table)

# Examine
pop_admin_freq_table %>% filter(by2 == 2) %>%
 summarise(twoway_sum_p = sum(p) )

sum(pop_admin_freq_table[2:7, "n"])
sum(pop_admin_freq_table[2:7, "p"])

##H ---------------------------------
## >> Export outputs  ----
##H ---------------------------------

  xlsxfile <- "freq_table.xlsx"
  fn_xlsx_path_file()
  write_xlsx(pop_admin_freq_table, xlsx_path_file)
  fn_xlsx_open()

##H --------------------------------
### >> Save RData ----
##H --------------------------------

write_rds(pop_admin_freq_table,
  file="Output/04-RData/freq_table.rds")

#========================================
##> Step 3. Obtain pop_respmean ----
# Auxiliary file for R-indicator
# Obtain the population size and
# population proportions, and
# weighted admin counts and respmean
#========================================
# using pop_admin_freq_table,
    head(pop_admin_freq_table);

    fn_pop_respmean(pop_admin_freq_table)
    print(pop_respmean)

# Check intermediate outputs
    # View(pop_admin_mean_c)
    # View(mean_c)
    # View(freq)

# for user manual
 print(pop_admin_mean_c[, c(2:6,9,11,13:15)])

##H ---------------------------------
## >> Export intermediate outputs  ----
##H ---------------------------------
  xlsxfile <- "pop_admin_mean_c.xlsx"
  fn_xlsx_path_file()
  write_xlsx(pop_admin_mean_c, xlsx_path_file)
  fn_xlsx_open()


##H --------------------------------
### >> Save RData ----
##H --------------------------------
  write_rds(pop_respmean,
   file = "Output/04-RData/pop_respmean.rds")

#========================================
##> Step 4. Obtain the bivariate counts ----
## Prep for R-indicator
#========================================
# Use benchmark data (set as pop); dim(df)
  pop <- df
fn_bivariate_counts_popcov(pop)
    head(popcov)

##H ---------------------------------
## >> Export  ----
##H ---------------------------------

  popcov_df <- as.data.frame(popcov);
  xlsxfile <- "bivariate_counts.xlsx"
    fn_xlsx_path_file()
    write_xlsx(popcov_df, xlsx_path_file)
    # fn_xlsx_open()

# Save
write_rds(popcov,
    file="Output/04-RData/bivariate_counts.rds")


### End ###