
# Run Custom_Path.R if necessary.
    rm(list = ls())    # Remove objects
    getwd()

    # Run the code file with functions.
    source("Functions/1_Functions.R")

    # Disable scientific notation.
    options(scipen = 999)

    library("dplyr")     # data manipulation
    library("tidyr")     # data manipulation
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

#H-----------------------------------------------
#H This is for demo version. So, it may be overwritten.
#H----------------------------------------------
#===============================================
# > Part 1B, Creating a weighted sample data.

#H========================================
## > Step 1. Generate a weighted sample data
#========================================

# Load Census microdata
load("pop_u_short_public_release_5vars.RData")
ls()

df  <- pop_u_short_public_release_5vars
  dim(df)   # obs = 1163659
  names(df)
  # glimpse(df) ; head(df)

#H--------------------------------------
#  Draw a random sample 1:50 from census
#H--------------------------------------

# Prep random sampling
random50sample <- round(nrow(df)/50)
  nrow(df)
  random50sample # 23273

# Generate random numbers
# from uniform distribution
set.seed(-345)
df$decide <- runif(nrow(df), min = 0, max = 1)
  summary(df$decide)
# hist(df$decide)

randomsample <- df %>% arrange(decide) %>%
    mutate(decide_seq = row_number()) %>%
    filter(decide_seq <= random50sample)

df <- randomsample %>%
    dplyr::select(-starts_with("decide"))
  dim(df);  # 23273
  # head(df) ;
  glimpse(df)

# Examine popsize
popsize <-  nrow(df) * 50 ; popsize

#========================================
## > Step 2. Obtain the frequency table ----
#========================================

#H---------------------------------
## >> a. Benchmark data ----
#H---------------------------------

fn_freq_table(df)

  # Examine
  head(freq_table)

# Now, n * 50, rename after checking's done!
pop_table <- freq_table %>%
          rename(raw_n = n) %>%
          mutate(n = raw_n * 50) %>%
          dplyr::select(seq, n, p, v, v1, v2,
            everything() )
  # Examine
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
  print(pop_admin_freq_table[1:20, 1:13])
  # View(pop_admin_freq_table)

##H ---------------------------------
## >> Export outputs  ----
##H ---------------------------------

  xlsxfile <- "Weightedsample_freq_table.xlsx"
  fn_xlsx_path_file()
  write_xlsx(pop_admin_freq_table, xlsx_path_file)
  # fn_xlsx_open()

##H --------------------------------
### >> Save RData ----
##H --------------------------------

write_rds(pop_admin_freq_table,
  file="Output/04-RData/Weightedsample_freq_table.rds")

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
    # print(pop_respmean)

# Check intermediate outputs
    # View(pop_admin_mean_c)
    # View(mean_c)
    # View(freq)

##H ---------------------------------
## >> Export intermediate outputs  ----
##H ---------------------------------
  xlsxfile <- "weightedsample_pop_admin_mean_c.xlsx"
  fn_xlsx_path_file()
  write_xlsx(pop_admin_mean_c, xlsx_path_file)
  # fn_xlsx_open()


##H --------------------------------
### >> Save RData ----
##H --------------------------------
  write_rds(pop_respmean,
   file = "Output/04-RData/weightedsample_pop_respmean.rds")


#========================================
##> Step 4. Obtain pop bivariate counts ----
## Prep for R-indicator
#========================================
# Use benchmark data (set as pop); dim(df)
  pop <- df
fn_bivariate_counts_popcov(pop)

# Now, apply weights(n * 50)
popcov  <- popcov * 50
    head(popcov)

##H ---------------------------------
## >> Export  ----
##H ---------------------------------
  popcov_df <- as.data.frame(popcov);

  xlsxfile <- "weightedsample_bivariate_counts.xlsx"
    fn_xlsx_path_file()
    write_xlsx(popcov_df, xlsx_path_file)
    # fn_xlsx_open()

# Save
  write_rds(popcov,
    file="Output/04-RData/weightedsample_bivariate_counts.rds")

### End ###
