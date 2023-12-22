

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

##H ----------------------------------------
### > PREP PART 1A. ----
###  Use the existing weighted sample data  ----
###   Steps 1-3
##H ----------------------------------------

##H ----------------------------------
### > Step 1: Read in the data ----
##H ----------------------------------

# Please customise
# df  <- read_csv("custom_wtsample.csv")

##H ----------------------------------
## >  For demonstration
##H ----------------------------------
load("pop_u_short_public_release_5vars.RData")
df  <- pop_u_short_public_release_5vars

# rename, keep variables of interest and sort
df <- df %>%
 arrange(geog1, sex, agecode1, eth_code5, econg)
# head(df)

#H ---------------------------------------
## > Summary table ----
#H--------------------------------------
  # ALLOW A MINUTE TO EXECUTE fn_CreateTableOne_table()
  # fn_CreateTableOne_table
  # One needs to install "tableone" package.
  # Ignore this if installation is unsuccessful.
  # The function prints a summary
  # distribution table and save it as .txt file.
  # The generated txt file launches automatically.

  # txtfile <- "wtsample_summary.txt"
  # fn_CreateTableOne_table()

##H ----------------------------------
### > Step 3: Obtain the frequency table ----
##H ----------------------------------

fn_maxvar5_freq_table()

##H ----------------------------------
## >  Step 4. Check  ----
## Check whether the total adds up to 1
##H ----------------------------------

    dim(freq_table)
    tail(freq_table)
    freq_table[1:8, 1:9]
    # View(freq_table)

    # Manual, check the total.
    sum(freq_table[1:6, "n"])
    sum(freq_table[1:6, "p"])

# check the total.
freq_table %>% group_by(by3, by1) %>%
  summarise(sum_freq = sum(n), sum_p = sum(p))

##H ----------------------------------
### > Testing with 4 variables
##H ----------------------------------

# Please customise depending on the maxvar.
# Users may use fn_maxvar4_freq_table() or
# fn_maxvar6_freq_table().
    # df <- df %>%
    #  select(geog1, sex, agecode1, eth_code5)
    # fn_maxvar4_freq_table()

#H---------------------------------------
## > Step 5. Rename and save ----
#H--------------------------------------

Weightedsample_freq_table <- freq_table %>%
          dplyr::select(seq, twdigits,
              wtsample_n = n,
              wtsample_perc = p,
              everything())
# View(Weightedsample_freq_table)

##H ----------------------------------------
## > Step 6. Export outputs  ----
##H ----------------------------------------

  xlsxfile <- "Weightedsample_freq_table.xlsx"
  fn_xlsx_path_file()
  write_xlsx(Weightedsample_freq_table, xlsx_path_file)
  # fn_xlsx_open()

##H ----------------------------------------
## > Step 7. Save RData (use for distance metrics)
##H ----------------------------------------

save(Weightedsample_freq_table,
  file="Output/04-RData/Weightedsample_freq_table.RData")

#H-----------------------------------------------
#H This is for demo version. So, it may be overwritten.
#H----------------------------------------------
#===============================================
##H ----------------------------------------
## > PREP PART 1B. ----
## > Generate a weighted sample survey data ----
##> 0. Recap. Launch output files
##H ----------------------------------------

# xlsxfile <- "Weightedsample_freq_table.xlsx"
# fn_xlsx_open()
# file <- "WeightedSample_Data_Summary.txt"

#H---------------------------------------
## > Step 1. Load Census microdata ----
#H--------------------------------------

load("pop_u_short_public_release_5vars.RData")
ls()

df  <- pop_u_short_public_release_5vars
  dim(df)   # obs = 1163659
  names(df)
  glimpse(df) ; head(df)

#H ---------------------------------------
## > Summary table ----
#H--------------------------------------
# SLOW. ALLOW A MINUTE TO EXECUTE fn_CreateTableOne_table
# One needs to install "tableone" package.
# Ignore this if installation is unsuccessful.
# The function prints a summary
# distribution table and save it as .txt file.
# The generated txt file launches automatically.

# txtfile <- "census_summary.txt"
# fn_CreateTableOne_table()

#H ---------------------------------------
## > Step 2. Draw a random sample ----
#H--------------------------------------
# 1.  Draw a random sample 1:50 from census

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
  dim(df); head(df) ;

#H---------------------------------------
##> Step 3. Obtain oneway, twoway Freq_tables ----
#H--------------------------------------

fn_maxvar5_freq_table()

# Now, n * 50, rename after checking's done!
freq_table  <- freq_table %>%
          rename(raw_n = n) %>%
          mutate(n = raw_n * 50) %>%
          dplyr::select(seq, twdigits,
            raw_n, n, p, everything() )

# View(freq_table)

##H ----------------------------------
## >  Step 4. Check  ----
## Check whether the total adds up to 1
##H ----------------------------------

    dim(freq_table)
    tail(freq_table)
    freq_table[1:8, 1:9]
    # View(freq_table)

    # Manual, check the total.
    sum(freq_table[1:6, "n"])
    sum(freq_table[1:6, "p"])

# automatic, check the total.
freq_table %>% group_by(by3, by1) %>%
  summarise(sum_freq = sum(n), sum_p = sum(p))

#H---------------------------------
## > Step 5. Rename and save ----
#H---------------------------------

Weightedsample_freq_table <- freq_table %>%
          dplyr::select(seq, twdigits,
            wtsample_n    = n,
            wtsample_perc = p, everything())
# View(Weightedsample_freq_table)

##H ---------------------------------
## > Step 6. Export outputs  ----
##H ---------------------------------

  sheets <- list(
    "Weightedsample_freq_table" = Weightedsample_freq_table,
    "Random_sample" = randomsample)

  xlsxfile <- "Weightedsample_freq_table.xlsx"
  fn_xlsx_path_file()
  write_xlsx(sheets, xlsx_path_file)
  # fn_xlsx_open()

##H --------------------------------
### > Step 7. Save RData ----
##H --------------------------------

save(Weightedsample_freq_table,
  file="Output/04-RData/Weightedsample_freq_table.RData")

save(var, file="Output/04-RData/var.RData")

### End ###