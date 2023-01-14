
# Run Custom_Path.R if necessary.
    # rm(list = ls())    # Remove objects
    getwd()

    # Run the code file with functions.
    source("Functions/1_Functions.R")

    # Define output file folders, path
    fn_output_folder_path()

    # Disable scientific notation.
    options(scipen = 999)

    library("tidyverse") # data manipulation
    library("ggplot2")   # visualisation
    library("janitor")   # cross-tabulation
    library("readxl")    # read large csv file
    library("writexl")   # export to Excel

# If the Census data or a weighted sample data are available,
# users consult Part 1 A. Use the existing sample data.
# If these data are unavailable, see
# Part 1 B. Generate a weighted sample data.

##H ----------------------------------------
## > PREP PART 1A. ----
## > Use the existing weighted sample data  ----
##H ----------------------------------------

##H ----------------------------------
## >  Steps 1-3  ----
##H ----------------------------------

## Step 1: Read in the data
# df  <- read_csv("custom_wtsample.csv")  # Please customise

  ##H ----------------------------------
  ## >  For demonstration  ----
  load("pop_u_short_public_release_5vars.RData")
  ls()
  df  <- pop_u_short_public_release_5vars
  ##H ----------------------------------
  dim(df)       # obs = ?
  head(df)      # first 6 obs
  glimpse(df)   # Quick glance at the data
  names(df)     # variable names
  View(df[1:100, ])

## Step 2: Declare variables to be tabulated
  var <- c("geog1", "sex", "agecode1",  # Please customise
            "eth_code5", "econg")
  var

  maxvar <- length(var)           # No need to customise
  maxvar

  # SLOW. ALLOW A MINUTE TO EXECUTE CreateTableOne.
  # The function prints a summary distribution table, then save it as .txt file then launches the .txt file.

  # txtfile <- "wtsample_summary.txt"
  # fn_CreateTableOne_table()

## Step 3: Obtain the frequency table
  f     <- NULL
  t     <- NULL
  last  <- NULL

fn_maxvar5_freq_table()

# Please customise depending on the maxvar.
# Users may use fn_maxvar4_freq_table()
# Beyond 5 maxvar, please create your own functions.

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

#H---------------------------------------
## > Step 5. Rename and save ----
#H--------------------------------------

Weightedsample_freq_table <- freq_table %>%
          dplyr::select(seq, twdigits,
              wtsample_n = n,
              wtsample_perc = p,
              everything())
View(Weightedsample_freq_table)

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

#--------------------------------------------------
# This is for demo version. So, it may be overwritten.
#--------------------------------------------------
#################################################
##H ----------------------------------------
## > PREP PART 1B. ----
## > Generate a weighted sample survey data
##> 0. Recap. Launch output files  ----
##H ----------------------------------------

# xlsxfile <- "Weightedsample_freq_table.xlsx"
# # fn_xlsx_open()
# file <- "WeightedSample_Data_Summary.txt"

#H---------------------------------------
## > Step 1. Load Census/wtsample data
#H--------------------------------------

load("pop_u_short_public_release_5vars.RData")
ls()
# Call the data as df from now on.
# ls()

df  <- pop_u_short_public_release_5vars
dim(df)   # obs = 1163659

names(df)

glimpse(df)
head(df)

#H---------------------------------------
##> Prep var
#H--------------------------------------

var <- c("geog1", "sex", "agecode1",
          "eth_code5", "econg")
(maxvar <- length(var))

# SLOW. ALLOW A MINUTE TO EXECUTE CreateTableOne.
# The function prints a summary distribution table, then save it as .txt file then launches the .txt file.

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
    # dplyr::select(-person_id)

df <- df %>% arrange(decide) %>%
    mutate(decide_seq = row_number()) %>%
    filter(decide_seq <= random50sample)

names(df) ;
head(df)
tail(df)
# View(df)

randomsample <- df

#H---------------------------------------
##> Step 3. Obtain oneway, twoway Freq_tables ----
#H--------------------------------------

f     <- NULL
t     <- NULL
last  <- NULL

fn_maxvar5_freq_table()

# Now, n * 50, rename after checking's done!
freq_table  <- freq_table %>%
          rename( raw_n = n) %>%
          mutate(
            n = raw_n * 50) %>%
          dplyr::select(seq, twdigits,
            raw_n, n, p, everything() )

View(freq_table)

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

#H---------------------------------------
## > Step 5. Rename and save ----
#H--------------------------------------

Weightedsample_freq_table <- freq_table %>%
          dplyr::select(seq, twdigits,
            wtsample_n    = n,
            wtsample_perc = p, everything())
View(Weightedsample_freq_table)

##H ----------------------------------------
## > Step 6. Export outputs  ----
##H ----------------------------------------

  sheets <- list(
    "Weightedsample_freq_table" = Weightedsample_freq_table,
    "Random_sample" = randomsample)

  xlsxfile <- "Weightedsample_freq_table.xlsx"
  fn_xlsx_path_file()
  write_xlsx(sheets, xlsx_path_file)
  fn_xlsx_open()

##H ----------------------------------------
## > Step 7. Save RData
##H ----------------------------------------

save(Weightedsample_freq_table,
  file="Output/04-RData/Weightedsample_freq_table.RData")

save(var, file="Output/04-RData/var.RData")

### End ###