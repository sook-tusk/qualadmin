
# Run Custom_Path.R if necessary.
# rm(list = ls())
getwd()

# Run the code file with functions.
source("Functions/1_Functions.R")

# Define output file folders, path
fn_output_folder_path()

# Disable scientific notation.
options(scipen = 999)

library("tidyverse")
library("readxl")
library("writexl")
library("janitor")
library("ggplot2")

##H ----------------------------------------
## > PREP PART 1. ----
## > Generate a weighted sample survey data
##> 0. Recap. Launch output files  ----
##H ----------------------------------------

# xlsxfile <- "Weightedsample_freq_table.xlsx"
# # fn_xlsx_open()

# file <- "WeightedSample_Data_Summary.txt"

#H---------------------------------------
## > Step 1. Load Census data
#H--------------------------------------

load("pop_u_short_before_sim_5vars.RData")

# Call the data as df from now on.
# ls()

df  <- pop_u_short_before_sim_5vars
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

txtfile <- "census_summary.txt"
fn_CreateTableOne_table()

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
##H ----------------------------------

##>>  Adjust to get population count

# Check whether the total adds up to 1
freq_table[1:8, 1:9]
sum(freq_table[1:6, "n"])
sum(freq_table[1:6, "p"])

# Obtain pop count(Master) by multiplying 50
df %>% tabyl(var[5]) %>% adorn_totals()
nrow(df) * 50

  # When % is multiplied by 50, correct!
  # % is 2265    0.097323078
  # % is 113250  0.097323078
  (2265*50)/(nrow(df) * 50)

  # twoway sum check: geoga(a) 1-6 x agecode1 1-14
  # twdigits from 101301 to 106314
  tw  <- freq_table %>%
   filter(twdigits %in% c(101301, 106314)) %>%
   summarise(tw = seq)
  tw
  #   seq
  # 1  43
  # 2 126
  tw[[1]][1]
  tw[[1]][2]
  freq_table %>%
    filter(seq %in% c(tw[[1]][1]:tw[[1]][2])) %>%
    summarise(tw_n = sum(n), tw_p = sum(p))

  # Manual input
  # freq_table %>% filter(seq %in% c(54:137)) %>%
  #    summarise(tw_n = sum(n), tw_p = sum(p))

    tail(freq_table)
    dim(freq_table)
  # View(freq_table)

#H---------------------------------------
## > Step 5. Rename and save ----
#H--------------------------------------

Weightedsample_freq_table <- freq_table %>%
          dplyr::select(seq, twdigits,
              raw_n, wtsample_n = n,
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
  # fn_xlsx_open()

##H ----------------------------------------
## > Step 7. Save RData (use for distance metrics)
##H ----------------------------------------

save(Weightedsample_freq_table,
  file="Output/04-RData/Weightedsample_freq_table.RData")

##H ----------------------------------------
##> PREP PART 2. ----
##> Auxiliary file for R-indicator ----
##  Use Weightedsample_freq_table
##H ----------------------------------------

ls()

popsize <- freq_table %>%
          filter(oneway == 1 & by1 == "geog1") %>%
          summarise(popsize = sum(n))
popsize
freq_table  <- data.frame(freq_table, popsize)
View(freq_table)
# 1163650  # Census obs = 1163659

# Compute meanpop
auxiliary  <- freq_table %>%
          filter(oneway == 1) %>%
          group_by(by1) %>%
          mutate(
              meanpop = n / popsize) %>%
          ungroup()  %>%
          dplyr::select(seq, count = n, by1,
           v, by2, meanpop, raw_n)
names(auxiliary)

firstrow <- data.frame(seq = 0, count = popsize,
            by1 = "total", v = 0, by2 = "00",
            meanpop = 1, raw_n = 0)
names(firstrow)  <- names(auxiliary)
firstrow

auxiliary <- bind_rows(firstrow, auxiliary)

auxiliary <- auxiliary %>%
              mutate(seq = row_number(),
              type = "wtsample")

head(auxiliary)
names(auxiliary)
str(auxiliary)

# View(auxiliary)
# View(freq_table)

wtsample_auxiliary_econg <- auxiliary

# Output
print(wtsample_auxiliary_econg)

# Save RData
save(auxiliary, file = "Output/04-RData/auxiliary.RData")

write_xlsx(auxiliary,
  "Output/03-ExcelOutput/wtsample_auxiliary_econg.xlsx")

# Launches the Excel file
# shell.exec("Output\\03-ExcelOutput\\wtsample_auxiliary_econg.xlsx")

### End ###
