
# Run Custom_Path.R if necessary.
# rm(list = ls())
getwd()

# Disable scientific notation.
options(scipen = 999)

# Define output file folders, path
source("Functions/1_Functions.R")

fn_output_folder_path()

library("tidyverse")
library("readxl")
library("writexl")
library("janitor") # tabulations
library("ggplot2")

# H------------------------------------------
##  Preliminary step
## > 0. Obtain wtsample f_table
## (to serve as the benchmarks)
# H------------------------------------------

# source("2_Prep_Wtsample_Freq_Table.R")

xlsxfile <- "Weightedsample_freq_table.xlsx"
# fn_xlsx_open()

## Now admin data

#H------------------------------
## > Step 1: read admin data ----
#H------------------------------

df  <- read_csv("public_release_admin.csv")
nrow(df)
head(df)

glimpse(df)
names(df)

View(df[1:100, ])

#H------------------------------------------
##>>  Initial look on admin freq tables 
#H------------------------------------------

# Declare variables to be used.
# Please customise as needed.
  var <- c("geog1", "sex", "agecode1",
            "eth_code5", "econg")

# Check, first and second variables
# Please inspect if frequency counts are correct!

    var

    df %>% tabyl(var[1]) %>% adorn_totals()
    df %>% tabyl(var[2]) %>% adorn_totals()

    # cross-tabulation
    addmargins(table(df[, c(var[1], var[2])]))

#H------------------------------------------
## >  Step 2: Obtain admin freq tables ----
#H------------------------------------------

  maxvar <- length(var)
  maxvar
  t     <- NULL
  last  <- NULL

# Obtain frequency tables
  fn_maxvar5_freq_table()

  Admin_f_table_one  <- freq_table %>%
    rename(admin_n = n, admin_perc = p)

  # View(Admin_f_table_one)

#H----------------------------------------------
## > Step 2: Inspect
#H----------------------------------------------
  # Elaborate in manual.
  head(Admin_f_table_one)
  tail(Admin_f_table_one)


  dim(Admin_f_table_one)
  names(Admin_f_table_one)

#H----------------------------------------------
## >  Step 3: Merge admin + Wtsample  freq table ----
#H----------------------------------------------

fn_merge_one_admin_wtsample_f_table_temp()
print(temp[1:20, 1:9])
names(temp)
View(temp)

#H----------------------------------------------
## > Ready to move onto distance metrics 
#H----------------------------------------------

#H----------------------------------------------
## > Step 4: Create Quality indicators DOMAIN ----
#H----------------------------------------------

  fn_create_domain_temp()

  View(temp)

  display_domain <- temp

  # for user manual, display_domain
  display_domain %>% tabyl(fct_domain)

#H ----------------------------------------
#> Step 5: Compute distance_metrics ----
#H ----------------------------------------

fn_unstd_distance_metrics_full()
# View(unstd_distance_metrics_full)

fn_unstd_distance_metrics_tidy()
View(unstd_distance_metrics_tidy)

#H ----------------------------------------
# > Step 6: Standardise distance metrics ----
#H ----------------------------------------

  temp <- unstd_distance_metrics_tidy %>%
    mutate(
     Std_Duncan = 1 - Duncan,
     Std_t_HD = 1-HD,
     Std_t_KL = 1-KL
   )

  std_distance_metrics_wide  <- temp

##H ----------------------------------------
##>> CHECK: (OPTIONAL) launch distance_metrics_wide
##H ----------------------------------------

 xlsxfile <- "distance_metrics_wide.xlsx"
  fn_xlsx_path_file()
  write_xlsx(std_distance_metrics_wide, xlsx_path_file)
  fn_xlsx_open()

#H-------------------------------------
## > Step 7: Reshape for plotting ----
#H-------------------------------------

  fn_distance_metrics_long()

  head(distance_metrics_long)
  View(distance_metrics_long)

  df <- distance_metrics_long
  # std_test(1-Duncan, 1-HD, 1-KL) only
  df <- df %>% filter(std_test_use == 1)

  # for manual
  df[1:9, c(1:2, 4:5, 9)]

# View(df)

#H-------------------------------------
## > Step 8a: Visualisation prep ----
#H-------------------------------------

  sh  <- scale_shape_manual(
          name = "Reference & indicator",
          labels = c("Duncan", "HD", "KL"),
          values = c(2, 1, 0)
        )

  s  <- scale_color_manual(
      name = "Reference & indicator",
      labels = c("Duncan", "HD", "KL"),
      values = c(
        "#5b5b5b",
        "steelblue",
        "#d04a99" )
    )

  sh2  <- scale_shape_manual(
          name = "Reference & indicator",
          labels = c("Duncan", "HD"),
          values = c(2, 0)
        )

  s2  <- scale_color_manual(
      name = "Reference & indicator",
      labels = c("Duncan", "HD"),
      values = c(
        "#5b5b5b",
        "#d04a99" )
    )

  theme_set(theme_bw())

#H ----------------------------------------
# >  Step 8b: Scatterplot, all 3 ----
#H ----------------------------------------

# names(df) ; str(df) ;
 # alpha = 0.7,
p <- df %>%
    ggplot (aes(x = fct_domain, y = index
      )) +
    geom_point(aes(shape = ref,
      colour = ref, stroke = 1.2)) +
    labs( x = "Domain", y = "Distance metrics index")  +
    coord_flip() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 12)
    ) + sh + s
plot(p)

 figfile <- "Scatter_distance_metrics_all3_flipped.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

#H ----------------------------------------
# > Step 8: Scatterplot, NOT-flipped
#H ----------------------------------------
# View(df)

pz <- df %>%
    ggplot (aes(x = fct_domain, y = index)) +
    geom_point(aes(shape = ref,
      colour = ref, stroke = 1.1)) +
    labs( x = "", y = "Distance metrics") +
  theme(
      legend.position = "top",
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x=element_text(angle = 90,
      vjust = 0.5)
  ) + sh + s
plot(pz)

 figfile <- "Scatter_distance_metrics_not_flipped.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

### End ###
