

# Run Custom_Path.R if necessary.
rm(list = ls())
getwd()

# Define output file folders, path
source("Functions/1_Functions.R")

# Disable scientific notation.
options(scipen = 999)

    library("dplyr")     # data manipulation
    library("ggplot2")   # visualisation
    library("readr")     # read large csv file
    library("tidyr")     # data manipulation
    library("readxl")    # read Excel file
    library("writexl")   # export to Excel
    library("janitor")   # cross-tabulation

# H------------------------------------------
##  Preliminary step
## > 0. Obtain wtsample f_table
## (to serve as the benchmarks)
# H------------------------------------------

# Run the file to obtain Weightedsample_freq_table
source("2_Prep_Wtsample_Freq_Table.R")

xlsxfile <- "Weightedsample_freq_table.xlsx"
# fn_xlsx_open()

#H------------------------------
## Now admin data
## > Step 1: read admin data ----
#H------------------------------

df  <- read_csv("public_release_admin.csv")

# rename, keep variables of interest and sort
df <- df %>%
 dplyr::select(-person_id) %>%
 arrange(geog1, sex, agecode1, eth_code5, econg)

head(df)

# View(df[1:100, ])

#H------------------------------------------
##>>  Initial look on admin freq tables
#H------------------------------------------

# Check, first and second variables
# Please inspect if frequency counts are correct!
    var  <- names(df) ; var
    df %>% tabyl(var[1]) %>% adorn_totals()
    df %>% tabyl(var[2]) %>% adorn_totals()

    # cross-tabulation
    addmargins(table(df[, c(var[1], var[2])]))

#H------------------------------------------
## >  Step 2: Obtain admin freq tables ----
#H------------------------------------------

fn_maxvar5_freq_table()

Admin_f_table_one <- freq_table %>%
    rename(admin_n = n, admin_perc = p)

## > Inspect (Elaborate in manual)
  dim(Admin_f_table_one)
  # head(Admin_f_table_one)
  tail(Admin_f_table_one)
  # names(Admin_f_table_one)
  # View(Admin_f_table_one)
  # View(Weightedsample_freq_table)

#H----------------------------------------------
## >  Step 3: Merge admin + Wtsample  freq table ----
#H----------------------------------------------

freq_table2 <- full_join(Weightedsample_freq_table,
                  Admin_f_table_one)

freq_table2 <- freq_table2 %>%
    relocate(starts_with("by"), .after = last_col())
  dim(freq_table2)
  print(freq_table2[1:20, 1:9])
# View(temp)

#H ----------------------------------------
#> Step 4: Compute distance_metrics ----
#H ----------------------------------------

fn_distance_metrics()
# View(distance_metrics)
distance_metrics[1:6, 1:6]

##H ----------------------------------------
##>> CHECK: (OPTIONAL) launch distance_metrics_wide
##H
----------------------------------------

 xlsxfile <- "distance_metrics.xlsx"
  fn_xlsx_path_file()
  write_xlsx(distance_metrics, xlsx_path_file)
  # fn_xlsx_open()

#H-------------------------------------
## > Step 5: Reshape for plotting ----
#H-------------------------------------

fn_distance_metrics_long()

# Print all
  upto  <- nrow(distance_metrics_long)
  print(distance_metrics_long, n = upto)

#H-------------------------------------
## > Step 6a: Visualisation prep ----
#H-------------------------------------
df  <- distance_metrics_long
  # View(df)

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
# >  Step 6b: Scatterplot, all 3 ----
#H ----------------------------------------

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
# > Step 6c: Scatterplot, NOT-flipped
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
