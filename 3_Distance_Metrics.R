
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

#H-----------------------------------
##> Step 1: Prep  ----
#H-----------------------------------
# Run one of the previous files as appropriate
# source("2_A_Prep_auxiliary.R")
source("2_B_Prep_auxiliary_CreateRandomSample.R")

  head(pop_admin_freq_table)

#H ----------------------------------------
#> Step 2: Compute distance_metrics ----
#H ----------------------------------------

fn_distance_metrics()
# View(distance_metrics)
distance_metrics[1:6, 1:6]

##H ----------------------------------------
##>> Export
##H----------------------------------------

 # xlsxfile <- "distance_metrics.xlsx"
 xlsxfile <- "weightedsample_distance_metrics.xlsx"
  fn_xlsx_path_file()
  write_xlsx(distance_metrics, xlsx_path_file)
  # fn_xlsx_open()

#H-------------------------------------
## > Step 3: Reshape for plotting ----
#H-------------------------------------

fn_distance_metrics_long()

# Print all
  upto  <- nrow(distance_metrics_long)
  print(distance_metrics_long, n = upto)

#H-------------------------------------
## > Step 4a: Visualisation prep ----
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
# >  Step 4b: Scatterplot, all 3 ----
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

 # figfile <- "scatter_distancemetrics.png"
 figfile <- "weightedsample_scatter_distancemetrics.png"
  fn_fig_path_file()
 ggsave(fig_path_file)

#H ----------------------------------------
# > Step 4c: Scatterplot, NOT-flipped
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

 # figfile <- "scatter_distancemetrics2.png"
 figfile <- "weightedsample_scatter_distancemetrics2.png"
  fn_fig_path_file()
  ggsave(fig_path_file)

### End ###
