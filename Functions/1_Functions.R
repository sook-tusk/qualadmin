
##H ----------------------------------------
##   Not to be customised.
##   Please run as it is.
##H ----------------------------------------

##H ----------------------------------------
##>> Define output file folders, path ----
##H ----------------------------------------


fn_output_folder_path <- function() {

  currentdate <<- Sys.Date()
  txtpath   <<- "Output/01-Txt/"
  figpath   <<- "Output/02-Figure/"
  xlsxpath  <<- "./Output/03-ExcelOutput/"
  Rdatapath <<- "Output/04-RData/"
}

# Combine path and filename
fn_txt_path_file <- function() {
  # prep for capture.output(), cat()
  txt_path_file <<- paste0(txtpath, txtfile)
}

fn_xlsx_path_file <- function() {
  xlsx_path_file <<- paste0(xlsxpath, xlsxfile)
}

fn_fig_path_file <- function() {
  fig_path_file <<- paste0(figpath, figfile)
}

fn_Rdata_path_file <- function() {
  Rdata_path_file <<- paste0(Rdatapath, Rdatafile)
}

# Open
fn_txt_open <- function() {
 shell.exec (normalizePath(file.path(txtpath, txtfile)))
}

fn_xlsx_open <- function() {
 shell.exec (normalizePath(file.path(xlsxpath, xlsxfile)))
}

fn_fig_open <- function() {
 shell.exec (normalizePath(file.path(figpath, figfile)))
}

# Define output file folders, path
fn_output_folder_path()

##H ----------------------------------------
##>> strata_p  ----
##H ----------------------------------------

# TO BE REMOVED in public release. Simulation_x20

fn_12strata_p <- function() {
  temp <<- NULL
  (temp  <<- data.frame(rbind(s1, s2, s3, s4, s5,
    s6, s7, s8, s9, s10, s11, s12)))
  temp$seq  <<- row.names(temp)
  temp$seq <<- seq.int(nrow(temp))
  row.names(temp) <<- 1:nrow(temp)
  colnames(temp) <<- c(
      paste0("p", 1:4), "strata")
  temp  <<- temp[, c("strata", paste0("p", 1:4))]
  temp  <<- data.frame(temp)
  temp
  temp$pp1  <<- temp$p1
  temp$pp2  <<- temp$p1 + temp$p2
  temp$pp3  <<- temp$p1 + temp$p2 + temp$p3
  temp$pp4  <<- temp$p1 + temp$p2 + temp$p3 + temp$p4
  strata_p <<- temp
  strata_p
}

##H ----------------------------------------
##>> Summary Table using CreateTableOne ----
##H ----------------------------------------

fn_CreateTableOne_table <- function() {
  # prep for capture.output()
  txt_path_file <<- paste0(txtpath, txtfile)

  df_ <<- df %>%
    mutate_at(var, factor)
  glimpse(df)
library(tableone)
  a <<- print(CreateTableOne(data = df_),
              showAllLevels = TRUE)
  capture.output(noquote(txtfile), noquote(a),
    file = txt_path_file)
  fn_txt_open()
}

##H ----------------------------------------
##>> Visualise using summarytools ----
##H ----------------------------------------

fn_save_visualisation_summarytools <- function() {

  # View in browser
  # stview(dfSummary(df))

  html_path_file <<- paste0(figpath, htmlfile)
  stview(dfSummary(df), file = html_path_file)

}

fn_visualisation_open_in_browser <- function() {
 shell.exec (normalizePath(file.path(figpath, htmlfile)))
}

##H ----------------------------------------
##>>  Size of categories, last cat  ----
##H ----------------------------------------
# nvar must exist first!
fn_nvar <- function() {
  nvar  <<- NULL
  variablenum <<- length(var)
  # levelsvar  <<- sapply(aa, levels)

    for (i in 1:variablenum) {
    nvar[i] <<- length(levelsvar[[i]])
    # print(nvar)
  }
}

fn_lastcat <- function() {

  a <<- NULL
  for (i in 1:variablenum) {
   a[i] <<- print(paste0(var[i], "_", nvar[i]))
   }
 lastcat <<- as.vector(a)
}

#H--------------------------------------------
##> postloop_treat_rownames_temp ----
#H--------------------------------------------

fn_postloop_treat_rownames_temp <- function() {
  temp <<- magic_result()
  temp <<- as.data.frame(temp)
# Create a non-character variable, seq.
  temp$seq  <<- row.names(temp)
  temp$seq  <<- as.integer(temp$seq)
  row.names(temp) <<- 1:nrow(temp)
}

fn_treat_rownames_temp <- function() {
  temp <<- as.data.frame(temp)
# Create a non-character variable, seq.
  temp$seq  <<- row.names(temp)
  temp$seq  <<- as.integer(temp$seq)
  row.names(temp) <<- 1:nrow(temp)
}

##H ----------------------------------------
##>>  distance_metrics_wide  ----
##H ----------------------------------------

fn_distance_metrics_wide <- function() {

  # Print Summary table
  distance_metrics_wide <-  distance_metrics %>%
      group_by(domain_id) %>%
      mutate(
        firstobs = as.integer(
          row_number() == 1)) %>%
      filter(firstobs == 1) %>%
      ungroup() %>%
      arrange(domain_id) %>%
      dplyr::select(
        domain_id, domain, fct_domain,
       contains("duncan"),
       contains("hellinger"),
       contains("kullback"), entropy_p
        ) %>%
      print(n = nrow(.))
# View(distance_metrics_wide)

  distance_metrics_wide <<- distance_metrics_wide %>%
     rename(
      Duncan = B_duncan,
      Std_Duncan = B_std_duncan,
      HD = B_hellinger,
      Std_HD = B_std_hellinger,
      KL = B_kullback,
      Std_KL = B_std_kullback) %>%
      mutate(
        Std_HD_ = 1-HD,
        Std_KL_ = 1-KL
      ) %>%
      relocate(Std_HD_, .after = Std_HD) %>%
      relocate(Std_KL_, .after = Std_KL)
}

# TO BE REMOVED in public release. Simulation_x20
fn_distance_metrics_wide_x20 <- function() {

  # Print Summary table
  distance_metrics_w <<-  distance_metrics %>%
      group_by(domain_id) %>%
      mutate(
        firstobs = as.integer(
          row_number() == 1)) %>%
      filter(firstobs == 1) %>%
      ungroup() %>%
      arrange(domain_id) %>%
      dplyr::select(
       domain_id, domain, fct_domain,
       contains("duncan"),
       contains("hellinger"),
       contains("kullback"), entropy_p
        ) %>%
      print(n = nrow(.))
  # View(distance_metrics_wide)
  distance_metrics_wide <<- distance_metrics_w %>%
     rename(
      Duncan = B_duncan,
      Std_Duncan = B_std_duncan,
      HD = B_hellinger,
      Std_HD = B_std_hellinger,
      KL = B_kullback,
      Std_KL = B_std_kullback) %>%
      mutate(
        Std_HD_ = 1-HD,
        Std_KL_ = 1-KL
      ) %>%
      relocate(Std_HD_, .after = Std_HD) %>%
      relocate(Std_KL_, .after = Std_KL)
}

##H ----------------------------------------
##>> Reshape, distance_metrics_long ----
##H ----------------------------------------

fn_distance_metrics_long <- function() {

  # Reshape, gather columns except the first four.
  # key=stub, create 2 new vars!
  # Removed , factor_key=TRUE
  # to allow flexibility in graphics.
  temp_ <<-
    std_distance_metrics_wide %>%
      dplyr::select(-entropy_p) %>%
      gather(key = "indicator",
          value = "index", -(1:3)) %>%
      arrange(domain_id, indicator)

# ref1 = A if the indicator starts with A(^A)
# ref2 = 1 if the indicator ends with x(x$)
  distance_metrics_long <<- temp_ %>%
      mutate(
        std =
        ifelse(grepl("^Std", indicator),
          1, 0),
        std_test_temp =
        ifelse(grepl("^Std_t_", indicator),
          1, 0),
        ref =
        ifelse(grepl("Duncan", indicator), 1,
        ifelse(grepl("HD", indicator), 2, 3) ),
        std_test_use = ifelse(std == 1 &
          (ref == 1 | std_test_temp == 1), 1, 0)) %>%
      mutate_at(vars(std, ref), factor) %>%
      arrange(domain_id, std_test_use, indicator)
}


# TO BE REMOVED in public release. Simulation_x20

fn_distance_metrics_long_x20 <- function() {

  # Reshape, gather columns except the first four.
  # key=stub, create 2 new vars!
  # Removed , factor_key=TRUE
  # to allow flexibility in graphics.
 temp_ <- std_distance_metrics_wide_x20 %>%
      dplyr::select(-entropy_p) %>%
      gather(key = "indicator",
          value = "index", -(1:3)) %>%
      arrange(domain_id, indicator)

# ref1 = A if the indicator starts with A(^A)
# ref2 = 1 if the indicator ends with x(x$)
  temp <<- temp_ %>%
      mutate(
        std =
        ifelse(grepl("^Std", indicator),
          1, 0),
        std_test_temp =
        ifelse(grepl("^Std_t_", indicator),
          1, 0),
        ref =
        ifelse(grepl("Duncan", indicator), 1,
        ifelse(grepl("HD", indicator), 2, 3) ),
        std_test_use = ifelse(std == 1 &
          (ref == 1 | std_test_temp == 1), 1, 0)) %>%
      mutate_at(vars(std, ref), factor) %>%
      arrange(domain_id, std_test_use, indicator)
# Indicator not sorted.
# HD1, HD10, HD11 --> HD1, HD2, HD3, ...
# Create a to show 1-20 seq.
  temp$a <- regmatches(temp$indicator, gregexpr("[[:digit:]]+", temp$indicator))
  temp$a <- as.numeric(temp$a)

  distance_metrics_long_x20 <<- temp %>%
      arrange(domain_id, ref, std_test_use, a, indicator)
}

##H ----------------------------------------
##> Compute unstd_distance_metrics ----
##H ----------------------------------------

# for public release (wtsample-based only)
fn_unstd_distance_metrics_full <- function() {
  temp_ <<- temp %>%
    arrange(seq) %>%
    mutate(
      entropy_ptemp =
       ifelse(wtsample_perc != 0, wtsample_perc * log2(wtsample_perc), 0) ) %>%
    group_by(domain_id) %>%
    mutate(
      entropy_psigma = sum(entropy_ptemp),
      entropy_p = -1 * entropy_psigma ) %>%
    ungroup() %>%
    mutate(abs_perc = abs(wtsample_perc - admin_perc)) %>%
    group_by(domain_id) %>%
    mutate(
     Duncan = 0.5 *sum(abs_perc)) %>%
    ungroup()

  unstd_distance_metrics_full <<- temp_ %>%
    mutate(
      htemp = sqrt(wtsample_perc) - sqrt(admin_perc),
      htempsq = htemp^2,
      ktemp = ifelse(wtsample_perc != 0 & admin_perc != 0,
              wtsample_perc * log2((wtsample_perc/admin_perc)), 0)
    ) %>%
    group_by(domain_id) %>%
    mutate(
      htempsq_sum = sum(htempsq),
      HD = sqrt(htempsq_sum)/(sqrt(2)),
      KL = sum(ktemp)
        ) %>%
    ungroup() %>%
    arrange(domain_id) %>%
    dplyr::select(
     domain_id, domain, fct_domain,
     contains("Duncan"),
     contains("HD"),
     contains("KL"), entropy_p, everything()
      )
}

fn_unstd_distance_metrics_tidy <- function() {
  temp <-  unstd_distance_metrics_full %>%
      group_by(domain_id) %>%
      mutate(
        firstobs = as.integer(
          row_number() == 1)) %>%
      filter(firstobs == 1) %>%
      ungroup() %>%
      arrange(domain_id) %>%
      dplyr::select(
       domain_id, domain, fct_domain,
       contains("Duncan"),
       contains("HD"),
       contains("KL"), entropy_p
        )
  unstd_distance_metrics_tidy  <<- temp
}

##H ----------------------------------------
##>   Compute unstd_distance_metrics_x20 ----
##H ----------------------------------------

# TO BE REMOVED in public release. Simulation_x20
fn_unstd_distance_metrics_full_x20 <- function() {

  (p_col    <<- paste0("p", 1: a_col))
  abs_perc <<- paste0("abs_perc", 1: a_col)
  Duncan <<- paste0("Duncan", 1: a_col)
  # A_std_duncan <<- paste0("A_std_duncan", 1: a_col)

  temp_ <<- temp %>%
    arrange(seq) %>%
    mutate(
      entropy_ptemp =
       ifelse(perc != 0, perc * log2(perc), 0) ) %>%
    group_by(domain_id) %>%
    mutate(
      entropy_psigma = sum(entropy_ptemp),
      entropy_p = -1 * entropy_psigma ) %>%
    ungroup() %>%
    mutate_at(vars(abs_perc = all_of(p_col)),
     list(~ abs(perc - . )) ) %>%
    group_by(domain_id) %>%
    mutate_at(
     vars(Duncan = all_of(abs_perc)), list(~0.5*(sum(.))) ) %>%
    # mutate_at(
    #  vars(A_std_duncan = all_of(Duncan)), list(~1 - . )) %>%
    ungroup()
  # Can be more elegant later...?
  (htemp  <<- paste0("htemp", 1: a_col))
  htempsq <<- paste0("htempsq", 1: a_col)
  ktemp   <<- paste0("ktemp", 1: a_col)
  htempsq_sum <<- paste0("htempsq_sum", 1: a_col)

  HD <<- paste0("HD", 1: a_col)
  KL <<- paste0("KL", 1: a_col)

  temp_ <<- temp_ %>%
    mutate_at(
     vars(htemp = all_of(p_col)), list(~ sqrt(perc) - sqrt(.))) %>%
    mutate_at(
     vars(htempsq = all_of(htemp)), list(~ .^2)) %>%
    mutate_at(
     vars(ktemp = all_of(p_col)),
     list(~ ifelse(perc != 0 & . != 0,
              perc * log2((perc/.)), 0)))

  unstd_distance_metrics_full_x20 <<- temp_ %>%
    group_by(domain_id) %>%
    mutate_at(
     vars(htempsq_sum = all_of(htempsq)), list(~sum(.))) %>%
    mutate_at(
     vars(HD = all_of(htempsq_sum)), list(~sqrt(.)/(sqrt(2)))) %>%
    mutate_at(
     vars(KL = all_of(ktemp)), list(~ sum(.))) %>%
    ungroup()
}

fn_unstd_distance_metrics_tidy_x20 <- function() {
  temp <-  unstd_distance_metrics_full_x20 %>%
      group_by(domain_id) %>%
      mutate(
        firstobs = as.integer(
          row_number() == 1)) %>%
      filter(firstobs == 1) %>%
      ungroup() %>%
      arrange(domain_id) %>%
      dplyr::select(
       domain_id, domain, fct_domain,
       contains("Duncan"),
       contains("HD"),
       contains("KL"), entropy_p
        )
  unstd_distance_metrics_tidy_x20  <<- temp
}

##H ----------------------------------------
## > Clean and create Quality indicators domain
##H ----------------------------------------

fn_create_domain_temp <- function() {
  domaintemp <- temp %>%
    mutate(
    temp_id = as.numeric(paste0(oneway, v, by4))
            ) %>%
    relocate(temp_id, .after = seq)

  temp <- domaintemp %>%
      group_by(temp_id) %>%
      mutate(
        domain_id = cur_group_id(),
        domain = paste0(
          ifelse(by3 == "oneway", by1,
            paste0(by1, ":", by3)))) %>%
      ungroup() %>%
      dplyr::select(seq, twdigits, domain_id,
        domain, temp_id, wtsample_n, wtsample_perc,
        admin_n, admin_perc,
        everything() )
  # New factor domain variable
  order  <- unique(temp$domain)
  order
  temp$fct_domain <- factor(temp$domain,
    levels = order)

  levels(temp$fct_domain)
  temp %>% tabyl(fct_domain)

  temp  <<- temp %>%
      relocate(fct_domain, .after = domain)

}

# Customised domain, to be used for graphics
fn_custom_domain_df <- function() {
  temp  <- df

    temp$custom_domain <- factor(temp$domain_id)
    levels(temp$custom_domain) <- order
    temp  <- temp %>%
        relocate(custom_domain, .after = domain)
    temp %>% tabyl(custom_domain)

  df  <<- temp
}

# TO BE REMOVED in public release. Simulation_x20
# Simulation only n, perc,
fn_create_domain_temp_x20 <- function() {
  domaintemp <<- temp %>%
   mutate(
   temp_id = as.numeric(paste0(oneway, v, by4))
         ) %>%
         relocate(temp_id, .after = seq)
  temp <<- domaintemp %>%
    group_by(temp_id) %>%
    mutate(
        domain_id = cur_group_id(),
        domain = paste0(
          ifelse(by3 == "oneway", by1,
            paste0(by1, ":", by3)))) %>%
    ungroup() %>%
    dplyr::select(seq, twdigits, domain_id,
     domain, temp_id, wtsample_n, wtsample_perc,
      everything() )
  # New factor domain variable
  order  <- unique(temp$domain)
  order
  temp$fct_domain <- factor(temp$domain,
    levels = order)

  levels(temp$fct_domain)
  temp %>% tabyl(fct_domain)

  temp  <<- temp %>%
      relocate(fct_domain, .after = domain)
}



##H -------------------------------
## > resppop, sim_obs, obs size ----
##H -------------------------------

# TO BE REMOVED in public release. Simulation_x20

fn_sim_obs <- function() {
  sim_obs  <<- NULL
  for (i in 1:simsize) {
    sim_obs[i]  <<- nrow(sim_list[[i]])
  }
  cbind(sim_obs)
}

fn_sim_obs_diff <- function() {

  t  <- stack_part3_create_domain %>%
    filter(type == "invp_used") %>%
    group_by(a) %>%
    summarise(invp_wt = first(total))
  t

  temp <- data.frame(sim_obs, t,
    wtsample = 1163650)
  temp
  sim_obs_diff <- temp %>%
      dplyr::select(a, admin = sim_obs, everything()) %>%
      mutate(
      diff_admin  = wtsample - admin,
      diff_admin_p = (wtsample - admin)/wtsample,
      diff_invp_wt = wtsample - invp_wt,
      diff_invp_wt_p =(wtsample - invp_wt)/wtsample
    )
  sim_obs_diff <<- round(sim_obs_diff, 3)
  sim_obs_diff_memo  <<- paste0("diff: subtracted from wtsample, 1163650"    )
  print(sim_obs_diff_memo)
  round(sim_obs_diff, 3)
}

# total consistency confirmed across 15 domains.
# t  <- a %>% filter(type == "invp_used") %>%
#   group_by(a, domain_id) %>%
#   summarise(First_value_total = first(total))
#   # tabyl(a, total)
# View(t)

##H -------------------------------
## > fn_invp_maxvar5_f_table ----
##H -------------------------------

fn_invp_maxvar5_f_table <- function() {

  #H----------------------------------
  ##> 2 One-way
  #H----------------------------------
    # One-way
  invp_oneway_list <- NULL
  for (i in 1:length(var)) {
    a <- wtd.table(df[, c(var[i])],
          weights = df[, c("invp")])
    invp_oneway_list[[i]] <- data.frame(v = i, a,
      total = sum(a))
  }

      length(invp_oneway_list)
      # print(invp_oneway_list[[3]])

  invp_oneway <- bind_rows(invp_oneway_list)
  # invp_oneway

  # xlsxfile <- "invp_freq_table_x20.xlsx"
  #   fn_xlsx_path_file()
  #   write_xlsx(invp_oneway, xlsx_path_file)
  # fn_xlsx_open()

  #H----------------------------------
  ##> 2 Two-way
  #H----------------------------------

  # 1 x 2-5
  invp_tw1_list <- NULL
  for (i in 2:length(var)) {
    t <- wtd.table(df[, "geog1a"], df[, c(var[i])],
        weights = df[, c("invp")])
    t <- data.frame(v = 1, t,
          total = sum(t))
    invp_tw1_list[[i]] <- t %>% arrange(v, Var1, Var2)

  }
      length(invp_tw1_list)
      # print(invp_tw1_list[[2]] )

# 2 x 3-5
  invp_tw2_list <- NULL
  for (i in 3:length(var)) {
    t <- wtd.table(df[, "sex"], df[, c(var[i])],
        weights = df[, c("invp")])
    t <- data.frame(v = 2, t,
          total = sum(t))
    invp_tw2_list[[i]] <- t %>% arrange(v, Var1, Var2)
  }
      length(invp_tw2_list)
      # print(invp_tw2_list[[4]])

# 3 x 4-5
  invp_tw3_list <- NULL
  for (i in 4:length(var)) {
    t <- wtd.table(df[, "agecode1"], df[, c(var[i])],
        weights = df[, c("invp")])
    t <- data.frame(v = 3, t,
          total = sum(t))
    invp_tw3_list[[i]] <- t %>% arrange(v, Var1, Var2)

  }
      length(invp_tw3_list)
      # print(invp_tw3_list[[5]])

# 4 x 5
  t <- wtd.table(df[, "eth_code5"], df[, c(var[5])],
        weights = df[, c("invp")])
    t <- data.frame(v = 4, t,
          total = sum(t))
    invp_tw4_list <- t %>% arrange(v, Var1, Var2)
      # print(invp_tw4_list)

# Merge
  t <- bind_rows(invp_oneway,
            invp_tw1_list, invp_tw2_list,
            invp_tw3_list, invp_tw4_list)
  invp_maxvar5_f_table <<- t
  head(invp_maxvar5_f_table)
}

#H--------------------------------------------
##> 5 Add labels, Tidy
#H--------------------------------------------

  # t %>% tabyl(v, v.1)
  # t %>% tabyl(by2, Var1)  # same
  # t %>% tabyl(by5, Var2)  # same

fn_invp_maxvar5_f_table_tidy <- function() {

  t  <- data.frame(invp_maxvar5_f_table, stem)

  t  <- t %>%
        mutate(oneway = ifelse(by4 == 0, 1, 2)) %>%
        dplyr::select(seq, twdigits, v,
        starts_with("by"),
        invp_n = Freq, invpn_total = total, oneway,
        Var1_by2 = Var1, Var2_by5 = Var2, everything())
  invp_maxvar5_f_table_tidy <<- t
  head(invp_maxvar5_f_table_tidy)
  # View(t)
}

##H -------------------------------
## > fn_maxvar5_freq_table ----
##H -------------------------------

# 1. Declare a freq table function
fn_get_n_1 <- function(col_name) {
  t <<- table(df[, c(var[1], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 1)
}

fn_get_n_2 <- function(col_name) {
  t <<- table(df[, c(var[2], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 2)
}

fn_get_n_3 <- function(col_name) {
  t <<- table(df[, c(var[3], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 3)
}

fn_get_n_4 <- function(col_name) {
  t <<- table(df[, c(var[4], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 4)
}

fn_get_n_last5 <- function() {
  t <<- table(df[, c(var[5])])
  last <<- data.frame(
   prop.table(t), n = c(t), v = 5, last = 1)
  # rename
  names(last)[1] <<- var[5]
}

fn_maxvar5_freq_table  <- function() {
  # without factors, slow freq calculations !!!
  df <<-  df %>%
        dplyr::select(any_of(var)) %>%
        mutate_at(var, factor)
  tw1  <<- lapply(names(df),            fn_get_n_1)
  tw2  <<- lapply(names(df[, -c(1)]),   fn_get_n_2)
  tw3  <<- lapply(names(df[, -c(1:2)]), fn_get_n_3)
  tw4  <<- lapply(names(df[, -c(1:3)]), fn_get_n_4)
  fn_get_n_last5()
  last; length(tw1) ; length(tw2) ;length(tw3)
  tw1[1] ; tw1[2] ; # tw1[4]
  tw2[3] ; # tw3[2] ; # tw4[2]
  sum(tw1[[4]][4]) # geog1a : agecode1
  sum(tw4[[2]][4]) # eth_code : econg
  temp <<- bind_rows(tw1, tw2, tw3, tw4, last)
  temp <<- as.data.frame(temp)
  row.names(temp) <<- 1:nrow(temp)
  temp <<- temp %>%
        dplyr::select(any_of(var),
         n, v, p = Freq, everything()) %>%
        mutate_at(var, as.numeric)
  # by2 copies the values of columns 1-4
  temp <<- temp %>%
   mutate(
    by1 = case_when(
      v == 1 ~ var[1],
      v == 2 ~ var[2],
      v == 3 ~ var[3],
      v == 4 ~ var[4],
      v == 5 ~ var[5] ),
    by2 = case_when(
      v == 1 ~ .[[1]],
      v == 2 ~ .[[2]],
      v == 3 ~ .[[3]],
      v == 4 ~ .[[4]],
      v == 5 ~ .[[5]] ),
    by3 = case_when(
      v == 1 & .[[2]] > 0 ~ var[2],
      v == 1 & .[[3]] > 0 ~ var[3],
      v == 2 & .[[3]] > 0 ~ var[3],
      v == 1 & .[[4]] > 0 ~ var[4],
      v == 2 & .[[4]] > 0 ~ var[4],
      v == 3 & .[[4]] > 0 ~ var[4],
      v == 1 & .[[5]] > 0 ~ var[5],
      v == 2 & .[[5]] > 0 ~ var[5],
      v == 3 & .[[5]] > 0 ~ var[5],
      v == 4 & .[[5]] > 0 ~ var[5] ),
    by4 = case_when(
      v == 1 & .[[2]] > 0 ~ .[[2]],
      v == 1 & .[[3]] > 0 ~ .[[3]],
      v == 2 & .[[3]] > 0 ~ .[[3]],
      v == 1 & .[[4]] > 0 ~ .[[4]],
      v == 2 & .[[4]] > 0 ~ .[[4]],
      v == 3 & .[[4]] > 0 ~ .[[4]],
      v == 1 & .[[5]] > 0 ~ .[[5]],
      v == 2 & .[[5]] > 0 ~ .[[5]],
      v == 3 & .[[5]] > 0 ~ .[[5]],
      v == 4 & .[[5]] > 0 ~ .[[5]] )) %>%
     mutate(keep = ifelse(is.na(by3) & n==0, "no",
        "yes")) %>%
     filter(keep == "yes") %>%
     relocate(starts_with("by"), .after = v) %>%
     mutate(by3 = ifelse(is.na(by3), "oneway", by3),
      by4 = as.numeric(ifelse(by3 == "oneway", 0, by4)))
  temp <<- temp %>%
    dplyr::select(n, p, v, starts_with("by"))
  freq_table <<- temp %>%
      mutate(
       by5 = by4,
       by4 = case_when(
         by3 == "oneway" ~ 0,
         by3 == var[2] ~ 2,
         by3 == var[3] ~ 3,
         by3 == var[4] ~ 4,
         by3 == var[5] ~ 5 ),
       by2 = ifelse(by2 < 10,
        paste0(0, by2), by2),
       by5 = ifelse(by5 > 0 & by5 < 10,
        paste0(0, by5), by5),
       oneway = ifelse(by4 == 0, 1, 2),
       twdigits =
       ifelse(by5 == 0, paste0(v, by2),
         paste0(v, by2, by4, by5) ),
       twdigits = as.numeric(twdigits)) %>%
      arrange(oneway, v, by4, by2, by5 ) %>%
      mutate(seq = row_number()) %>%
      dplyr::select(seq, twdigits, everything()) %>%
      relocate(by5, .after = by4) %>%
      relocate(oneway, .before = v)
}

##H -------------------------------
## > fn_maxvar4_freq_table ----
##H -------------------------------

# 1. Declare a freq table function
fn_get_n_1 <- function(col_name) {
  t <<- table(df[, c(var[1], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 1)
}

fn_get_n_2 <- function(col_name) {
  t <<- table(df[, c(var[2], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 2)
}

fn_get_n_3 <- function(col_name) {
  t <<- table(df[, c(var[3], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 3)
}

fn_get_n_last4 <- function() {
  t <<- table(df[, c(var[4])])
  last <<- data.frame(
   prop.table(t), n = c(t), v = 4, last = 1)
  # rename
  names(last)[1] <<- var[4]
}

fn_maxvar4_freq_table  <- function() {
  df <<-  df %>%
        dplyr::select(any_of(var)) %>%
        mutate_at(var, factor)
  tw1  <<- lapply(names(df),            fn_get_n_1)
  tw2  <<- lapply(names(df[, -c(1)]),   fn_get_n_2)
  tw3  <<- lapply(names(df[, -c(1:2)]), fn_get_n_3)
  fn_get_n_last4()
  last; length(tw1) ; length(tw2) ;length(tw3)
  tw1[1] ; tw1[2] ; # tw1[4]
  tw2[3]
  # sum(tw1[[4]][4]) # geog1a : agecode1
  # sum(tw3[[2]][4]) # eth_code : econg
  temp <<- bind_rows(tw1, tw2, tw3, last)
  temp <<- as.data.frame(temp)
  row.names(temp) <<- 1:nrow(temp)
  temp <<- temp %>%
        dplyr::select(any_of(var),
         n, v, p = Freq, everything()) %>%
        mutate_at(var, as.numeric)
  # by2 copies the values of columns 1-4
  temp <<- temp %>%
   mutate(
    by1 = case_when(
      v == 1 ~ var[1],
      v == 2 ~ var[2],
      v == 3 ~ var[3],
      v == 4 ~ var[4]),
    by2 = case_when(
      v == 1 ~ .[[1]],
      v == 2 ~ .[[2]],
      v == 3 ~ .[[3]],
      v == 4 ~ .[[4]]),
    by3 = case_when(
      v == 1 & .[[2]] > 0 ~ var[2],
      v == 1 & .[[3]] > 0 ~ var[3],
      v == 2 & .[[3]] > 0 ~ var[3],
      v == 1 & .[[4]] > 0 ~ var[4],
      v == 2 & .[[4]] > 0 ~ var[4],
      v == 3 & .[[4]] > 0 ~ var[4]),
    by4 = case_when(
      v == 1 & .[[2]] > 0 ~ .[[2]],
      v == 1 & .[[3]] > 0 ~ .[[3]],
      v == 2 & .[[3]] > 0 ~ .[[3]],
      v == 1 & .[[4]] > 0 ~ .[[4]],
      v == 2 & .[[4]] > 0 ~ .[[4]],
      v == 3 & .[[4]] > 0 ~ .[[4]])
  ) %>%
     mutate(keep = ifelse(is.na(by3) & n==0, "no",
        "yes")) %>%
     filter(keep == "yes") %>%
     relocate(starts_with("by"), .after = v) %>%
     mutate(by3 = ifelse(is.na(by3), "oneway", by3),
      by4 = as.numeric(ifelse(by3 == "oneway", 0, by4)))
  temp <<- temp %>%
    dplyr::select(n, p, v, starts_with("by"))
  freq_table <<- temp %>%
      mutate(
       by5 = by4,
       by4 = case_when(
         by3 == "oneway" ~ 0,
         by3 == var[2] ~ 2,
         by3 == var[3] ~ 3,
         by3 == var[4] ~ 4),
       by2 = ifelse(by2 < 10,
        paste0(0, by2), by2),
       by5 = ifelse(by5 > 0 & by5 < 10,
        paste0(0, by5), by5),
       oneway = ifelse(by4 == 0, 1, 2),
       twdigits =
       ifelse(by5 == 0, paste0(v, by2),
         paste0(v, by2, by4, by5) ),
       twdigits = as.numeric(twdigits)) %>%
      arrange(oneway, v, by4, by2, by5 ) %>%
      mutate(seq = row_number()) %>%
      dplyr::select(seq, twdigits, everything()) %>%
      relocate(by5, .after = by4) %>%
      relocate(oneway, .before = v)
}

##H ------------------------------
##> fn_meanpop_auxiliary ----
##H ------------------------------

fn_meanpop_auxiliary <- function() {

  auxiliary  <- freq_table %>%
            filter(oneway == 1) %>%
            group_by(by1) %>%
            mutate(meanpop = wtsample_n/popsize) %>%
            ungroup()  %>%
            dplyr::select(seq,
              count = wtsample_n, by1,
              v, by2, meanpop)
  names(auxiliary)

  firstrow <- data.frame(seq = 0, count = popsize,
              by1 = "total", v = 0, by2 = "00",
              meanpop = 1)
  names(firstrow)  <- names(auxiliary)
  firstrow

  auxiliary <- bind_rows(firstrow, auxiliary)

  auxiliary <<- auxiliary %>%
                mutate(seq = row_number(),
                type = "wtsample")
}

##H ------------------------------
##> fn_add_cumfreq_decide4gr_n ----
##H ------------------------------

# percent, cumfreq in two decimal places
fn_add_cumfreq_decide4gr_n <- function() {
  decide4gr_n <<- decide4gr_n %>%
    mutate(cumfreq_ = cumsum(percent),
    cumfreq = ifelse(cumfreq_ <= 1,
      format(round(cumfreq_, 2), nsmall = 2), ""),
    percent = ifelse(cumfreq_ <= 1,
      format(round(percent, 2), nsmall = 2), "")) %>%
    dplyr::select(-cumfreq_)
}

#H----------------------------------------------
##>> Merge one admin + Weightedsample
##  freq table
#H----------------------------------------------

fn_merge_one_admin_wtsample_f_table_temp <- function() {

  Weightedsample_f_table <- read_excel ("Output/03-ExcelOutput/Weightedsample_freq_table.xlsx")

  # View(Master_Weightedsample_f_table) ;

  # Merge master and admin data frequency tables
  temp <- Weightedsample_f_table
  temp <- cbind(temp, Admin_f_table_one)

   names(temp) ; dim(temp);

  # Remove duplicates
  temp <- temp %>% subset(., select = which(!duplicated(names(.))))
  names(temp) ;  dim(temp);

  temp <<- temp %>%
    relocate(starts_with("by"), .after = last_col())
}

# >> TO BE REMOVED in public release. Simulation_x20
fn_merge_admin_x20_wtsample_f_table_temp <- function() {

  Weightedsample_f_table <- read_excel ("Output/03-ExcelOutput/Weightedsample_freq_table.xlsx")

  # View(Weightedsample_f_table) ;

  # Merge master and admin data frequency tables
  temp <- Weightedsample_f_table
  temp <- cbind(temp, Admin_f_table_wide_x20)

   names(temp) ; dim(temp);

  # Remove duplicates
  temp <- temp %>% subset(., select = which(!duplicated(names(.))))
  names(temp) ;  dim(temp);

  temp <<- temp %>%
    relocate(starts_with("by"), .after = last_col())
}

##>> stack wtsample
fn_stack_wtsample <- function() {

  Weightedsample_f_table <- read_excel ("Output/03-ExcelOutput/Weightedsample_freq_table.xlsx")

  View(Weightedsample_f_table) ;

  # Merge master and invp frequency tables
  temp <- Weightedsample_f_table %>%
      mutate(type = "wtsample", a = 0) %>%
      rename(n = wtsample_n, perc = wtsample_perc)
  temp <- bind_rows(temp, invp_maxvar5_f_table_stacked)

   names(temp) ; dim(temp);

  # Remove duplicates
  temp <- temp %>% subset(., select = which(!duplicated(names(.))))
  names(temp) ;  dim(temp);

  temp <- temp %>%
    relocate(starts_with("by"), .after = last_col())
  stack_wtsample <<- temp
}

fn_stack_part2_raw_admin <- function() {

  load("Output/04-RData/Admin_f_table_stacked.RData")

  # View(Admin_f_table_stacked) ;

  # Merge master and invp frequency tables
  temp <- Admin_f_table_stacked %>%
      mutate(type = "raw_admin")
  temp <- bind_rows(stack_wtsample, temp)

   names(temp) ; dim(temp);

  # Remove duplicates
  temp <- temp %>% subset(., select = which(!duplicated(names(.))))
  names(temp) ;  dim(temp);

  temp <- temp %>%
    relocate(starts_with("by"), .after = last_col())
  stack_part2_raw_admin <<- temp
}

fn_stack_part3_create_domain <- function() {
  temp  <- stack_part2_raw_admin
  temp$by1[temp$by1 == "geog1"]  <- "geog1a"
  temp$by1[temp$by3 == "geog1"]  <- "geog1a"
  domaintemp <- temp %>%
    mutate(
    temp_id = as.numeric(paste0(oneway, v, by4))
            ) %>%
    relocate(temp_id, .after = seq)

  temp <- domaintemp %>%
      group_by(temp_id) %>%
      mutate(
        domain_id = cur_group_id(),
        domain = paste0(
          ifelse(by3 == "oneway", by1,
            paste0(by1, ":", by3)))) %>%
      ungroup() %>%
      dplyr::select(seq, twdigits, domain_id,
        domain, temp_id,
        everything() )
  # New factor domain variable
  order  <- unique(temp$domain)
  order
  temp$fct_domain <- factor(temp$domain,
    levels = order)

  levels(temp$fct_domain)
  temp %>% tabyl(fct_domain)

  # Fix domain_id in wtsample. geog1 to geog1a
  # temp$fct_domain_old <- temp$fct_domain
  # temp$fct_domain <- str_replace(temp$fct_domain,
  #                   "geog1", "geog1a")
  temp$domain_id  <- as.factor(temp$domain_id)

  temp <- temp %>%
      relocate(fct_domain, .before = domain) %>%
      dplyr::select(-c(raw_n, temp_id))

  temp <- temp %>%
        group_by(type, a, domain_id) %>%
        mutate(p = n /sum(n),
        total = ifelse(is.na(total), sum(n), total)) %>%
        relocate(p, .after = n) %>%
        ungroup()
  stack_part3_create_domain <<- temp

}

# # ##H -------------------------------
# # ## > One-way frequency table ----
# # ##H -------------------------------

oneway_freq_table <- function(i) {
     addmargins(cbind(table(i)))
  }

oneway_freq_table_no_total <- function(i) {
  cbind(table(i))
}

fn_clean_oneway_no_total_temp <- function() {
  library(rlist)
  temp <<- as.data.frame(rlist::list.rbind(mylist))
  temp$cat  <<- row.names(temp)
  row.names(temp) <<- 1:nrow(temp)
  firstrow <- data.frame(V1 = 0,
            cat = "total")
  temp <<- rbind(firstrow, temp)
}

### End ###
