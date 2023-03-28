
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
##>> Summary Table using CreateTableOne ----
##H ----------------------------------------

fn_CreateTableOne_table <- function() {
  # prep for capture.output()
  txt_path_file <<- paste0(txtpath, txtfile)

  df_ <<- df %>%
    mutate_at(var, factor)
  glimpse(df)
library("tableone")
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

## ##H -------------------------------
## ## > One-way frequency table ----
## ##H -------------------------------

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

#H-----------------------------------
##>      R_INDICATORS   ----
#H-----------------------------------

##> Overall R-indicators Begin ----

#H-----------------------------------
##>> 1 fn_overall_r_indicator_1 ----
## design_matrix_with_weights
#H-----------------------------------

# Ensure popmean_row_vector is loaded first.
# dplyr:: added to avoid unused argument error

fn_prep_aa_freq <-  function() {
    variablenum <<- length(var) # No need to customise
    maxvar <<- length(var)      # No need to customise
    variablenum ; maxvar

    aa <<-  aa %>% dplyr::select(any_of(var)) %>%
              mutate_at(var, factor)
    # Declare macro variables
    (resppop <<-  nrow(aa))
    (rrate  <<-  nrow(aa)/popsize)
    # Check number of categories of each variable.
    # Run before fn_nvar().
    (levelsvar <<-  sapply(aa, levels))
    # Size of categories, last cat
    fn_nvar()
    # Obtain last category (to be dropped)
    # numcat = 35 (in SAS)..
    fn_lastcat()
    piinv   <<-  1.0
    aa <<-  aa %>%
          mutate(
            finalwgt = 1/rrate,
            pi = piinv,
            piinv  = 1/pi  #1.110273
          )
    # View(aa[1:20, ])
    head(aa) ; dim(aa)
    aa %>% tabyl(finalwgt)
    #H--------------------------------------
    ## dummy variables ----
    #H--------------------------------------
    library("fastDummies")
    # Intecept
    aa  <<-  aa %>% mutate(des1 = 1)

    # Allow a minute to execute.
    aa <<-  dummy_cols(aa, select_columns = var)
    names(aa)
    # Only keep dummy variables
    dummy_from_aa <<-  aa %>%
                    dplyr::select(-c(1:8))
    # Prepare dummy varnames
    # to use as column names of freq counts.
    dummy_col_names  <<-  names(dummy_from_aa)
    dummy_col_names
    #H--------------------------------------------
    ## Admin summary Freq Table + dummy variables
    #H--------------------------------------------
    # Prepare a one-way freq table object as well.
    # to merge later, domain_ used, seq removed
    mylist <<-  sapply(aa[, var], oneway_freq_table_no_total)
    fn_clean_oneway_no_total_temp()
    temp <<-  cbind(temp, dummy_col_names)
    temp$cat <<- NULL
            # dplyr::select(-cat) %>%
    freq <<-  temp %>%
            mutate(seq = row_number(),
            str = str_extract(dummy_col_names, "_\\d+"),
            n_cat = str_extract(str, "\\d+"),
            n_cat = ifelse(seq == 1, 1, n_cat))
    freq <<-  freq %>%
            mutate(
            first = ifelse(n_cat == 1, 1, 0),
            first = ifelse(seq == 1, 0, first),
            domain_n = cumsum(first)
            ) %>%
            dplyr::select(-c(str, first), count = V1) %>%
            relocate(seq, .before = count) %>%
            group_by(domain_n) %>%
            mutate(cum_count = max(cumsum(count))) %>%
            ungroup() %>%
            rename(domain_ = dummy_col_names)
    freq$seq <<- NULL
    # dummy_col_names
    # View(freq)
}

fn_r_indicator_1_vvv <-  function() {
    fn_prep_aa_freq()
  #H--------------------------------------------
  ## Completes design matrix with weights
  #H--------------------------------------------
  # Save the dataset as vvv
  vvv  <<- aa
  # Keep necessary dummy variables only.
  names(vvv) ;   dim(vvv)
  temp_remove  <<- c("pi", var, lastcat)
  vvv  <<- vvv %>%
        dplyr::select(- one_of(temp_remove))
  names(vvv) ; dim(vvv)

  # Define maximum numcat (eg. 37)
  temp_remove  <<- c("finalwgt", "piinv")
  numcat  <<- vvv %>%
             dplyr::select(- one_of(temp_remove))%>%
             length()
  numcat
  dim(vvv)
  head(vvv)

  # Rename as des variables
  colnames(vvv)  <<- c("finalwgt", "piinv",
                      paste0("des", 1:numcat))
  head(vvv)
  names(vvv)

  # Reorder, view design matrix
  vvv  <<- vvv %>%
        relocate(finalwgt, .after = last_col()) %>%
        relocate(piinv, .after = last_col())
  # View(vvv[1:100,])
  cat("--- End of design matrix with weights ---")
}

#H-----------------------------------
##>> 2 fn_overall_r_indicator_2 ----
#H-----------------------------------

fn_r_indicator_2_pop_respmean <- function() {

  #H--------------------------------------------
  ## admin data Distributions as row vectors ----
  #H--------------------------------------------

  # Admin data
    ls()
    popsize

  finalwgt  <<-  1/rrate
    table(finalwgt)

  # Treat des1(Intercept) separately. 0.999045879
  a  <<-  vvv %>% tabyl(des1) # unweighted=1047084
  a  <<- as.numeric(a[2]) # 1033664
  a
  aw  <<-  a * finalwgt
  aw
  respmean1  <<-  aw/popsize
  respmean1
  table(respmean1)
  # ttt generated to use as merging id.
  ttt  <<-  0
  other_rowvector <<-  cbind(respmean1, finalwgt, piinv, ttt)
  other_rowvector
  colnames(other_rowvector) <<- c("respmean1",
                      "finalwgt", "piinv", "ttt")
  names(other_rowvector)
  other_rowvector
# Drop other_rowvector from vvv, keep des2-des26
  temp_remove  <<- c("des1", "finalwgt", "piinv")
  des2_last <<- vvv %>%
        dplyr::select(- one_of(temp_remove))
  names(des2_last)  # des2-des26

# obtain weighted sample counts for des2-des26
  weighted_counts <<- function(i) {
      tab_a  <- table(i)
      tab_aw <- as.integer(tab_a[2] * finalwgt)
      tab_aw_p  <- tab_aw/popsize
      print(tab_aw_p)
    }
          # View(vvv[1:100, ])

  mylist <<- sapply(des2_last, weighted_counts)
  wtcount_from_des2 <<- as.data.frame(mylist)
  wtcount_from_des2
  # Transpose to arrange in row vector format.No need.
  temp <<- data.frame(t(wtcount_from_des2))
  # Rename variables "respmean",
  colnames(temp) <<- c(paste0("respmean", 2:numcat))
  wtcount_from_des2  <<- temp
  wtcount_from_des2
  # view(wtcount_from_des2)   # row vector
  other_rowvector
  temp <<- cbind(wtcount_from_des2, other_rowvector)
  temp <<- temp %>% relocate(respmean1, .before = respmean2)
  # SAVE
  respmean_row_vector <<- temp
  respmean_row_vector
  # Merge Step 1 out of 2
  pop_respmean <<-  full_join( popmean_row_vector,
                      respmean_row_vector) %>%
            relocate(ttt, .after = last_col())
  # View(pop_respmean)
  cat("--- End of pop_respmean ---")
}

#H-----------------------------------
##>> 3 fn_overall_r_indicator_3 ----
## des_pop_respmean
#H-----------------------------------

fn_r_indicator_3_des_pop_respmean <- function() {

  # des_pop_respmean corresponds to finalfile in SAS code.
  des_pop_respmean  <<- full_join(vvv, pop_respmean) %>%
                       mutate(seq = row_number(),
                              responsesamp1 = 1 )
  des_pop_respmean$ttt  <<- NULL
  dim(des_pop_respmean) ; names(des_pop_respmean)
  head(des_pop_respmean)
  # glimpse(des_pop_respmean)
  # seq is integer.
  # popmean1-popmean35 shouldn't be characters.
  popmean_col <<-  c(paste0("popmean", 1:numcat))
  des_pop_respmean[, popmean_col] <<-
   sapply(des_pop_respmean[, popmean_col], as.numeric)
  # Inspect
  # View(des_pop_respmean[1:20,])
  cat("--- End of des_pop_respmean ---")
}

# View(des_pop_respmean[1:20,])

#H-----------------------------------
##>> 4 fn_overall_r_indicator_4 ----
#H-----------------------------------

fn_r_indicator_4_gh <-  function() {
  #H--------------------------------------------
  ## G. Create diff_des_mean ----
  #H--------------------------------------------

  ls()
  # use df for programming.
  df <<-  data.frame(des_pop_respmean)
  dim(df) ; names(df)
  # glimpse(df)
  # Prep for loop.
  # We have same number of columns, and rows,
  # so just simply subtract df1 - df2.
  des_col      <<-  c(paste0("des"     , 1:numcat))
  respmean_col <<-  c(paste0("respmean", 1:numcat))
  popmean_col  <<-  c(paste0("popmean" , 1:numcat))
  des      <<-  df[, des_col]
  respmean <<-  df[, respmean_col]
  popmean  <<-  df[, popmean_col]
  rsam <<-  des - respmean
  psam <<-  des - popmean
  temp <<-  data.frame(rsam, psam)
  # Rename variables, seq is integer.
  colnames(temp)  <<-  c(paste0("rsam", 1:numcat),
                       paste0("psam", 1:numcat))
  # Combine, save as gh
  gh <<-  cbind(des_pop_respmean, temp)
  gh <<-  gh %>%
           relocate(c(responsesamp1, seq),
          .after = last_col() )
  glimpse(gh)
  names(gh) ; dim(gh) ; class(gh)
  cat("--- End of Pre-matrix ---")
}

#H-----------------------------------
##>> 5 fn_overall_r_indicator_5 ----
## compute R-indicators
#H-----------------------------------

fn_R_indicators <- function() {
  ls()
  resppop ; popsize; class(gh)
  # Prep :
  psam_col    <<-  c(paste0("psam", 1:numcat))
  rsam_col    <<-  c(paste0("rsam", 1:numcat))
  px          <<- as.matrix(gh[, psam_col])
  rx          <<- as.matrix(gh[, rsam_col])
  #H--------------------------------------
  ##>> Calculate propensity scores  ----
  #H--------------------------------------
  # element-wise multiplication (by scalar) : *
  # matrix multiplication(mathematical): %*%
  xxp  <<-   t(px) %*% px   # psam
  xxr  <<-   t(rx) %*% rx   # rsam
  # zp is row vector.
  popmean_col  <<- c(paste0("popmean", 1:numcat))
  zp  <<- as.matrix(gh[1, popmean_col])
  zzp <<- t(zp) %*% zp
  yyyp  <<- as.numeric(popsize/resppop) * xxp
  yyyr  <<- as.numeric(popsize/resppop) * xxr
  ttp   <<- as.numeric(popsize) * zzp
  gzpop <<-  yyyp + ttp
  gzmix <<-  yyyr + ttp
  # computes the Moore-Penrose Generalized
  # INVerse of matrix
  library("MASS")
  bbp   <<-  ginv(gzpop)
  bbmix <<-  ginv(gzmix)
  # Weights
  nf        <<- as.matrix(gh[, "responsesamp1"])
  weightf   <<- as.matrix(gh[, "piinv"])
  cnf     <<-  weightf * nf
  # Obtain the beta coefficients
  des_col  <<- c(paste0("des",  1:numcat))
  r        <<- as.matrix(gh[, des_col])
  ccw    <<-  t(r) %*% cnf
  ddpopw <<-  bbp   %*% ccw
  ddmix  <<-  bbmix %*% ccw
  # Obtain 2 kinds of propensity scores, column vector
  roipop <<-  r %*% ddpopw
  roimix <<-  r %*% ddmix
  # Save as user-friendly object names
  prop_pop  <<-  roipop
  prop_mix  <<-  roimix
  # If matrix errors are present, one may see below:
  # Error: cannot allocate vector of size xxxx.x Gb
  # gc() ; memory.limit()
  #H--------------------------------------
  ## I. Compute R-indicators ----
  #H--------------------------------------
  # Use data frame now. # roimix = prop_mix
  ee1b <<- as.data.frame(weightf * roimix)
  ee1c <<- as.data.frame(weightf * roipop)
  s2term11b <<- (1/popsize) * colSums(ee1b)
  s2term11c <<- (1/popsize) * colSums(ee1c)
  weightf <<- as.data.frame(weightf)
        class(weightf)
  s2term21  <<- (1/popsize) * colSums(weightf)
     s2term11b ; s2term11c ;   s2term21
  # variances:
  s2T11b    <<-  (popsize/(popsize-1)) *
                (s2term11b-(s2term21 * s2term21))
  s2T11c    <<-  (popsize/(popsize-1)) *
               (s2term11c-(s2term21 * s2term21))
  # R-indicators:
  r_ind1b <<-  1 - 2 * sqrt(s2T11b)
  r_ind1c <<-  1 - 2 * sqrt(s2T11c)
    r_ind1b; r_ind1c
  prop_mix_based_R_indicator <<- unname(r_ind1b)
  prop_pop_based_R_indicator <<- unname(r_ind1c)

  R_indicators  <<- c(unname(r_ind1b), unname(r_ind1c))
  R_indicators
  prop_pop_based_R_indicator
  prop_mix_based_R_indicator
  cat("--- End of Overall R-indicators ---")
}

#****************************************
#H---------------------------------
##> Partial R-indicators Begin ----
#H---------------------------------

#H---------------------------------
##>> fn_r_indicator_partial ----
#H---------------------------------
fn_R_indicators_partial <-  function() {
    library("dplyr")
    library("tidyr")
  fn_prep_aa_freq()
  aa  <<-  aa %>% mutate(ns = finalwgt,
              seq = row_number() )
  #H---------------------------------
  ##>>> 1. Prep prop_mix ----
  #H---------------------------------
  # View(prop) # column vector
  temp  <<-  as.data.frame(prop)
  temp$seq <<- seq.int(nrow(temp))
  head(temp) ; str(temp) ; names(temp)
  # roimix
  colnames(temp) <<- c("roi", "seq")
  prop <<- temp
  # in SAS, rindicmix or rindicpop, radd
  rindi  <<-  full_join(aa, prop)
  #H------------------------------------
  ##>>> 2. Sum, mean of finalwgt(ns) propens scores (roi)
  #H------------------------------------
  df <<-  rindi
  fn_stats_finalwgt_prop_score_none()
   temp <<-  bind_rows(a_list, aa_list, b_list, bb_list)
    temp  <<-  data.frame(t(temp))
    temp$seq <<- seq.int(nrow(temp))
  # Rename variables
  fbar_col    <<-  paste0("fbar",    1:variablenum)
  sum_r_col   <<-  paste0("sum_r",   1:variablenum)
  mean_wt_col <<-  paste0("mean_wt", 1:variablenum)
  mrphat_col  <<-  paste0("mrphat",  1:variablenum)
  colnames(temp) <<-  c(fbar_col, sum_r_col,
    mean_wt_col,  mrphat_col, "seq")
  names(temp) ; head(temp)
  # View(temp)
  # Prepare vectorised fbar_mrphat to merge later
  des1row <<- NA
  fbar  <<- data.frame(append(des1row, unlist(a_list)))
  mrphat <<- data.frame(append(des1row, unlist(bb_list)))
  fbar_mrphat <<- data.frame(dummy_col_names, fbar, mrphat)
  colnames(fbar_mrphat) <<- c("dummy_col_names",
    "fbar", "mrphat")
  fbar_mrphat$seq <<- seq.int(nrow(fbar_mrphat))
  # SAVE as new dataset (outputs only)
  rindicatorall  <<-  temp
  #H------------------------------------
  ##>>> 3. Overall mean of propensities ----
  #H------------------------------------
  mrphatall <<-  mean(df$roi)
  mrphatall       #  0.9597769
  temp  <<- data.frame(fbar_mrphat, mrphatall)
  # p2Zk = R_indicator, cvpsk=(abs(p2Zk))/mrphatall
  temp_cat  <<- temp %>%
    mutate(
      p2Zk = sqrt((fbar/popsize))*(mrphat-mrphatall),
      cvp2k = (abs(p2Zk))/mrphatall ) %>%
    relocate(p2Zk, cvp2k, .after = mrphat) %>%
    rename(domain = dummy_col_names)
  #H------------------------------------
  ##>>> 4. Calculations – cat level ----
  #H------------------------------------
  # Compute partial indicators – cat level
  df  <<-  rindicatorall
  # loop dataframe and column declared
  fbar   <<-  df[, fbar_col]
  mrphat <<-  df[, mrphat_col]
  # Compute R_indicator_category_level
  popsize  <<- as.numeric(popsize)
  fn_R_indicator_cat_level_temp()
  length(p2Zk_list)
  temp$seq <<- seq.int(nrow(temp))
  # Rename variables
  p2Zk_col <<-  paste0("p2Zk", 1:variablenum)
  p1Zk_col <<-  paste0("p1Zk", 1:variablenum)
  cvp2k_col <<-  paste0("cvp2k",1:variablenum)
  colnames(temp) <<-  c(p2Zk_col, p1Zk_col,
                     cvp2k_col, "seq")
  # Merge by seq
  df_    <<-  full_join(df, temp)
  right  <<-  df_
  #H---------------------------
  ##>>> 5. Calc – variable level ----
  #H---------------------------
  # Compute R_indicator_variable_level
  df          <<-  df_
  p1Zk_col    <<-  paste0("p1Zk", 1:variablenum)
  betweenvar  <<-  sapply(df[p1Zk_col], sum, na.rm = TRUE)
  sqrtbetween <<-  sqrt(betweenvar)
  cvpart      <<-  sqrt(betweenvar)/mrphatall
  temp    <<-   data.frame(var, nvar,
           betweenvar, sqrtbetween, cvpart)
  between     <<- temp
  r           <<- data.frame(R_indicator, resppop)
  colnames(r) <<- c("R_indicator", "resppop")
  between_    <<- data.frame(r, between)
  #H---------------------------
  ##>>> 6. Add var names, using macro, to right ----
  #H---------------------------
  var_col <<-  paste0("var_", 1:variablenum)
  v       <<-  as.data.frame(var, var_col)
  v       <<-  as.data.frame(t(v))
  v$seq   <<-  1
  right_  <<-  full_join(right, v, by = "seq")
  # var_1 fbar1 mrphat1 p2Zk1 .. var_2 fbar2 ...
  # p2Zk is cat-level (partial) R-indicator
  temp    <<-  c("var_", "fbar", "mrphat",
             "p2Zk", "p1Zk", "cvp2k")
  temp_   <<-  rep(1:variablenum, each = variablenum + 1)
  a       <<-  rep(paste0(temp,  temp_))
  # unnecessary columns will be dropped automatically
  right_var <<-  right_[ , c(a)]
  ## Merge between, and right_var ----
  temp  <<-  bind_rows(between_, right_var)
  # Type: 0 var level, 1: cat level.
  temp_  <<-   temp %>% rename(freq_ = nvar) %>%
           mutate(seq = row_number(),
            type = ifelse(is.na(freq_), 1, 0))
  temp_ <<- temp_ %>% relocate(type, .before = freq_) %>%
           relocate(resppop, .after = freq_)
  right  <<-  temp_
  ## p2Zk_cvp2k first. domain is non-numeric!!
  temp0   <<- c(R_indicator, mrphatall, resppop)
  p2Zk    <<- temp_cat$p2Zk
  p2Zk_   <<- append(betweenvar, p2Zk)
  p2Zk_   <<- append(temp0, p2Zk_)  # add 3 rows
  cvp2k   <<- temp_cat$cvp2k
  cvp2k_  <<- append(cvpart, cvp2k)
  cvp2k_  <<- append(temp0, cvp2k_)  # add 3 rows
  top <<- c("Overall", "mrphatall", "resppop")
  var_domain <<- append(var, dummy_col_names)
  var_domain_ <<- append(top, var_domain)
  var_domain_all <<- data.frame(var_domain_, p2Zk_, cvp2k_)
  var_domain_all$seq  <<- seq.int(nrow(var_domain_all))
  var_domain_all <<- var_domain_all %>% rename(domain_ = var_domain_)
   left    <<- full_join(var_domain_all, freq, by = "domain_")
 # Prepare a combined output table
  leftright  <<- full_join(left, right)
  leftright_ <<- data.frame(leftright, mrphatall)
  leftright_ <<- leftright_ %>%
       relocate(mrphatall, .after = R_indicator) %>%
       relocate(seq, .before = domain_)
  leftright_ <- leftright_ %>%
     rename(domain = domain_ ) %>%
     mutate(R_indicator_summary = p2Zk_) %>%
     relocate(R_indicator_summary, .before = p2Zk_)
 partial  <<- leftright_ %>%
  mutate(fct_domain = factor(domain, unique(domain))) %>%
  relocate(fct_domain, .after = domain)
  cat("-- End of fn_r_indicator_partial --")
}

# > R-indicator, Sub-functions
    #H------------------------------------
    ##>> Sum, mean of finalwgt (ns)
    ##     propensity scores (roimix) ----
    #H------------------------------------

    fn_stats_finalwgt_prop_score_none <- function() {

      a_list  <<- list()
      aa_list <<- list()
      b_list  <<- list()
      bb_list <<- list()

      for (i in 1:variablenum) {
        # Sum and mean of ns by var[i]
        # var[i] is the grouping var.
        # for freq table, sapply won't work. # without freq table
        # c <- lapply(df[, var[i]], table)
        element <- tapply(df$ns, df[, var[i]], sum)
        a_list[[length(a_list) + 1]] <<- element

        element_ <- tapply(df$roi, df[, var[i]], sum)
        aa_list[[length(aa_list) + 1]] <<- element_

        element <- tapply(df$ns, df[, var[i]], mean)
        b_list[[length(b_list) + 1]] <<- element

        element_ <- tapply(df$roi, df[, var[i]], mean)
        bb_list[[length(bb_list) + 1]] <<- element_
      }
    }

    #H------------------------------------
    ## R_indicator_cat_level_temp ----
    #H------------------------------------

    fn_R_indicator_cat_level_temp <- function() {
      p2Zk_list  <<- list()
      p1Zk_list  <<- list()
      cvp2k_list <<- list()
      for (i in 1:variablenum) {
       p2Zk <<- sqrt((fbar[i]/popsize))*(mrphat[i]-mrphatall)
       p2Zk_list[[length(p2Zk_list)+1]] <<- p2Zk

       element <<- p2Zk^2
       p1Zk_list[[length(p1Zk_list)+1]] <<- element

       element_ <<- (abs(p2Zk))/mrphatall
       cvp2k_list[[length(cvp2k_list)+1]] <<- element_
      }
      temp   <<- data.frame(p2Zk_list)
      temp_  <<- data.frame(p1Zk_list)
      temp_2 <<- data.frame(cvp2k_list)
      temp <<- data.frame(temp, temp_, temp_2)
    }

    #H------------------------------------
    ##  Calculations column order  ----
    #H------------------------------------

    fn_give_order_calc_temp <- function() {
      for (i in 1:variablenum) {
      col <- c(var[i],
      paste0("fbar",   i),
      paste0("mrphat", i),
      paste0("p2Zk",   i),
      paste0("p1Zk",   i),
      paste0("cvp2k",  i)
      )
      p  <<- rbind(col)
      print(p)
      p  <<- cbind(p)
      print(p)
      }
    }

    fn_col_order_calc <- function() {
      # temp[[1]]       # All lists
      temp <<- as.data.frame(temp)
      row.names(temp) <<- 1:nrow(temp)
      library("tidyverse")
      names(temp) <<- temp %>%
       slice(1) %>% unlist()
      names(temp)
      col_order_calc <<- temp
    }

### End ###