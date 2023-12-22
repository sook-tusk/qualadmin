
#===================================
##>  DEFINE OUTPUT FILE FOLDERS, PATH  ----
#===================================

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
  var <- names(df)
  df_ <- df %>%
    mutate_at(var, factor)
 library("tableone")
  a <- print(CreateTableOne(data = df_),
              showAllLevels = TRUE)
  capture.output(noquote(txtfile), noquote(a),
    file = txt_path_file)
  fn_txt_open()
}


##H -------------------------------
## > One-way frequency table ----
##H -------------------------------

oneway_freq_table <- function(i) {
     addmargins(cbind(table(i)))
  }

oneway_freq_table_no_total <- function(i) {
  cbind(table(i))
}

##H -------------------------------
## > Declare a freq table function ----
##H -------------------------------

fn_get_n_1 <- function(col_name) {
  t <- table(df[, c(var[1], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 1)
}

fn_get_n_2 <- function(col_name) {
  t <- table(df[, c(var[2], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 2)
}

fn_get_n_3 <- function(col_name) {
  t <- table(df[, c(var[3], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 3)
}

fn_get_n_4 <- function(col_name) {
  t <- table(df[, c(var[4], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 4)
}

fn_get_n_5 <- function(col_name) {
  t <- table(df[, c(var[5], col_name)])
  data.frame(
   prop.table(t), n = c(t), v = 5)
}

##H -------------------------------
## > last ----
##H -------------------------------

fn_get_n_last4 <- function() {
  t <- table(df[, c(var[4])])
  last4 <- data.frame(
   prop.table(t), n = c(t), v = 4, last = 1)
  # rename
  names(last4)[1] <- var[4]
  assign("last4", last4, .GlobalEnv)
  return(last4)
}


fn_get_n_last5 <- function() {
  t <- table(df[, c(var[5])])
  last5 <- data.frame(
   prop.table(t), n = c(t), v = 5, last = 1)
  # rename
  names(last5)[1] <- var[5]
  assign("last5", last5, .GlobalEnv)
  return(last5)
}


fn_get_n_last6 <- function() {
  t <- table(df[, c(var[6])])
  last6 <- data.frame(
   prop.table(t), n = c(t), v = 6, last = 1)
  # rename
  names(last6)[1] <- var[6]
  assign("last6", last6, .GlobalEnv)
  return(last6)
}

##H -------------------------------
## > fn_maxvar4_freq_table ----
##H -------------------------------

fn_maxvar4_freq_table  <- function() {
  var <-  names(df)
  assign("var", var, .GlobalEnv)
  df <-  df %>%
        dplyr::select(any_of(var)) %>%
        mutate_at(var, factor)
  tw1  <- lapply(names(df),            fn_get_n_1)
  tw2  <- lapply(names(df[, -c(1)]),   fn_get_n_2)
  tw3  <- lapply(names(df[, -c(1:2)]), fn_get_n_3)
  fn_get_n_last4()
  last  <- last4
  tw <- bind_rows(tw1, tw2, tw3, last)
  tw <- as.data.frame(tw)
  row.names(tw) <- 1:nrow(tw)
  tw <- tw %>%
        dplyr::select(any_of(var),
         n, v, p = Freq, everything()) %>%
        mutate_at(var, as.numeric)
  # by2 copies the values of columns 1-4
  tw <- tw %>%
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
  tw <- tw %>%
    dplyr::select(n, p, v, starts_with("by"))
  freq_table <- tw %>%
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
  assign("freq_table", freq_table, .GlobalEnv)
  head(freq_table)
}

  # Inspection
  # last4; length(tw1) ; length(tw2) ;length(tw3)
  # tw1[1] ; tw1[2] ; # tw1[4]
  # tw2[3]
  # sum(tw1[[4]][4]) # geog1a : agecode1
  # sum(tw3[[2]][4]) # eth_code : econg
##H -------------------------------
## > fn_maxvar5_freq_table ----
##H -------------------------------

fn_maxvar5_freq_table <- function() {
  # slow calculations for non-factored var.
  var <-  names(df)
  assign("var", var, .GlobalEnv)
  df   <-  df %>%
        dplyr::select(any_of(var)) %>%
        mutate_at(var, factor)
  tw1  <- lapply(names(df),            fn_get_n_1)
  tw2  <- lapply(names(df[, -c(1)]),   fn_get_n_2)
  tw3  <- lapply(names(df[, -c(1:2)]), fn_get_n_3)
  tw4  <- lapply(names(df[, -c(1:3)]), fn_get_n_4)
  fn_get_n_last5()
  last  <- last5
  tw <- bind_rows(tw1, tw2, tw3, tw4, last)
  tw <- as.data.frame(tw)
  row.names(tw) <- 1:nrow(tw)
  tw <- tw %>%
        dplyr::select(any_of(var),
         n, v, p = Freq, everything()) %>%
        mutate_at(var, as.numeric)
  # by2 copies the values of columns 1-4
  tw <- tw %>%
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
  tw <- tw %>%
    dplyr::select(n, p, v, starts_with("by"))
  freq_table <- tw %>%
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
  assign("freq_table", freq_table, .GlobalEnv)
  head(freq_table)
}

    # Inspection
    # last5; length(tw1) ; length(tw2) ;length(tw3)
    # tw1[1] ; tw1[2] ;  tw1[4]
    # tw2[3] ; # tw3[2] ; # tw4[2]
    # sum(tw1[[4]][4]) # geog1a : agecode1
    # sum(tw4[[2]][4]) # eth_code : econg

##H -------------------------------
## > fn_maxvar6_freq_table ----
##H -------------------------------

fn_maxvar6_freq_table <- function() {
  # slow calculations for non-factored var.
  var <-  names(df)
  assign("var", var, .GlobalEnv)
  df   <-  df %>%
        dplyr::select(any_of(var)) %>%
        mutate_at(var, factor)
  tw1  <- lapply(names(df),            fn_get_n_1)
  tw2  <- lapply(names(df[, -c(1)]),   fn_get_n_2)
  tw3  <- lapply(names(df[, -c(1:2)]), fn_get_n_3)
  tw4  <- lapply(names(df[, -c(1:3)]), fn_get_n_4)
  tw5  <- lapply(names(df[, -c(1:4)]), fn_get_n_5)
  fn_get_n_last6()
  last  <- last6
  tw <- bind_rows(tw1, tw2, tw3, tw4, tw5, last)
  tw <- as.data.frame(tw)
  row.names(tw) <- 1:nrow(tw)
  tw <- tw %>%
        dplyr::select(any_of(var),
         n, v, p = Freq, everything()) %>%
        mutate_at(var, as.numeric)
  # by2 copies the values of columns 1-4
  tw <- tw %>%
   mutate(
    by1 = case_when(
      v == 1 ~ var[1],
      v == 2 ~ var[2],
      v == 3 ~ var[3],
      v == 4 ~ var[4],
      v == 5 ~ var[5],
      v == 6 ~ var[6] ),
    by2 = case_when(
      v == 1 ~ .[[1]],
      v == 2 ~ .[[2]],
      v == 3 ~ .[[3]],
      v == 4 ~ .[[4]],
      v == 5 ~ .[[5]],
      v == 6 ~ .[[6]] ),
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
      v == 4 & .[[5]] > 0 ~ var[5],
      v == 1 & .[[6]] > 0 ~ var[6],
      v == 2 & .[[6]] > 0 ~ var[6],
      v == 3 & .[[6]] > 0 ~ var[6],
      v == 4 & .[[6]] > 0 ~ var[6],
      v == 5 & .[[6]] > 0 ~ var[6] ),
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
      v == 4 & .[[5]] > 0 ~ .[[5]],
      v == 1 & .[[6]] > 0 ~ .[[6]],
      v == 2 & .[[6]] > 0 ~ .[[6]],
      v == 3 & .[[6]] > 0 ~ .[[6]],
      v == 4 & .[[6]] > 0 ~ .[[6]],
      v == 5 & .[[5]] > 0 ~ .[[6]] )) %>%
     mutate(keep = ifelse(is.na(by3) & n==0, "no",
        "yes")) %>%
     filter(keep == "yes") %>%
     relocate(starts_with("by"), .after = v) %>%
     mutate(by3 = ifelse(is.na(by3), "oneway", by3),
      by4 = as.numeric(ifelse(by3 == "oneway", 0, by4)))
  tw <- tw %>%
    dplyr::select(n, p, v, starts_with("by"))
  freq_table <- tw %>%
      mutate(
       by5 = by4,
       by4 = case_when(
         by3 == "oneway" ~ 0,
         by3 == var[2] ~ 2,
         by3 == var[3] ~ 3,
         by3 == var[4] ~ 4,
         by3 == var[5] ~ 5,
         by3 == var[6] ~ 6 ),
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
  assign("freq_table", freq_table, .GlobalEnv)
  head(freq_table)
}


#===================================
## > Compute distance_metrics
#===================================

fn_distance_metrics <- function() {
  #H--------------------------------------
  ##  Create DOMAINS ----
  #H--------------------------------------
  domaintemp <- freq_table2 %>%
    mutate(
    temp_id = as.numeric(paste0(oneway, v, by4))
            ) %>%
    relocate(temp_id, .after = seq)
  domaintemp <- domaintemp %>%
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
  # factor domain variable
  order  <- unique(domaintemp$domain) ; order
  domaintemp$fct_domain <- factor(domaintemp$domain,
    levels = order)
    levels(domaintemp$fct_domain)
  display_domain <- domaintemp %>%
      relocate(fct_domain, .after = domain)
      # for user manual, display_domain
      display_domain %>% tabyl(fct_domain)
  ##H -----------------------------------
  ##   Compute unstd_distance_metrics ----
  ##H -----------------------------------
  domain_ <- display_domain %>%
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
  unstd_distance_metrics_full <- domain_ %>%
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
  unstd_tidy <- unstd_distance_metrics_full %>%
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
  distance_metrics <- unstd_tidy %>%
    mutate(
     Std_Duncan = 1 - Duncan,
     Std_HD   = 1 - HD,
     Std_KL   = 1 - KL) %>%
    relocate(starts_with("Std"), .after = fct_domain)
  assign("distance_metrics", distance_metrics, .GlobalEnv)
  print(distance_metrics)
}

##H ----------------------------------------
### > Reshape, distance_metrics_long ----
##H ----------------------------------------

fn_distance_metrics_long <- function() {
  # Reshape, gather columns except the first four.
  # key=stub, create 2 new vars!
  # Removed , factor_key=TRUE
  # to allow flexibility in graphics.
  wide_ <- distance_metrics %>%
      dplyr::select(-entropy_p) %>%
      gather(key = "indicator",
          value = "index", -(1:3)) %>%
      arrange(domain_id, indicator)

# ref1 = A if the indicator starts with A(^A)
# ref2 = 1 if the indicator ends with x(x$)
  long_ <- wide_ %>%
      mutate(
        std =
        ifelse(grepl("^Std", indicator),
          1, 0),
        std_test_temp =
        ifelse(grepl("^Std_", indicator),
          1, 0),
        ref =
        ifelse(grepl("Duncan", indicator), 1,
        ifelse(grepl("HD", indicator), 2, 3) ),
        std_test_use = ifelse(std == 1 &
          (ref == 1 | std_test_temp == 1), 1, 0)) %>%
      mutate_at(vars(std, ref), factor) %>%
      arrange(domain_id, std_test_use, indicator)

  distance_metrics_long <- long_ %>%
     filter(std_test_use == 1) %>%
     dplyr::select(-starts_with("std"))
  assign("long_", long_, .GlobalEnv)
  assign("distance_metrics_long", distance_metrics_long, .GlobalEnv)
  return(distance_metrics_long)
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

    auxiliary <- auxiliary %>%
                  mutate(seq = row_number(),
                  type = "wtsample")
    assign("auxiliary", auxiliary, .GlobalEnv)
    return(auxiliary)
}


## > ===============================
#===================================
##>  TABLE-BASED R_INDICATOR   ----
#===================================
fn_RUN_table_based_R_indicator <- function() {
     fn_t_design_matrix()
     fn_freq()
     fn_t_respmean()
     fn_des_pop_respmean()
     fn_gh ()
     fn_t_R_indicator()
     fn_t_rindicatorall()
     fn_partial()
}

#=========================================
# > 1 fn_t_design_matrix
#=========================================

fn_t_design_matrix <- function() {
  #H-----------------------------------
  ##  1. lastcat   ----
  #H-----------------------------------
    var  <- names(aa) ; var
    variablenum <- length(var)
    maxvar      <- length(var)
    aa <- aa %>% dplyr::select(any_of(var)) %>%
               mutate_at(var, factor)
    levelsvar <- sapply(aa, levels) ; levelsvar
   # Size of categories, last cat
    nvar    <-  NULL
    lastcat <-  NULL
    for (i in 1:maxvar) {
     nvar[i] <- length(levelsvar[[i]])
     lastcat[i] <- print(paste0(var[i], "_", nvar[i]))
    }
  #H-----------------------------------
  ##  2. Produce a pp table ----
  #H-----------------------------------
   # 2520 = 6*2*14*5*3
   gridrow  <- prod(nvar); gridrow
   produpto <- maxvar - 1  ; produpto
    # set each
   prod_list  <- list()
    for (i in 2:maxvar) {
      element <- prod(nvar[i:maxvar])
      prod_list[[length(prod_list) + 1]] <- element
    }
   lastgrid <- rep(1:nvar[maxvar], each = 1,
                   length.out = gridrow)
      # Remaining grid
      grid_list  <- list()
      for (i in 1:produpto) {
        element <- rep(seq(1:nvar[i]), each = prod_list[[i]],
                    length.out = gridrow)
        grid_list[[length(grid_list) + 1]] <- element
      }
  # Combine all
  grid <- data.frame(rlist::list.cbind(grid_list))
  grid <- cbind(grid, lastgrid)
  names(grid) <- var
  grid <- grid %>% dplyr::select(any_of(var)) %>%
            mutate_at(var, factor)
  pp <- grid %>%
        mutate(pp_seq = row_number()) %>%
        dplyr::select(pp_seq, everything() )
  # print(pp[1:30, ])
  #H-----------------------------------
  ##  3. weights ----
  #H-----------------------------------
  ##  Create the global weight  ----
  (resppop <- nrow(aa))
  (rrate  <- nrow(aa)/popsize)
  aa_ <-  aa %>%
            mutate(seq = row_number(),
               finalwgt = 1/rrate)
  ##  Merge ----
  # Recall  nrow(aa) = 1033664
  ppaa  <- aa_ %>% dplyr::full_join(pp)
  ppaa  <- ppaa %>% arrange(seq)
    nrow(ppaa)            # 1034120
  #  Adding non-zero weights
  # Using non-missing data, obtain weights.
  # i.e., adjusted aggregate inv weights
  matched <- ppaa %>% filter(!is.na(seq))
  matched_ <- matched %>% tabyl(pp_seq)
  matched_$percent <- NULL   # sum(weights$n)
  finalwgt_global <- 1/rrate ; finalwgt_global
  matched_wgt  <- cbind(matched_, finalwgt_global)
  matched_wgt  <- matched_wgt %>%
    mutate(wgt_nnn = n * finalwgt_global) %>%
    rename(nnn = n) %>%
    mutate(piinv = nnn)
  head(matched_wgt)
  #  Treat 456 missing comb cases with 0 weights
  # Fill 0 with nnn, piinv, finalwgt
  temp_remove <- c(var, "seq", "finalwgt")
  mi <- ppaa %>% filter(is.na(seq)) %>%
     dplyr::select(-one_of(temp_remove)) %>%
       mutate(nnn = 0,
        finalwgt_global = finalwgt_global,
        wgt_nnn = 0,  piinv = 0)
  # Check the total combinations: 6*2*14*16=2688
    nrow(matched_wgt) + nrow(mi)
  # pp table (Combination) with weights
  matched_wgtmi <- bind_rows(matched_wgt, mi)
  pall <- pp %>% full_join(matched_wgtmi)
  pall <- pall %>%
          relocate(pp_seq, .before = nnn)
        # for user manual
        missing  <- nrow(mi); missing
        ppaafrom <- nrow(ppaa) - missing - 4
        ppaato   <- ppaafrom + 10
  # print(ppaa[ppaafrom: ppaato, ])
  #H--------------------------------------
  ## 4. pp_design_matrix  ----
  #H--------------------------------------
  # Check number of categories of each variable.
  # Size of categories, last cat (to be dropped)
  deleteupto <- ncol(pall) ;  deleteupto
  pall  <- pall %>% mutate(des1 = 1)
  library("fastDummies")
  pall <- dummy_cols(pall, select_columns = var)
  temp_remove   <- c(lastcat, "nnn",
      "pp_seq",  "finalwgt_global")
  design_matrix_temp <- pall %>%
   dplyr::select(- one_of(temp_remove)) %>%
   relocate(c(wgt_nnn, piinv), .after = last_col())
  tail(design_matrix_temp, n = 3)
  # Define maximum numcat (eg. 35)
  extra  <- c("wgt_nnn", "piinv") # "piinv = nnn",
  numcat <- ncol(design_matrix_temp)-length(extra)-maxvar
  # Rename design matrix columns as des variables
  # to be merged with pop_respmean, mergingid=ttt
  design_matrix <- design_matrix_temp
  colnames(design_matrix) <-
    c(var, paste0("des", 1:numcat),
      "wgt_nnn", "piinv")  # "nnn",
  design_matrix <- cbind(design_matrix, ttt = 0)
  tail(design_matrix, n = 3)
    assign("resppop", resppop, .GlobalEnv)
    assign("rrate", rrate, .GlobalEnv)
    assign("var", var, .GlobalEnv)
    assign("maxvar", maxvar, .GlobalEnv)
    assign("lastcat", lastcat, .GlobalEnv)
    assign("nvar", nvar, .GlobalEnv)
    assign("finalwgt_global", finalwgt_global, .GlobalEnv)
    assign("deleteupto", deleteupto, .GlobalEnv)
    assign("pall", pall, .GlobalEnv)
    assign("design_matrix", design_matrix, .GlobalEnv)
  cat("--- End of fn_t_design_matrix ---")
}

#=========================================
##> 2 fn_t_respmean ----
#=========================================
fn_t_respmean <-  function() {
  #H--------------------------------------------
  ## respmean using admin data  ----
  ## admin data Distributions as row vectors
  #H--------------------------------------------
# Need dummies to compute respmean
  library("fastDummies")
  aa_designmat <- dummy_cols(aa, select_columns = var)
  aa_designmat <- aa_designmat[, -c(1:maxvar)]
  des2_last <- aa_designmat %>%
        dplyr::select(- one_of(lastcat))
# obtain weighted sample counts/prop for des2-des26
    table_weighted_means <- function(i) {
        tab_a  <- table(i)
        tab_aw <- as.integer(tab_a[2] * finalwgt_global)
        tab_aw_p  <- tab_aw/popsize
        return(tab_aw_p)
      }
  respmn_list <- sapply(des2_last,
                   table_weighted_means)
  respmn_list_from_des2 <- as.data.frame(respmn_list)
  respmn_list_from_des2
  # Transpose to arrange in row vector format.
  respmean <- data.frame(t(respmn_list_from_des2))
  # Rename variables "respmean", mergingid = ttt
  colnames(respmean) <- c(paste0("respmean", 2:numcat))
  respmean <- cbind(ttt = 0,
    respmean1 = 1, respmean)
  assign("respmn_list_from_des2", respmn_list_from_des2, .GlobalEnv)
  assign("respmean", respmean, .GlobalEnv)
  return(respmean)
  cat("--- End of fn_t_respmean ---")
}


#=========================================
## > 5 fn_t_R_indicator ----
## Using Matrix, compute R-indicators
#=========================================
fn_t_R_indicator <-  function() {
  psam_col    <- c(paste0("psam", 1:numcat))
  rsam_col    <- c(paste0("rsam", 1:numcat))
  px          <- as.matrix(gh[, psam_col])
  rx          <- as.matrix(gh[, rsam_col])
  #H-----------------------------------
  ## H. Calculate propensity scores  ----
  #H-----------------------------------
  gh   <- gh %>% mutate(gg = sqrt(piinv))
    # head(gh[, c("seq", "gg", "piinv",
    #        "wgt_nnn")])
  gg   <- gh[, "gg"]         # Not matrix
  pxm  <-  px * gg           # weighted_psam
  xxpm <-  t(pxm) %*% pxm
  rxm  <-  rx * gg           # weighted_rsam
  xxrm <-  t(rxm) %*% rxm
  popmean_col  <- c(paste0("popmean", 1:numcat))
  zp  <- as.matrix(gh[1, popmean_col])
  zzp <-  t(zp) %*% zp   # popmean1-popmean26
  1 / (resppop/popsize) ; popsize/resppop
  # yyyp = finalwgt X xxpm(crossproduct of weighted_psam)
  yyyp  <- as.numeric(popsize/resppop) * xxpm
  yyyr  <- as.numeric(popsize/resppop) * xxrm
  ttp   <-  as.numeric(popsize) * zzp # zzp=popmean crossproduct
  # yyyp = finalwgt X xxp(crossproduct of psam)
  # ttp  = popsize  X zzp(crossproduct of popmean)
  gzpop <- yyyp + ttp  # to get beta coefficients
  gzmix <- yyyr + ttp  # to get beta coefficients
  # computes the Moore-Penrose Generalized
  # INVerse of matrix
  library("MASS")
  bbp   <- ginv(gzpop)  # to get beta coefficients
  bbmix <- ginv(gzmix)  # to get beta coefficients
  ########### weights #####################
  nf <- as.matrix(gh[, "responsesamp1"])
  weightf   <- as.matrix(gh[, "piinv"])
  # Treat weightf, nf as matrix, not column vectors
  cnf    <- weightf * nf
 ##### Obtain the beta coefficients #######
  des_col     <- c(paste0("des",  1:numcat))
  r           <- as.matrix(gh[, des_col])
  ccw    <- t(r) %*% cnf  # Not element-wise
 # Obtain the beta coefficients
  ddpopw <- bbp   %*% ccw
  ddmix  <- bbmix %*% ccw
  # Get propensity scores, column vector
  roipop <- r %*% ddpopw
  roimix <- r %*% ddmix
  # Save as user-friendly object names
  prop_pop  <- roipop
  prop_mix  <- roimix
  #H---------------------------------
  ## add weighted prop score ----
  #H---------------------------------
  # to be used in partial R-indicators
  # rindicmix is gh_prop_mix
  gh_prop_mix  <- cbind(gh, prop_mix) %>%
     mutate(roi = prop_mix) %>%
     mutate(roi_wgt = roi * wgt_nnn) %>%
     relocate(wgt_nnn, piinv, .after = last_col())
  #H--------------------------------------
  ## I. Compute R-indicators ----
  #H--------------------------------------
  # use propensity weighting to adjust for coverage bias
  ee1b <- as.data.frame(weightf * roimix)
  ee1c <- as.data.frame(weightf * roipop)
  s2term11b <- (1/popsize) * colSums(ee1b)
  s2term11c <- (1/popsize) * colSums(ee1c)
  weightf  <- as.data.frame(weightf)
  s2term21 <- (1/popsize) * colSums(weightf)
  # variances
  var_s2T11b    <- (popsize/(popsize-1)) *
                (s2term11b-(s2term21 * s2term21))
  var_s2T11c    <- (popsize/(popsize-1)) *
               (s2term11c-(s2term21 * s2term21))
  # R-indicators: 1 - 2SD = 1 - 2SD of propensity scores
  r_ind1b <- 1 - 2 * sqrt(var_s2T11b) # prop_mix-based
  r_ind1c <- 1 - 2 * sqrt(var_s2T11c) # prop_pop-based
  prop_mix_based_R_indicator <- unname(r_ind1b)
  prop_pop_based_R_indicator <- unname(r_ind1c)
  # Inspect propensity scores, Export
  R_indicator <- prop_mix_based_R_indicator
  assign("R_indicator", R_indicator, .GlobalEnv)
  assign("gh_prop_mix", gh_prop_mix, .GlobalEnv)
  cat("--- End of fn_t_R_indicator ---")
}

#=========================================
## > 6 fn_t_rindicatorall ----
#=========================================
fn_t_rindicatorall <- function() {
    #H------------------------------------
    ## weighted propensity scores ----
    #H------------------------------------
          df  <- gh_prop_mix
          ws_list <- list()
          wr_list <- list()
          w_list  <- list()
      for (i in 1:maxvar) {
        # var[i] is the grouping var.
        element <- tapply(df$wgt_nnn, df[, var[i]], sum)
        ws_list[[length(ws_list) + 1]] <- element
        # Use weighted roi = roi_wgt(roi * wgt_nnn)
        element_ <- tapply(df$roi_wgt, df[, var[i]], sum)
        wr_list[[length(wr_list) + 1]] <- element_
      }
      for (i in 1:maxvar) {
        # Obtain mean: divide by ws_list
        element <- wr_list[[i]] / ws_list[[i]]
        w_list[[length(w_list) + 1]] <- element
      }
      rindicatorall   <-  bind_rows(ws_list, w_list)
      rindicatorall   <-  data.frame(t(rindicatorall))
      rindicatorall$seq <- seq.int(nrow(rindicatorall))
      fbar_col   <- paste0("fbar", 1:maxvar)
      mrphat_col <- paste0("mrphat",  1:maxvar)
      colnames(rindicatorall) <- c(fbar_col,
                       mrphat_col, "seq")
      fbar   <- rindicatorall[, fbar_col]
      mrphat <- rindicatorall[, mrphat_col]
      # Prepare vectorised fbar_mrphat to merge later
      des1row <- NA
      fbarc  <- data.frame(append(des1row, unlist(ws_list)))
      mrphatc <- data.frame(append(des1row, unlist(w_list)))
      fbar_mrphat <- data.frame(dummy_col_names,
        level = "category-level", fbarc, mrphatc)
      colnames(fbar_mrphat) <- c("dummy_col_names",
        "level", "fbar", "mrphat")
      fbar_mrphat$seq <- seq.int(nrow(fbar_mrphat))

      #H------------------------------------
      ## 2. Overall mean of propensities ----
      #H------------------------------------
      df <- gh_prop_mix
      mrphatall <- sum(df$roi_wgt) / sum(df$wgt_nnn)
      # left of the report, by dummy, fbar_mrphat_
      fbar_mrphat_  <- data.frame(fbar_mrphat, mrphatall)
      # p2Zk = R_indicator, cvpsk=(abs(p2Zk))/mrphatall
      fbar_mrphat_  <- fbar_mrphat_ %>%
        mutate(
          p2Zk = sqrt((fbar/popsize))*(mrphat-mrphatall),
          cvp2k = (abs(p2Zk))/mrphatall ) %>%
        relocate(p2Zk, cvp2k, .after = mrphat) %>%
        rename(domain = dummy_col_names)
      assign("rindicatorall", rindicatorall, .GlobalEnv)
      assign("fbar_mrphat_", fbar_mrphat_, .GlobalEnv)
      assign("fbar",   fbar, .GlobalEnv)
      assign("mrphat", mrphat, .GlobalEnv)
      assign("mrphatall", mrphatall, .GlobalEnv)
      return(rindicatorall)
  cat("--- End of fn_t_rindicatorall ---")
}


## > ===============================
#===================================
##>      R_INDICATORS   ----
#===================================
fn_RUN_R_indicator <- function() {
    fn_design_matrix()
    fn_freq()
    fn_respmean()
    fn_des_pop_respmean()
    fn_gh()
    fn_R_indicator()
    fn_rindicatorall()
    fn_partial()
}

#H-----------------------------------
##>> 1 fn_design_matrix ----
#H-----------------------------------
# Ensure popmean_row_vector is loaded first.
# dplyr:: added to avoid unused argument error

fn_design_matrix <-  function() {
    #H-----------------------------------
    ##  1. lastcat   ----
    #H-----------------------------------
    var  <- names(aa) ; var
    variablenum <- length(var)
    maxvar      <- length(var)
    aa <-  aa %>% dplyr::select(any_of(var)) %>%
              mutate_at(var, factor)
    levelsvar <- sapply(aa, levels) ; levelsvar
   # Size of categories, last cat
    nvar    <-  NULL
    lastcat <-  NULL
    for (i in 1:maxvar) {
     nvar[i] <- length(levelsvar[[i]])
     lastcat[i] <- print(paste0(var[i], "_", nvar[i]))
    }
    #H-----------------------------------
    ##  2. weights   ----
    #H-----------------------------------
    # Declare macro variables
    (resppop <-  nrow(aa))
    (rrate   <-  nrow(aa)/popsize)
    finalwgt <-  1/rrate
    piinv    <-  1.0
    aa <-  aa %>%
          mutate(
            finalwgt = 1/rrate,
            pi = piinv,
            piinv  = 1/pi  #1.110273
          )
    aa %>% tabyl(finalwgt) # 1.125753 1033664
    # View(aa[1:20, ])
    #H-----------------------------------
    ## 3. design_matrix ----
    #H-----------------------------------
    deleteupto <- ncol(aa) ;  deleteupto
    # Intercept
    aa  <-  aa %>% mutate(des1 = 1)
    # Allow a minute to execute.
    library("fastDummies")
    aa_ <-  dummy_cols(aa, select_columns = var)
    # Keep necessary dummy variables only.
    temp_remove  <- c(lastcat, "pi")
    vvv  <- aa_ %>%
          dplyr::select(- one_of(temp_remove)) %>%
          relocate(finalwgt, piinv, .after = last_col() )
      names(vvv) ; dim(vvv)
    # Define maximum numcat (eg. 35)
    extra  <- c("finalwgt", "piinv")
    numcat <- ncol(vvv)-length(extra)-maxvar ; numcat
    # Rename as des variables
    # to be merged with pop_respmean, mergingid=ttt
    colnames(vvv)  <- c(var, paste0("des", 1:numcat),
                       "finalwgt", "piinv")
    vvv <- cbind(vvv, ttt = 0)
    design_matrix  <- vvv
    # View(vvv[1:100,])
    # to use in fn_prep_aa_freq()
    pall  <- aa_
    assign("resppop", resppop, .GlobalEnv)
    assign("rrate", rrate, .GlobalEnv)
    assign("var",     var, .GlobalEnv)
    assign("maxvar",  maxvar, .GlobalEnv)
    assign("lastcat", lastcat, .GlobalEnv)
    assign("nvar",    nvar, .GlobalEnv)
    assign("finalwgt", finalwgt, .GlobalEnv)
    assign("deleteupto", deleteupto, .GlobalEnv)
    assign("pall",    pall, .GlobalEnv)
    assign("design_matrix", design_matrix, .GlobalEnv)
    assign("vvv",     vvv, .GlobalEnv)
    cat("--- End of fn_design_matrix ---")
}


#=========================================
# >> 1b fn_freq (both microdata and tablebased)
#=========================================
# freq object to be used in Admin Summary

fn_freq <- function() {
  # Prepare a one-way freq table object as well.
    dummy_col_names <- pall[, -c(1:deleteupto)]
    # to use as column names of freq counts.
    dummy_col_names <- names(dummy_col_names)
    mylist <- sapply(aa[, var], oneway_freq_table_no_total)
    freq <- as.data.frame(rlist::list.rbind(mylist))
    freq$cat  <- row.names(freq)
    row.names(freq) <- 1:nrow(freq)
    firstrow <- data.frame(V1 = 0,
            cat = "total")
    freq <- rbind(firstrow, freq)
    freq <- cbind(freq, dummy_col_names)
    freq$cat <- NULL
    freq <- freq %>%
            mutate(seq = row_number(),
            str = str_extract(dummy_col_names, "_\\d+"),
            n_cat = str_extract(str, "\\d+"),
            n_cat = ifelse(seq == 1, 1, n_cat))
    freq <- freq %>%
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
    freq$seq <- NULL
    assign("freq", freq, .GlobalEnv)
    assign("dummy_col_names", dummy_col_names, .GlobalEnv)
    return(freq)
  cat("--- End of fn_freq ---")
}

#H-----------------------------------
##>> 2 fn_respmean ----
#H-----------------------------------

fn_respmean <- function() {
  #H--------------------------------------------
  ## respmean using admin data  ----
  ## admin data Distributions as row vectors
  #H--------------------------------------------
  # vvv is design matrix.
  # Need dummies to compute respmean
  vvv$des1 <- NULL
  des <- vvv %>%
     dplyr::select(starts_with("des"))
  names(des)  # des2-des26

# obtain weighted sample prop. for des2-des26
  weighted_means <- function(i) {
      tab_a  <- table(i)
      tab_aw <- as.integer(tab_a[2] * finalwgt)
      tab_aw_p <- tab_aw/popsize
      return(tab_aw_p)
    }
  respmn_list  <- sapply(des, weighted_means)
  respmean_column <- as.data.frame(respmn_list)
  respmean_column
  # Transpose to arrange in row vector format.
  respmean <- data.frame(t(respmean_column))
  # Rename variables "respmean",
  colnames(respmean) <- c(paste0("respmean", 2:numcat))
  respmean <- cbind(ttt = 0,
    respmean1 = 1, respmean)
  assign("respmean_column", respmean_column, .GlobalEnv)
  assign("respmean", respmean, .GlobalEnv)
  return(respmean)
  cat("--- End of fn_respmean ---")
}

#H-----------------------------------
##>> 3 fn_des_pop_respmean (both microdata and tablebased) ----
#H-----------------------------------
fn_des_pop_respmean <- function() {
  # Merge Step 1 out of 2
  pop_respmean <-  full_join(popmean_row_vector,
                      respmean) %>%
            relocate(ttt, .after = last_col())
  # Merge Step 2 out of 2
  des_pop_respmean  <-
    full_join(design_matrix, pop_respmean) %>%
    mutate(seq = row_number(), responsesamp1 = 1)
  des_pop_respmean$ttt  <- NULL
  # popmean1-popmean35 should be integer.
  popmean_col <-  c(paste0("popmean", 1:numcat))
  des_pop_respmean[, popmean_col] <-
   sapply(des_pop_respmean[, popmean_col], as.numeric)
  # Inspect
  names(des_pop_respmean)
  assign("des_pop_respmean", des_pop_respmean, .GlobalEnv)
  cat("--- End of fn_des_pop_respmean ---")
}

#H-----------------------------------
##>> 4 fn_gh (both microdata and tablebased) ----
#H-----------------------------------
fn_gh <-  function() {
  #H--------------------------------------------
  ## G. Create diff_des_mean ----
  #H--------------------------------------------
  ls()
  # use df for programming.
  df <-  data.frame(des_pop_respmean)
  # Prep for loop.
  des_col      <-  c(paste0("des"     , 1:numcat))
  respmean_col <-  c(paste0("respmean", 1:numcat))
  popmean_col  <-  c(paste0("popmean" , 1:numcat))
  des      <-  df[, des_col]
  respmean <-  df[, respmean_col]
  popmean  <-  df[, popmean_col]
  rsam <-  des - respmean
  psam <-  des - popmean
  rpsam <-  data.frame(rsam, psam)
  # Rename variables, seq is integer.
  colnames(rpsam)  <-  c(paste0("rsam", 1:numcat),
                         paste0("psam", 1:numcat))
  # Combine, save as gh
  gh <-  cbind(des_pop_respmean, rpsam)
  gh <-  gh %>%
           relocate(c(responsesamp1, seq),
          .after = last_col() )
  assign("gh", gh, .GlobalEnv)
  cat("--- End of fn_gh, Pre-matrix ---")
}


#H-----------------------------------
##>> 5 fn_R_indicator ----
#H-----------------------------------

fn_R_indicator <- function() {
  psam_col    <- c(paste0("psam", 1:numcat))
  rsam_col    <- c(paste0("rsam", 1:numcat))
  px          <- as.matrix(gh[, psam_col])
  rx          <- as.matrix(gh[, rsam_col])
  #H--------------------------------------
  ## Calculate propensity scores  ----
  #H--------------------------------------
  # element-wise multiplication (by scalar) : *
  # matrix multiplication(mathematical): %*%
  xxp  <-   t(px) %*% px   # psam
  xxr  <-   t(rx) %*% rx   # rsam
  # zp is row vector.
  popmean_col  <- c(paste0("popmean", 1:numcat))
  zp    <- as.matrix(gh[1, popmean_col])
  zzp   <- t(zp) %*% zp
  yyyp  <- as.numeric(popsize/resppop) * xxp
  yyyr  <- as.numeric(popsize/resppop) * xxr
  ttp   <- as.numeric(popsize) * zzp
  gzpop <-  yyyp + ttp
  gzmix <-  yyyr + ttp
  # computes the Moore-Penrose Generalized
  # INVerse of matrix
  library("MASS")
  bbp   <-  ginv(gzpop)
  bbmix <-  ginv(gzmix)
  # Weights
  nf      <- as.matrix(gh[, "responsesamp1"])
  weightf <- as.matrix(gh[, "piinv"])
  cnf     <-  weightf * nf
  # Obtain the beta coefficients
  des_col <- c(paste0("des",  1:numcat))
  r       <- as.matrix(gh[, des_col])
  ccw     <-  t(r) %*% cnf
  ddpopw  <-  bbp   %*% ccw
  ddmix   <-  bbmix %*% ccw
  # Obtain 2 kinds of propensity scores, column vector
  roipop   <-  r %*% ddpopw
  roimix   <-  r %*% ddmix
  # Save as user-friendly object names
  prop_pop <-  roipop
  prop_mix <-  roimix
  #H---------------------------------
  ## add weighted prop score ----
  #H---------------------------------
  # to be used in partial R-indicators
  # rindicmix is gh_prop_mix
  gh_prop_mix  <- cbind(gh, prop_mix) %>%
     mutate(roi = prop_mix) %>%
     relocate(finalwgt, piinv, .after = last_col())
  #H--------------------------------------
  ## I. Compute R-indicators ----
  #H--------------------------------------
  # Use data frame now. # roimix = prop_mix
  ee1b <- as.data.frame(weightf * roimix)
  ee1c <- as.data.frame(weightf * roipop)
  s2term11b <- (1/popsize) * colSums(ee1b)
  s2term11c <- (1/popsize) * colSums(ee1c)
  weightf   <- as.data.frame(weightf)
        class(weightf)
  s2term21  <- (1/popsize) * colSums(weightf)
     s2term11b ; s2term11c ;   s2term21
  # variances:
  s2T11b    <-  (popsize/(popsize-1)) *
                (s2term11b-(s2term21 * s2term21))
  s2T11c    <-  (popsize/(popsize-1)) *
               (s2term11c-(s2term21 * s2term21))
  # R-indicators:
  r_ind1b <-  1 - 2 * sqrt(s2T11b)
  r_ind1c <-  1 - 2 * sqrt(s2T11c)
    r_ind1b; r_ind1c
  prop_mix_based_R_indicator <- unname(r_ind1b)
  prop_pop_based_R_indicator <- unname(r_ind1c)
  R_indicator <- prop_mix_based_R_indicator
  assign("gh_prop_mix", gh_prop_mix, .GlobalEnv)
  assign("R_indicator", R_indicator, .GlobalEnv)
  return(R_indicator)
  cat("--- End of fn_R_indicator ---")
}

#H-----------------------------------
##>> 6 fn_rindicatorall ----
#H-----------------------------------

fn_rindicatorall <- function() {
    ##  Sum, mean of finalwgt
    #H---------------------------------
    ## 1. Weighted propensity scores ----
    #H---------------------------------
    df      <- gh_prop_mix
    a_list  <- list()
    bb_list <- list()
    for (i in 1:maxvar) {
    # var[i] is the grouping var.
      element <- tapply(df$finalwgt, df[, var[i]], sum)
      a_list[[length(a_list) + 1]] <- element

      element2_ <- tapply(df$roi, df[, var[i]], mean)
      bb_list[[length(bb_list) + 1]] <- element2_
    }
    rindicatorall <- bind_rows(a_list, bb_list)
    rindicatorall     <- data.frame(t(rindicatorall))
    rindicatorall$seq <- seq.int(nrow(rindicatorall))
    fbar_col   <- paste0("fbar",   1:maxvar)
    mrphat_col <- paste0("mrphat", 1:maxvar)
    colnames(rindicatorall) <- c(fbar_col,
                     mrphat_col, "seq")
    fbar   <- rindicatorall[, fbar_col]
    mrphat <- rindicatorall[, mrphat_col]
    # Prepare vectorised fbar_mrphat to merge later
    des1row <- NA
    fbarc <- data.frame(append(des1row, unlist(a_list)))
    mrphatc <- data.frame(append(des1row, unlist(bb_list)))
    fbar_mrphat <- data.frame(dummy_col_names,
      level = "category-level", fbarc, mrphatc)
    colnames(fbar_mrphat) <- c("dummy_col_names",
      "level", "fbar", "mrphat")
    fbar_mrphat$seq <- seq.int(nrow(fbar_mrphat))

    #H------------------------------------
    ## 2. Overall mean of propensities ----
    #H------------------------------------
    mrphatall    <- mean(gh_prop_mix$roi) ; mrphatall
    fbar_mrphat_ <- data.frame(fbar_mrphat, mrphatall)
    # p2Zk = R_indicator, cvpsk=(abs(p2Zk))/mrphatall
    fbar_mrphat_  <- fbar_mrphat_ %>%
      mutate(
        p2Zk = sqrt((fbar/popsize))*(mrphat-mrphatall),
        cvp2k = (abs(p2Zk))/mrphatall ) %>%
      relocate(p2Zk, cvp2k, .after = mrphat) %>%
      rename(domain = dummy_col_names)

    assign("rindicatorall", rindicatorall, .GlobalEnv)
    assign("fbar_mrphat_", fbar_mrphat_, .GlobalEnv)
    assign("fbar",   fbar, .GlobalEnv)
    assign("mrphat", mrphat, .GlobalEnv)
    assign("mrphatall", mrphatall, .GlobalEnv)
    return(rindicatorall)
  cat("--- End of fn_rindicatorall ---")
}

#H-----------------------------------
##>> 7 fn_partial (both microdata and tablebased) ----
#H-----------------------------------

fn_partial <- function() {
    ## 3. Calculations – cat level ----
    # p2Zk = R_indicator, cvpsk=(abs(p2Zk))/mrphatall
    popsize <- as.numeric(popsize)
    p2Zk_list  <- list()
    p1Zk_list  <- list()
    cvp2k_list <- list()
      for (i in 1:maxvar) {
       p2Zk <- sqrt((fbar[i]/popsize))*(mrphat[i]-mrphatall)
       p2Zk_list[[length(p2Zk_list)+1]] <- p2Zk
       element <- p2Zk^2
       p1Zk_list[[length(p1Zk_list)+1]] <- element
       element_ <- (abs(p2Zk))/mrphatall
       cvp2k_list[[length(cvp2k_list)+1]] <- element_
      }
    p2Zk  <- data.frame(p2Zk_list)
    p1Zk  <- data.frame(p1Zk_list)
    cvp   <- data.frame(cvp2k_list)
    zkcvp <- data.frame(p2Zk, p1Zk, cvp)
    zkcvp$seq <- seq.int(nrow(zkcvp))
    p2Zk_col <-  paste0("p2Zk", 1:maxvar)
    p1Zk_col <-  paste0("p1Zk", 1:maxvar)
    cvp2k_col <- paste0("cvp2k",1:maxvar)
    colnames(zkcvp) <-  c(p2Zk_col, p1Zk_col,
                       cvp2k_col, "seq")
    ## 4. Calculations – variable level ----
    right  <-  full_join(rindicatorall, zkcvp)
    df  <-  right
    p1Zk_col <-  paste0("p1Zk", 1:maxvar)
    betweenvar  <- sapply(df[p1Zk_col], sum,
                    na.rm = TRUE)
    sqrtbetween <- sqrt(betweenvar)
    cvpart  <- sqrt(betweenvar)/mrphatall
    between <- data.frame(var, nvar,
             betweenvar, sqrtbetween, cvpart)
    r <- data.frame(R_indicator, resppop)
    colnames(r)  <- c("R_indicator", "resppop")
    between_ <- data.frame(r, between)
    ## 5. Add var names, to right ----
    var_col <-  paste0("var_", 1:maxvar)
    v     <-  as.data.frame(var, var_col)
    v     <-  as.data.frame(t(v))
    v$seq <-  1
    right_  <-  full_join(right, v, by = "seq")
    rr <-  c("var_", "fbar", "mrphat",
             "p2Zk", "p1Zk", "cvp2k" )
    rr_  <- rep(1:maxvar, each = length(rr))
    rrr  <-  rep(paste0(rr,  rr_)) ;
    right_var  <-  right_[ , c(rrr)]
    ## 6. Merge between, and right_var ----
    right  <-  bind_rows(between_, right_var)
    # Type: 0 var level, 1: cat level.
    right  <- right %>%
             rename(freq_ = nvar) %>%
             mutate(seq = row_number(),
              type = ifelse(is.na(freq_), 1, 0))
    right  <- right %>%
             relocate(type, .before = freq_) %>%
             relocate(resppop, .after = freq_)
    ## 7. tidy left, report ----
    # p2Zk = catlevel R_indicator,
    add3   <- c(R_indicator, mrphatall, resppop)
    p2Zk   <- fbar_mrphat_$p2Zk
    p2Zk_  <- append(sqrtbetween, p2Zk)
    p2Zk_  <- append(add3, p2Zk_)
    cvp2k  <- fbar_mrphat_$cvp2k
    cvp2k_ <- append(cvpart, cvp2k)
    cvp2k_ <- append(add3, cvp2k_) # add 3 rows
    top  <- c("Overall", "mrphatall", "resppop")
    var_domain <- append(var, dummy_col_names)
    var_domain_ <- append(top, var_domain)
    var_domain_all <- data.frame(var_domain_, p2Zk_,
                       cvp2k_)
    var_domain_all$seq  <- seq.int(nrow(var_domain_all))
    var_domain_all <- var_domain_all %>%
           rename(domain_ = var_domain_)
    left <- full_join(var_domain_all, freq,
        by = "domain_")
    # Prepare a combined output table
    leftright  <- full_join(left, right)
    leftright_ <- data.frame(leftright, mrphatall)
    leftright_ <- leftright_ %>%
        relocate(mrphatall, .after = R_indicator) %>%
        relocate(seq, .before = domain_)
    leftright_ <- leftright_ %>%
       rename(domain = domain_ ) %>%
       mutate(R_indicator_summary = p2Zk_) %>%
       relocate(R_indicator_summary, .before = p2Zk_)
    partial  <- leftright_ %>%
      mutate(fct_domain = factor(domain, unique(domain))) %>%
      relocate(fct_domain, .after = domain)
    partial <- partial %>%
       rename(levelvar = freq_) %>%
       mutate(seq_from =
        ifelse(domain == "resppop", 1,
         ifelse(domain == "des1", 2, 0) )) %>%
        mutate(level = ifelse(domain != "resppop",
            cumsum(seq_from), 99)) %>%
        mutate(level =
           ifelse(domain == "des1", 99, level)) %>%
        relocate(level,.after = seq) %>%
        relocate(fct_domain, .after = last_col()) %>%
        relocate(cvp2k_, .after = last_col())
      assign("partial", partial, .GlobalEnv)
      cat("-- End of fn_partial, object partial created --")
}
