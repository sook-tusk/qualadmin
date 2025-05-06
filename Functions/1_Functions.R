
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

#===================================
##>  AUXILIARY   ----
#===================================

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
## > fn_freq_table ----
##H -------------------------------

fn_freq_table <- function(x) {
  d   <-  x
  var <-  names(d); var
    assign("var", var, .GlobalEnv)
  d   <-  d %>%
        dplyr::select(any_of(var)) %>%
        mutate_at(var, factor)

    fn_tw <- function(col_name) {
      t <- table(d[, c(var[i], col_name)])
      tt <- data.frame(prop.table(t) )
      names(tt) <- c("v1", "v2", "p")
      tw <-  data.frame(tt, n = c(t),
        v = i, by1 = var[i],
        twby = col_name)
    }

  tw_list <- list()
  # Takes a while...
  for(i in 1:ncol(d)) {
    tw_list[[i]] <- lapply(names(d), fn_tw)
  }
  tw <- bind_rows(tw_list); head(tw)
  oneway <- tw %>% filter(by1 == twby) %>%
     filter(n != 0) %>%
     mutate(v1 = v, by2 = 0, oneway = 1) %>%
     mutate(twby = "oneway")

  # Add total
  total <- data.frame(n = nrow(d), p = 1); total
  oneway  <- bind_rows(total, oneway);
  # Prep last cats
  oneway_last <- oneway %>%
    group_by(v) %>%
    mutate(notlast = ifelse(
     row_number() == n(), 0, 1) ) %>%
    ungroup
  oneway_last <- oneway_last %>%
    mutate(notlast = ifelse(
     row_number() == 1, 1, notlast) ) %>%
    mutate(des_seq = cumsum(notlast) ) %>%
    mutate(des_seq = ifelse(
     notlast == 0, -999, des_seq) )

  tw_temp <- tw %>% filter(by1 != twby)
  # Create an index to remove duplicates
  t_num <- data.frame(
        by2 = c(1:length(var)), twby = var)
  tw_temp <- left_join(tw_temp, t_num)
  tw_temp <- tw_temp %>%
    mutate(drop = ifelse(v > by2, 1, 0))
  tway <- tw_temp %>% filter(drop == 0) %>%
    mutate(oneway = 2, notlast = -999, des_seq=0) %>%
    arrange(v, by1, by2, v1, v2) %>%
    dplyr::select(-drop)
  freq_table <- rbind(oneway_last, tway)
  freq_table <- freq_table %>%
    mutate(seq = row_number()) %>%
    mutate(oneway = ifelse(
      row_number() == 1, 1, oneway)) %>%
    mutate(twby = ifelse(
      row_number() == 1, "total", twby)) %>%
    mutate(oneway_by = ifelse(
      oneway == 1,
      paste0(by1, "_", v2), "")) %>%
    mutate(oneway_by = ifelse(
      twby == "total", "total", oneway_by)) %>%
    dplyr::select(seq, des_seq, oneway_by,
      notlast, n, p, everything())
    assign("freq_table", freq_table, .GlobalEnv)
    cat("--- End of fn_freq_table ---")
}


#===================================
## > Compute distance_metrics
#===================================

fn_distance_metrics <- function() {
  #H--------------------------------------
  ##  Create DOMAINS ----
  #H--------------------------------------
  domaintemp <- pop_admin_freq_table %>%
    mutate(
    temp_id = as.numeric(paste0(oneway, v, by2))
            ) %>%
    relocate(temp_id, .after = seq)

  domaintemp <- domaintemp %>%
      group_by(temp_id) %>%
      mutate(
        domain_id = cur_group_id(),
        domain = paste0(
          ifelse(twby == "oneway", oneway_by,
            paste0(by1, ":", twby)))) %>%
      ungroup()
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
       ifelse(p != 0, p * log2(p), 0) ) %>%
    group_by(domain_id) %>%
    mutate(
      entropy_psigma = sum(entropy_ptemp),
      entropy_p = -1 * entropy_psigma ) %>%
    ungroup() %>%
    mutate(abs_perc = abs(p - admin_p)) %>%
    group_by(domain_id) %>%
    mutate(
     Duncan = 0.5 *sum(abs_perc)) %>%
    ungroup()

  unstd_distance_metrics_full <- domain_ %>%
    mutate(
      htemp = sqrt(p) - sqrt(admin_p),
      htempsq = htemp^2,
      ktemp = ifelse(p != 0 & admin_p != 0,
              p * log2((p/admin_p)), 0)
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


##H -------------------------------
## > fn_bivariate_counts_popcov ----
##H -------------------------------

fn_bivariate_counts_popcov <- function(x) {
  df <- x
  var <-  names(df)
  assign("var", var, .GlobalEnv)
  df   <-  df %>%
        dplyr::select(any_of(var)) %>%
        mutate_at(var, factor)

  mylist <- lapply(df, oneway_freq_table_no_total)
  freq <- as.data.frame(rlist::list.rbind(mylist))

  tw_list <- list()
  t_list  <- list()

  fn_tw_n <- function(col_name) {
      t <- table(df[, c(var[i], col_name)])
    }

  for(i in 1:ncol(df)) {
    tw_list[[i]] <- lapply(names(df), fn_tw_n)
    t_list[[i]]  <- bind_cols( tw_list[[i]] )
  }

  t0 <- bind_rows(t_list);
  t0 <- bind_cols(freq, t0);

# Prep last cats, add total, weights(*50)
  levelsvar <- sapply(df, levels) ; levelsvar
  lastcat <-  NULL
  for (i in 1:length(df)) {
     lastcat[i] <- length(levelsvar[[i]])
     removecol <-  cumsum(nrow(t_list[[i]]))
    }

  remove_row <- cumsum(lastcat); remove_row
  remove_col <- 1 + cumsum(lastcat); remove_col
  tw0 <- t0[-remove_row, ]
  tw0 <- tw0[, -remove_col] ; dim(tw0)
  # tidy colnames, rownames
  rownames(tw0) <- NULL
  colnames(tw0)  <- c(paste0("c", 1:length(tw0)))
  first <- tw0[, 1]
  total  <- sum(mylist[[1]])
  row0 <- data.frame(total, t(first) );
  colnames(row0)  <- c(paste0("c", 1:length(row0)))
  tw <- bind_rows(row0, tw0);

  # Prep matrix for popcov = cov
  popcov <- as.matrix(tw);
  head(popcov)
  assign("popcov", popcov, .GlobalEnv)
  return(popcov)
  cat("--- End of fn_bivariate_counts_popcov ---")
}



#H ------------------------------
#> fn_pop_respmean ----
#H ------------------------------

fn_pop_respmean <- function(x) {
    pop_admin_freq_table <- x
    oneway <- pop_admin_freq_table %>%
     filter(oneway == 1)
# Create freq, dummy_col_names for admin data
    freq <- oneway %>%
     mutate(v = ifelse(
      oneway_by == "total", 0, v)) %>%
      dplyr::select(domain = oneway_by,
        count = admin_n, n_cat = v2,
        domain_n = v)
# Create pop_respmean
    pop_admin <- oneway %>%
     mutate(popmean_c = ifelse(
      notlast == 1, paste0("popmean", des_seq), "")) %>%
     mutate(respmean_c = ifelse(
      notlast == 1, paste0("respmean", des_seq), "")) %>%
     relocate(popmean_c, .before = n) %>%
     relocate(respmean_c, .after = admin_p)
    row1 <- pop_admin[1, ]
    row1 <- row1 %>%
     mutate(rrate = admin_n / n) %>%
     mutate(finalwgt = 1 / rrate) %>%
     mutate(popsize = n) %>%
     dplyr::select(rrate,
         finalwgt, popsize)
    pop_admin <- data.frame(pop_admin, row1)
    pop_admin_mean_c <- pop_admin %>%
     mutate(
      weighted_admin_n = admin_n*finalwgt) %>%
     mutate(
      respmean = weighted_admin_n/popsize) %>%
     dplyr::select(seq, des_seq, oneway_by, popmean_c,
       n, p, by1, v2, admin_n, admin_p, weighted_admin_n,
       respmean_c, respmean, rrate, finalwgt, popsize)

    mean_c <- pop_admin_mean_c %>%
      filter(popmean_c != "") %>%
      dplyr::select(oneway_by, popmean_c, p,
        respmean_c, respmean)

    pop_mean   <- t(mean_c[, 2:3])
    admin_mean <- t(mean_c[, 4:5])

    pop_admin_mean <- data.frame(pop_mean, admin_mean)
    colnames(pop_admin_mean) <- pop_admin_mean[1,]
    pop_admin_mean <- pop_admin_mean[-1, ]
    row.names(pop_admin_mean) <- 1:nrow(pop_admin_mean)
    pop_respmean <- pop_admin_mean
    pop_respmean <- pop_respmean %>%
      mutate_if(is.character, as.numeric)
        head(pop_respmean)
    assign("pop_admin_mean_c", pop_admin_mean_c, .GlobalEnv)
    assign("mean_c", mean_c, .GlobalEnv)
    assign("pop_respmean", pop_respmean, .GlobalEnv)
    assign("freq", freq, .GlobalEnv)
    return(pop_respmean)
}





## > ===============================
#===================================
##>      R_INDICATORS   ----
#===================================
fn_RUN_R_indicator <- function() {
    fn_design_matrix()
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
    ##  1) lastcat   ----
    #H-----------------------------------
    # aa  <- x
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
    ##  2) weights   ----
    #H-----------------------------------
    # Declare macro variables
    (resppop <-  nrow(aa))
    (rrate   <-  nrow(aa)/popsize)
    finalwgt <-  1/rrate; finalwgt
    piinv    <-  1.0
    aa_ <-  aa %>%
          mutate(
            finalwgt = 1/rrate,
            pi = piinv) %>%
          mutate(piinv  = 1/pi )
    aa_ %>% tabyl(finalwgt) # 1.125753 1033664
    # View(aa[1:20, ])
    #H-----------------------------------
    ## 3) design_matrix ----
    #H-----------------------------------
    # Intercept
    aa_  <-  aa_ %>% mutate(des1 = 1)
    # Allow a minute to execute.
    library("fastDummies")
    aa_dum <-  dummy_cols(aa_, select_columns = var)
    # Keep necessary dummy variables only.
    temp_remove  <- c(lastcat, "pi")
    design_matrix_names <- aa_dum %>%
      dplyr::select(-any_of(temp_remove)) %>%
      relocate(c(finalwgt, piinv), .after = last_col())

      # names(aa_) ; dim(vvv)
      names(design_matrix_names)
    # View(design_matrix_names[1:100,])
    # Rename as des variables
    # Define maximum numcat (eg. 35)
    design_matrix <- design_matrix_names
    vvv    <- design_matrix
    extra  <- c("finalwgt", "piinv")
    numcat <- ncol(vvv)-length(extra)-length(aa) ; numcat
      assign("numcat", numcat, .GlobalEnv)
    # to be merged with pop_respmean,
    colnames(design_matrix)  <- c(names(aa),
      paste0("des", 1:numcat), "finalwgt", "piinv")
          names(design_matrix)
        # head(design_matrix, n = 3)
    # to use in fn_prep_aa_freq()
    pall  <- aa_dum
    assign("resppop", resppop, .GlobalEnv)
    assign("rrate", rrate, .GlobalEnv)
    assign("var",     var, .GlobalEnv)
    assign("maxvar",  maxvar, .GlobalEnv)
    assign("lastcat", lastcat, .GlobalEnv)
    assign("nvar",    nvar, .GlobalEnv)
    assign("finalwgt", finalwgt, .GlobalEnv)
    assign("pall",    pall, .GlobalEnv)
    assign("design_matrix", design_matrix, .GlobalEnv)
    assign("design_matrix_names", design_matrix_names, .GlobalEnv)
    assign("vvv",     vvv, .GlobalEnv)
    cat("--- End of fn_design_matrix ---")
}


#H-----------------------------------
##>> 2 fn_des_pop_respmean (REPEAT)
# (both microdata and tablebased) ----
#H-----------------------------------
fn_des_pop_respmean <- function() {
  pop_respmean <- pop_respmean %>%
      mutate_if(is.character, as.numeric)
  glimpse(pop_respmean)
  des_pop_respmean  <-
    data.frame(design_matrix, pop_respmean,
    row.names = NULL)
  des_pop_respmean  <- des_pop_respmean %>%
    mutate(seq = row_number(), responsesamp1 = 1)
      head(des_pop_respmean, n = 3)
  # Inspect
      names(des_pop_respmean)
  assign("des_pop_respmean", des_pop_respmean, .GlobalEnv)
  cat("--- End of fn_des_pop_respmean ---")
}

#H-----------------------------------
##>> 3 fn_gh (REPEAT)
## (both microdata and tablebased) ----
## Create diff_des_mean (gh)  ----
#H-----------------------------------
fn_gh <-  function() {

  df <-  data.frame(des_pop_respmean)
  numcat <- ncol(design_matrix) - ncol(aa) - 2 ; numcat
    assign("numcat",  numcat, .GlobalEnv)
  des_col      <-  c(paste0("des"     , 1:numcat))
  respmean_col <-  c(paste0("respmean", 1:numcat))
  popmean_col  <-  c(paste0("popmean" , 1:numcat))
  des      <-  df[, des_col]
  respmean <-  df[, respmean_col]
  popmean  <-  df[, popmean_col]
  rsam <-  des - respmean
  psam <-  des - popmean
  rpsam <-  data.frame(rsam, psam)
  colnames(rpsam)  <-  c(paste0("rsam", 1:numcat),
                         paste0("psam", 1:numcat))
  # Combine, save as gh
  gh <-  cbind(des_pop_respmean, rpsam)
  gh <-  gh %>%
   relocate(c(responsesamp1, seq), .after = last_col() )
      names(gh)
  assign("gh", gh, .GlobalEnv)
  cat("--- End of fn_gh, Pre-matrix ---")
}


#H-----------------------------------
##>> 4 fn_R_indicator ----
#H-----------------------------------

fn_R_indicator <- function() {
  numcat <- ncol(design_matrix) - ncol(aa) - 2 ; numcat
    assign("numcat",  numcat, .GlobalEnv)
  psam_col    <- c(paste0("psam", 1:numcat))
  rsam_col    <- c(paste0("rsam", 1:numcat))
  px          <- as.matrix(gh[, psam_col])
  rx          <- as.matrix(gh[, rsam_col])
  #H--------------------------------------
  ## 1) Calculate propensity scores  ----
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
  gzmix <-  yyyr + ttp
  # Update: Apr2025
  gzpop_old <- yyyp + ttp # previously
  gzpop <- popcov
  # computes the Moore-Penrose Generalized
  # INVerse of matrix
  library("MASS")
  bbp   <-  ginv(gzpop)
  bbmix <-  ginv(gzmix)
  # Weights
  nf      <- as.matrix(gh[, "responsesamp1"])
  weightf <- as.matrix(gh[, "piinv"])
  cnf     <-  weightf * nf
  # Prep
  des_col <- c(paste0("des",  1:numcat))
  r       <- as.matrix(gh[, des_col])
  ccw     <-  t(r) %*% cnf
  # Obtain the beta coefficients
  ddmix   <-  bbmix %*% ccw
  ddpopw  <-  bbp   %*% ccw
  # Obtain 2 kinds of propensity scores, column vector
  roimix   <-  r %*% ddmix
  roipop   <-  r %*% ddpopw
  # Save as user-friendly object names
  prop_mix <-  roimix
  prop_pop <-  roipop
  #H---------------------------------
  ## 2) add weighted prop score ----
  #H---------------------------------
  # to be used in partial R-indicators
  # rindicmix is gh_prop_mix
  gh_prop_mix  <- cbind(gh, prop_mix) %>%
     mutate(roi = prop_mix) %>%
     relocate(finalwgt, piinv, .after = last_col())
  #H--------------------------------------
  ## 3) Compute R-indicators ----
  #H--------------------------------------
  # Use data frame now. # roimix = prop_mix
  ee1b <- as.data.frame(weightf * roimix)
  ee1c <- as.data.frame(weightf * roipop)
  s2term11b <- (1/popsize) * colSums(ee1b)
  s2term11c <- (1/popsize) * colSums(ee1c)
  weightf   <- as.data.frame(weightf)
  s2term21  <- (1/popsize) * colSums(weightf)
  # variances:
  var_s2T11b  <-  (popsize/(popsize-1)) *
                (s2term11b-(s2term21 * s2term21))
  var_s2T11c  <-  (popsize/(popsize-1)) *
               (s2term11c-(s2term21 * s2term21))
    var_s2T11b ; var_s2T11c
    print(paste0("variance is ", var_s2T11b))
  # R-indicators: 1-SD(var_of_propensityscores)
  r_ind1b <-  1 - sqrt(var_s2T11b)
  r_ind1c <-  1 - sqrt(var_s2T11c)
  prop_mix_based_R_indicator <- unname(r_ind1b)
  prop_pop_based_R_indicator <- unname(r_ind1c)
      prop_pop_based_R_indicator
  R_indicator <- prop_mix_based_R_indicator
      R_indicator;
  assign("gh_prop_mix", gh_prop_mix, .GlobalEnv)
  assign("R_indicator", R_indicator, .GlobalEnv)
  return(R_indicator)
  cat("--- End of fn_R_indicator ---")
}

# Previously 0.4960263 Updated 0.7480132



#H-----------------------------------
##>> 5 fn_rindicatorall ----
#H-----------------------------------

fn_rindicatorall <- function() {
    ##  Sum, mean of finalwgt
    #H---------------------------------
    ## 1) Weighted propensity scores ----
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
    dummy_col_names <- freq[, 1]
    assign("dummy_col_names", dummy_col_names, .GlobalEnv)
    fbar_mrphat <- data.frame(dummy_col_names,
      level = "category-level", fbarc, mrphatc)
    colnames(fbar_mrphat) <- c("dummy_col_names",
      "level", "fbar", "mrphat")
    fbar_mrphat$seq <- seq.int(nrow(fbar_mrphat))

    #H------------------------------------
    ##  2) Overall mean of propensities ----
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
##>> 6 fn_partial (REPEAT)
## (both microdata and tablebased) ----
#H-----------------------------------

fn_partial <- function() {
  #H------------------------------------
  ## 1) Calculations – cat level ----
  #H------------------------------------
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
  #H------------------------------------
  ## 2) Calculations – variable level ----
  #H------------------------------------
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
  #H------------------------------------
  ## 3) Merge between, and right_var ----
  #H------------------------------------
    right  <-  bind_rows(between_, right_var)
    # Type: 0 var level, 1: cat level.
    right  <- right %>%
             rename(freq_ = nvar) %>%
             mutate(seq = row_number(),
              type = ifelse(is.na(freq_), 1, 0))
    right  <- right %>%
             relocate(type, .before = freq_) %>%
             relocate(resppop, .after = freq_)
  #H------------------------------------
  ## 4) Tidy left, report ----
  #H------------------------------------
    # p2Zk = catlevel R_indicator,
    add3   <- c(R_indicator, mrphatall, resppop)
    p2Zk   <- fbar_mrphat_$p2Zk
    p2Zk_  <- append(sqrtbetween, p2Zk)
    p2Zk_  <- append(add3, p2Zk_)
    cvp2k  <- fbar_mrphat_$cvp2k
    cvp2k_ <- append(cvpart, cvp2k)
    cvp2k_ <- append(add3, cvp2k_) # add 3 rows
    top  <- c("Overall", "mrphatall", "resppop")

# updated
    var_        <- data.frame(domain = var)
    top         <- data.frame(domain = top)
    var_domain  <- bind_rows(top, var_, dummy_col_names)
    var_domain_all <- data.frame(var_domain, p2Zk_,
                       cvp2k_)
    var_domain_all$seq  <- seq.int(nrow(var_domain_all))
    left <- full_join(var_domain_all, freq,
        by = "domain")
    # Prepare a combined output table
    leftright  <- full_join(left, right)
    leftright_ <- data.frame(leftright, mrphatall)
    leftright_ <- leftright_ %>%
       relocate(mrphatall, .after = R_indicator) %>%
       rename(levelvar = freq_) %>%
       relocate(seq, .before = domain)
    partial <- leftright_ %>%
       mutate(R_indicator_summary = p2Zk_) %>%
       relocate(R_indicator_summary, .before = p2Zk_) %>%
       relocate(c(domain, cvp2k_), .after = last_col()) %>%
       mutate(fct_domain = factor(domain, unique(domain))) %>%
      relocate(fct_domain, .after = seq)
    partial <- partial %>%
       mutate(seq_from =
        ifelse(domain == "resppop", 1,
         ifelse(domain == "total", 2, 0) )) %>%
        mutate(level = ifelse(domain != "resppop",
            cumsum(seq_from), 99)) %>%
        mutate(level =
           ifelse(domain == "total", 99, level)) %>%
        relocate(level,.after = seq)
      assign("partial", partial, .GlobalEnv)
      cat("-- End of fn_partial, object partial created --")
}

## > ===============================
#===================================
##>  TABLE-BASED R_INDICATOR   ----
#===================================
fn_RUN_table_based_R_indicator <- function() {
     fn_t_design_matrix()
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
  ##  1) lastcat   ----
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
  ##  2) Produce a pp table ----
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
        element <- rep(seq(1:nvar[i]),
          each = prod_list[[i]], length.out = gridrow)
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
  ##  3) weights ----
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
  matched_wgt  <- cbind(matched_, finalwgt_global,
                 row.names = NULL)
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
  ## 4) pp_design_matrix  ----
  #H--------------------------------------
  # Check number of categories of each variable.
  # Size of categories, last cat (to be dropped)
  deleteupto <- ncol(pall) ;  deleteupto
  pall  <- pall %>% mutate(des1 = 1)
  library("fastDummies")
  pall <- dummy_cols(pall, select_columns = var)
  temp_remove   <- c(lastcat, "nnn",
      "pp_seq",  "finalwgt_global")
  # keep var in the first few columns
  design_matrix_temp <- pall %>%
   dplyr::select(- one_of(temp_remove)) %>%
   relocate(c(wgt_nnn, piinv), .after = last_col())
      tail(design_matrix_temp, n = 3)
  design_matrix <- design_matrix_temp
  # Define maximum numcat (eg. 35)
  extra  <- c("wgt_nnn", "piinv") # "piinv = nnn",
  numcat <- ncol(design_matrix)-length(extra)-length(aa)
    assign("numcat", numcat, .GlobalEnv)
  # Rename design matrix columns as des variables
  # to be merged with pop_respmean, mergingid=ttt
  colnames(design_matrix) <- c(names(aa), paste0("des", 1:numcat), "wgt_nnn", "piinv")  # "nnn",
  # design_matrix <- cbind(design_matrix, ttt = 0)
      head(design_matrix, n = 3)

  t_design_matrix <- design_matrix
    assign("resppop", resppop, .GlobalEnv)
    assign("rrate", rrate, .GlobalEnv)
    assign("var", var, .GlobalEnv)
    assign("maxvar", maxvar, .GlobalEnv)
    assign("lastcat", lastcat, .GlobalEnv)
    assign("nvar", nvar, .GlobalEnv)
    assign("finalwgt_global", finalwgt_global, .GlobalEnv)
    assign("deleteupto", deleteupto, .GlobalEnv)
    assign("pall", pall, .GlobalEnv)
    assign("aa_", aa_, .GlobalEnv)
    assign("t_design_matrix", t_design_matrix, .GlobalEnv)
    assign("design_matrix", design_matrix, .GlobalEnv)
  cat("--- End of fn_t_design_matrix ---")
}


#=========================================
## > 4 fn_t_R_indicator ----
## Using Matrix, compute R-indicators
#=========================================
fn_t_R_indicator <-  function() {
  numcat <- ncol(design_matrix) - ncol(aa) - 2 ; numcat
    assign("numcat",  numcat, .GlobalEnv)
  psam_col    <- c(paste0("psam", 1:numcat))
  rsam_col    <- c(paste0("rsam", 1:numcat))
  px          <- as.matrix(gh[, psam_col])
  rx          <- as.matrix(gh[, rsam_col])
  #H-----------------------------------
  ## 1) Calculate propensity scores  ----
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
  ttp   <- as.numeric(popsize) * zzp
  # yyyp = finalwgt X xxp(crossproduct of psam)
  # ttp  = popsize  X zzp(crossproduct of popmean)
  gzmix <- yyyr + ttp
  # Update: Apr2025
  # gzpop <- yyyp + ttp # previously
  gzpop <- popcov
  # computes the Moore-Penrose Generalized
  # INVerse of matrix
  library("MASS")
  bbmix <- ginv(gzmix)
  bbp   <- ginv(gzpop)
  # Weights
  # Treat weightf, nf as matrix (not column vectors)
  nf      <- as.matrix(gh[, "responsesamp1"])
  weightf <- as.matrix(gh[, "piinv"])
  cnf     <- weightf * nf
  # Prep
  des_col <- c(paste0("des",  1:numcat))
  r       <- as.matrix(gh[, des_col])
  ccw     <- t(r) %*% cnf
 # Obtain the beta coefficients
  ddmix  <- bbmix %*% ccw
  ddpopw <- bbp   %*% ccw
  # Get 2 kinds of propensity scores (column vector)
  roimix <- r %*% ddmix
  roipop <- r %*% ddpopw
  # Save as user-friendly object names
  prop_mix  <- roimix
  prop_pop  <- roipop
  #H---------------------------------
  ## 2) add weighted prop score ----
  #H---------------------------------
  # to be used in partial R-indicators
  # rindicmix is gh_prop_mix
  gh_prop_mix  <- cbind(gh, prop_mix) %>%
     mutate(roi = prop_mix) %>%
     mutate(roi_wgt = roi * wgt_nnn) %>%
     relocate(wgt_nnn, piinv, .after = last_col())
  #H--------------------------------------
  ## 3) Compute R-indicators ----
  #H--------------------------------------
  # use propensity weighting to adjust for coverage bias
  ee1b <- as.data.frame(weightf * roimix)
  ee1c <- as.data.frame(weightf * roipop)
  s2term11b <- (1/popsize) * colSums(ee1b)
  s2term11c <- (1/popsize) * colSums(ee1c)
  weightf  <- as.data.frame(weightf)
  s2term21 <- (1/popsize) * colSums(weightf)
  # variances
  var_s2T11b <- (popsize/(popsize-1)) *
                (s2term11b-(s2term21 * s2term21))
  var_s2T11c <- (popsize/(popsize-1)) *
               (s2term11c-(s2term21 * s2term21))
    var_s2T11b ; var_s2T11c
    print(paste0("variance is ", var_s2T11b))
  # R-indicators: 1-SD(var_of_propensityscores)
  # e.g., 1-sqrt(0.06349736)
  r_ind1b <- 1 - sqrt(var_s2T11b) # prop_mix-based
  r_ind1c <- 1 - sqrt(var_s2T11c) # prop_pop-based
  prop_mix_based_R_indicator <- unname(r_ind1b)
  prop_pop_based_R_indicator <- unname(r_ind1c)
        prop_pop_based_R_indicator
  R_indicator <- prop_mix_based_R_indicator
      R_indicator;
    print(paste0("R_indicator is ", R_indicator))
  assign("gh_prop_mix", gh_prop_mix, .GlobalEnv)
  assign("R_indicator", R_indicator, .GlobalEnv)
  return(R_indicator)
  cat("--- End of fn_t_R_indicator ---")
}


#=========================================
## > 5 fn_t_rindicatorall ----
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
      dummy_col_names <- freq[, 1]
      assign("dummy_col_names", dummy_col_names, .GlobalEnv)
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
