
#H-----------------------------------
##> Overall ----
#H-----------------------------------

#H-----------------------------------
##>> 1 fn_overall_r_indicator_1 ----
## design_matrix_with_weights
#H-----------------------------------

# Ensure popmean_row_vector is loaded first.
# dplyr:: added to avoid unused argument error

fn_r_indicator_1_vvv <-  function() {
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

  # Cross-check
  # View(aa[1:20,])

  # Reorder, view design matrix
  vvv  <<- vvv %>%
        relocate(finalwgt, .after = last_col()) %>%
        relocate(piinv, .after = last_col())
    # dim(vvv) ; ncol(vvv)
    # vvvfr <<- ncol(vvv)-4
    # vvv[1:10, vvvfr: ncol(vvv)]
  # vvv[1:10,]
  # View(vvv[1:100,])
  cat("--- End of design matrix with weights ---")
}

#H-----------------------------------
##>> 2 fn_overall_r_indicator_2 ----
#H-----------------------------------
# Line 196, warnings ignorable.

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

    # pop_respmean[, 1:5]
    # pop_respmean_fr <<- ncol(pop_respmean)-4
    # pop_respmean[, pop_respmean_fr: ncol(pop_respmean)]

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

  # column popmean27 onwards, when Viewed,
  # numbers shown as 0.0026674> which is okay!
  # R saves space by printing >
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
  # View(gh[1:100, ])

  cat("--- End of Pre-matrix ---")
}

  # removeobjects <- c("des", "respmean", "popmean", "rsam", "psam")
  # rm(list = Filter(exists, removeobjects))

#H-----------------------------------
##>> 5 fn_overall_r_indicator_5 ----
## compute R-indicators
#H-----------------------------------

fn_R_indicators <- function() {
  ls()
  #  "gh"   "numcat"  "popsize" "resppop"
  # View(gh)
  # Check, resppop (SK=1045581, NS=1047084)
  resppop ; popsize
  # converting the data frame into matrix
  x <<-  as.matrix(gh)
  class(x)
  # Prep :
  rsam_col    <<-  c(paste0("rsam", 1:numcat))
  psam_col    <<-  c(paste0("psam", 1:numcat))
  des_col     <<-  c(paste0("des",  1:numcat))
  popmean_col <<-  c(paste0("popmean", 1:numcat))
  respmean_col <<- c(paste0("respmean", 1:numcat))
  rx          <<-  x[, rsam_col]
  px          <<-  x[, psam_col]
  r           <<-  x[, des_col]

  #H--------------------------------------
  ##>> Calculate propensity scores  ----
  #H--------------------------------------
  # element-wise multiplication (by scalar) : *
  # matrix multiplication(mathematical): %*%
  # View(px)
  xxp  <<-   t(px) %*% px   # psam
  xxr  <<-   t(rx) %*% rx   # rsam
  # zp is row vector.
  zp  <<- t(x[1, popmean_col])
  zzp <<- t(zp) %*% zp  # fixed
  yyyp  <<- as.numeric(popsize/resppop) * xxp
  yyyr  <<- as.numeric(popsize/resppop) * xxr
  ttp   <<-  as.numeric(popsize) * zzp
  gzpop <<-  yyyp + ttp
  gzmix <<-  yyyr + ttp
  # computes the Moore-Penrose Generalized
  # INVerse of matrix
  library("MASS")
  bbp   <<-  ginv(gzpop)
  bbmix <<-  ginv(gzmix)
  nf      <<- x[, "responsesamp1"]
  weightf <<- x[, "piinv"]
  cnf     <<-  weightf * nf
  ccw     <<-  t(r) %*% cnf
  # Obtain the beta coefficients
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
  # Remove objects where necessary.
  gc() ; memory.limit()

  #H--------------------------------------
  ## I. Compute R-indicators ----
  #H--------------------------------------
  # Use data frame now.
  ee1b <<- as.data.frame(weightf * roimix) # roimix = prop_mix
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
  prop_mix_based_R_indicator
  prop_pop_based_R_indicator

  # R_indicators_ <- "The R-indicators are:"
  R_indicators  <<- c(unname(r_ind1b), unname(r_ind1c))
  R_indicators
  cat("--- End of Overall R-indicators ---")
  prop_mix_based_R_indicator
}

#****************************************
#H---------------------------------
##> Partial R-indicators Begin ----
#H---------------------------------

# >> Sub-functions
    #H------------------------------------
    ##> Sum, mean of finalwgt (ns)
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

#H---------------------------------
##> fn_r_indicator_partialtemp ----
#H---------------------------------

fn_r_indicator_partialtemp <-  function() {
  aa  <<-  aa %>% mutate(ns = finalwgt,
              seq = row_number() )
  #H---------------------------------
  ##> 1. Prep prop_mix ----
  # use prop for programming.
  #H---------------------------------
  # View(prop) # column vector
  temp  <<-  as.data.frame(prop)
  temp$seq <<- seq.int(nrow(temp))
  head(temp) ; str(temp) ; names(temp)
  # roimix
  colnames(temp) <<- c("roi", "seq")
  prop <<- temp
  head(prop)
  names(gh) ; dim(gh)
  # in SAS, rindicmix or rindicpop, radd
  rindi  <<-  full_join(aa, prop)
  names(rindi) ; dim(rindi)
  #H------------------------------------
  ##> 2. Sum, mean of finalwgt (ns)
  ##     propensity scores (roi) ----
  #H------------------------------------
  df <<-  rindi
  fn_stats_finalwgt_prop_score_none()
    temp <<-  bind_rows(a_list, aa_list,
            b_list, bb_list)
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
  ##> 3. Overall mean of propensities ----
  #H------------------------------------
  mrphatall <<-  mean(df$roi)
  mrphatall       #  0.9597769
  temp  <<- data.frame(fbar_mrphat, mrphatall)
  # p2Zk = R_indicator, cvpsk=(abs(p2Zk))/mrphatall
  temp_cat  <<- temp %>%
    mutate(
      p2Zk = sqrt((fbar/popsize))*(mrphat-mrphatall),
      cvp2k = (abs(p2Zk))/mrphatall
    ) %>%
    relocate(p2Zk, cvp2k, .after = mrphat) %>%
    rename(domain = dummy_col_names)
  #H------------------------------------
  ##> 4. Calculations – cat level ----
  #H------------------------------------
  # Compute partial indicators – cat level
  # use df for programming.
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
  ##> 5. Calc – variable level ----
  #H---------------------------
  # Compute R_indicator_variable_level
  df          <<-  df_
  p1Zk_col    <<-  paste0("p1Zk", 1:variablenum)
  betweenvar  <<-  sapply(df[p1Zk_col], sum,
                  na.rm = TRUE)
  sqrtbetween <<-  sqrt(betweenvar)
  cvpart      <<-  sqrt(betweenvar)/mrphatall
  # SAVE
  temp    <<-   data.frame(var, nvar,
           betweenvar, sqrtbetween, cvpart)
  between     <<- temp
  r           <<- data.frame(R_indicator, resppop)
  colnames(r) <<- c("R_indicator", "resppop")
  between_    <<- data.frame(r, between)
  #H---------------------------
  ##> 6. Add var names, using macro, to right ----
  #H---------------------------
  var_col <<-  paste0("var_", 1:variablenum)
  v       <<-  as.data.frame(var, var_col)
  v       <<-  as.data.frame(t(v))
  v$seq   <<-  1
  right_  <<-  full_join(right, v, by = "seq")
  # Reorder, to give column order
  # var_1 fbar1 mrphat1 p2Zk1 .. var_2 fbar2 ...
  # p2Zk is cat-level (partial) R-indicator
  temp    <<-  c("var_", "fbar", "mrphat",
             "p2Zk", "p1Zk", "cvp2k")
  temp_   <<-  rep(1:variablenum, each = variablenum + 1)
  a       <<-  rep(paste0(temp,  temp_))
  # unnecessary columns will be dropped automatically
  right_var <<-  right_[ , c(a)]
  ## Merge between, and right_var ----
  temp  <-  bind_rows(between_, right_var)
  # Type: 0 var level, 1: cat level.
  temp_  <<-   temp %>%
           rename(freq_ = nvar) %>%
           mutate(seq = row_number(),
            type = ifelse(is.na(freq_), 1, 0))
  temp_  <<-  temp_ %>%
           relocate(type, .before = freq_) %>%
           relocate(resppop, .after = freq_)
  right  <<-  temp_
  ## tidy left, report ----
  ## p2Zk_cvp2k first. numeric. ok. domain is non-numeric!!
  temp0   <<- c(R_indicator, mrphatall, resppop)
  p2Zk    <<- temp_cat$p2Zk
  p2Zk_   <<- append(betweenvar, p2Zk)
  p2Zk_   <<- append(temp0, p2Zk_)  # add 3 rows
  cvp2k   <<- temp_cat$cvp2k
  cvp2k_  <<- append(cvpart, cvp2k)
  cvp2k_  <<- append(temp0, cvp2k_)  # add 3 rows
  top <<- c("R_indicator", "mrphatall", "resppop")
  var_domain <<- append(var, dummy_col_names)
  var_domain_ <<- append(top, var_domain)
  var_domain_all <<- data.frame(var_domain_, p2Zk_, cvp2k_)
  var_domain_all$seq  <<- seq.int(nrow(var_domain_all))
  var_domain_all <<- var_domain_all %>% rename(domain_ = var_domain_)
      var_domain_all
   left    <<- full_join(var_domain_all, freq, by = "domain_")
 # Prepare a combined output table
  partialtemp_  <<- full_join(left, right)
  partialtemp   <<- data.frame(partialtemp_, mrphatall)
  partialtemp   <<- partialtemp %>%
          relocate(mrphatall, .after = R_indicator) %>%
          relocate(seq, .before = domain_)
  partialtemp <<- partialtemp %>%
     rename(domain = domain_,
            R_indicator_temp = R_indicator) %>%
     mutate(
      R_indicator_summary = p2Zk_,
      domain = ifelse(seq == 1, "Overall", domain) ) %>%
     relocate(R_indicator_summary, .before = p2Zk_)
  cat("-- End of fn_r_indicator_partialtemp --")
}
 #    # view(partialtemp)

#H-------------------------------------
##> R-indicator_Domain_order ----
#H-------------------------------------

fn_r_indicator_domain_order_partial <- function() {

  # View(partialtemp)

  # New factor domain variable
  order  <- c(print(partialtemp$domain))
  order
  partialtemp$fct_domain <- factor(partialtemp$domain,
    levels = order)

  levels(partialtemp$fct_domain)
  partialtemp %>% tabyl(fct_domain)

  partial  <<- partialtemp %>%
      relocate(fct_domain, .after = domain)

  cat("End of fn_r_indicator_domain_order_partial")
}

### End ###