
#H-------------------------------------
##> R-indicator_Domain_order ----
#H-------------------------------------

fn_r_indicator_domain_order_partial <- function() {

  partialtemp  <- partialtemp %>%
     rename(domain = domain_,
            R_indicator_temp = R_indicator) %>%
     mutate(
      domain = ifelse(seq == 1, "Overall", domain))

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

#H---------------------------------
##> Partial R-indicators ----
#H---------------------------------

fn_r_indicator_partialtemp <-  function() {
  # use prop for programming.
  aa  <<-  aa %>% mutate(ns = finalwgt,
              seq = row_number() )
  #H---------------------------------
  ##> 1. Prep prop_mix ----
  #H---------------------------------
  # View(prop) # column vector
  temp  <<-  as.data.frame(prop)
  temp$seq <<- seq.int(nrow(temp))
  head(temp) ; str(temp) ; names(temp)
  # In SAS, roimix
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
  # View(df[1:50, ])
    # testing
    # var[1]
    #  tapply(df$ns, df[, var[1]], sum)
    #  tapply(df$ns, df[, var[2]], sum)
  fn_stats_finalwgt_prop_score_none()
    a_list
    # aa_list ; b_list
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
  # Prepare vectrorised fbar_mrphat to merge later
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
  fn_R_indicator_cat_level_temp()
  length(p2Zk_list)
  temp$seq <<- seq.int(nrow(temp))
  # Rename variables
  p2Zk_col <<-  paste0("p2Zk", 1:variablenum)
  p1Zk_col <<-  paste0("p1Zk", 1:variablenum)
  cvp2k_col <<-  paste0("cvp2k",1:variablenum)
  colnames(temp) <<-  c(p2Zk_col, p1Zk_col,
                     cvp2k_col, "seq")
  # names(temp) ; head(temp)
  # View(temp)
  # Merge by seq
  df  <<-  full_join(df, temp)
  ## Add var names, using macro
  v     <<-  as.data.frame(t(var))
  colnames(v)  <<-  var
  v  <<-  v %>% mutate(seq = row_number())
  # View(v)
  # Ensure not to repeat varnames column.
  df_  <<-  full_join(df, v, by = "seq")
  # names(df_) ; class(df_) ; head(df_)
  # View(df_)
  # Reorder, to give column order
  # geog1a fbar1 mrphat1 p2Zk1 .. sex fbar2 ...
  temp <<-  c("fbar","mrphat",
            "p2Zk", "p1Zk", "cvp2k")
  temp_  <<-  rep(1:variablenum, each = variablenum)
  a  <<-  rep(paste0(temp,  temp_))
  # Reorder
  df_  <<-  df_[ , c(var, a)]
  df_  <<-  df_ %>% relocate(2, .before = fbar2)
  df_  <<-  df_ %>% relocate(2, .before = fbar3)
  df_  <<-  df_ %>% relocate(2, .before = fbar4)
  df_  <<-  df_ %>% relocate(2, .before = fbar5)
  names(df_)
  #H---------------------------
  ##> 5. Calc – variable level ----
  #H---------------------------
  # Compute R_indicator_variable_level
  df  <<-  df_
  p1Zk_col <<-  paste0("p1Zk", 1:variablenum)
  p1Zk_col
  betweenvar  <<-  sapply(df[p1Zk_col], sum,
                  na.rm = TRUE)
  sqrtbetween  <<-  sqrt(betweenvar)
  cvpart <<-  sqrt(betweenvar)/mrphatall
  # SAVE
  temp  <<-   data.frame(var, nvar,
           betweenvar, sqrtbetween, cvpart)
  between  <<- temp
  r <<- data.frame(R_indicator, resppop)
  colnames(r)  <<- c("R_indicator", "resppop")
  between_ <<- data.frame(r, between)
  temp  <<-  bind_rows(between_, df)
  # View(temp)
  # Type: 0 var level, 1: cat level.
  temp_  <<-   temp %>%
           rename(freq = nvar) %>%
           mutate(
            seq = row_number(),
            type = ifelse(is.na(freq), 1, 0))
  temp_  <<-  temp_ %>%
           relocate(type, .before = freq) %>%
           relocate(resppop, .after = freq)
  temp  <<- data.frame(fbar_mrphat, mrphatall)
  temp_cat  <<- temp %>%
    mutate(
      p2Zk = sqrt((fbar/popsize))*(mrphat-mrphatall),
      cvp2k = (abs(p2Zk))/mrphatall
    ) %>%
    relocate(p2Zk, cvp2k, .after = mrphat) %>%
    rename(domain = dummy_col_names)

  ### For simulation reports.
  #  use mutate, then create all p2Zk, cvp2k
  # Now, append variable level r-indicator.
  temp  <<- c("R_indicator",
             "mrphatall", "resppop")
  temp0  <<- c(R_indicator, mrphatall, resppop)
  domain  <<- temp_cat$domain
  domain_ <<- append(var, domain)
  domain_ <<- append(temp, domain_)
  p2Zk   <<- temp_cat$p2Zk
  p2Zk_  <<- append(betweenvar, p2Zk)
  p2Zk_  <<- append(temp0, p2Zk_)
  cvp2k  <<- temp_cat$cvp2k
  cvp2k_ <<- append(cvpart, cvp2k)
  cvp2k_ <<- append(temp0, cvp2k_)
  # Prepare a combined output table
   temp <<- data.frame(domain_, p2Zk_, cvp2k_)
   temp$seq <<- seq.int(nrow(temp))
   r_ind_sim <<- temp
  temp    <<- full_join(r_ind_sim, freq)
  partialtemp <<- full_join(temp, temp_)
  partialtemp <<- data.frame(partialtemp, mrphatall)
  partialtemp <<- partialtemp %>%
             relocate(mrphatall,
             .after = R_indicator) %>%
             relocate(seq, .before = domain_)
  cat("-- End of fn_r_indicator_partialtemp --")
}

#****************************************
#H-----------------------------------
##> Overall ----
#H-----------------------------------

#H-----------------------------------
##>> 1 fn_overall_r_indicator_1 ----
#H-----------------------------------

# Ensure popmean_row_vector is loaded first.
# dplyr:: added to avoid unused argument error

fn_overall_r_indicator_1 <-  function() {
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
    # names(aa)
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

    # Check: recycle lastcat macros.
    # Check the first two variables & lastcat
    # Print (customise as needed)
     #  var[1] ; nvar[[1]] ;
     #  lastcat[1]
     #  var[2] ; nvar[[2]] ; lastcat[2]

     # head(aa[1:10, 1:4])
     # head(aa[1:10, 9:11])
     # ncol_minus3  <<-  ncol(aa) - 3
     # ncol_minus3
     # tail(aa[1:10, ncol_minus3 : ncol(aa)])
    # View(aa[1:10, ])

    #H--------------------------------------------
    ## D Admin summary Freq Table + dummy variables
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

#H-----------------------------------
##>> 2 fn_overall_r_indicator_2 ----
#H-----------------------------------
# Line 196, warnings ignorable.

fn_overall_r_indicator_2 <-  function() {

  # Save the dataset as vvv
  vvv  <<- aa

  # Keep necessary dummy variables only.
  names(vvv) ;   dim(vvv)
  temp_remove  <<- c("pi", var, lastcat)
  vvv  <<- vvv %>%
        dplyr::select(- one_of(temp_remove))

  # vvv  <- vvv %>%
  #       dplyr::select(-"pi") %>%
  #       dplyr::select(-any_of(var)) %>%
  #       dplyr::select(-all_of(lastcat))
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
  # View(vvv[1:1000,])

  # Reorder
  vvv  <<- vvv %>%
        relocate(finalwgt, .after = last_col()) %>%
        relocate(piinv, .after = last_col())

  #H--------------------------------------------
  ## F Distributions as row vectors ----
  #H--------------------------------------------

  # Admin data
  ls()
  popsize

  finalwgt  <<-  1/rrate
  table(finalwgt)

  # Treat des1(Intercept) separately. 0.999045879
  a  <<-  vvv %>% tabyl(des1) # unweighted=1047084
  a[2]

  aw  <<-  a[2] * finalwgt
  aw
  respmean1  <<-  aw/popsize
  respmean1
  table(respmean1)

  # ttt generated to use as merging id.
  ttt  <<-  0

  other_rowvector <<-  cbind(respmean1, finalwgt, piinv, ttt)
  other_rowvector # respmean1 = 0.9990459

      # Next, des2-des35
      weighted_counts <<-  function(i) {
          b  <<-  NULL
          a  <<-  table(i)
          aw <<-  as.integer(a[2] * finalwgt)
          b  <<-  aw/popsize
          print(b)
        }

  mylist <<-  sapply(vvv, weighted_counts)
  mylist <<-  as.data.frame(mylist)
  mylist

  # Transpose to arrange in row vector format.
  temp <<- data.frame(t(mylist))

  # Drop ref
  temp_remove  <<- c("des1", "finalwgt", "piinv")
  temp <<- temp %>%
        dplyr::select(- one_of(temp_remove))

  temp <<- cbind(temp, other_rowvector)
  temp <<- temp %>%
          relocate(n, .before = des2)
  names(temp)

  # Rename variables "respmean",
  colnames(temp)  <<- c(paste0("respmean", 1:numcat),
                      "finalwgt", "piinv", "ttt")
  names(temp)
  head(temp)
  # View(temp)
  # SAVE
  respmean_row_vector <<-  temp
  # Merge Step 1 out of 2
  temp_  <<-  full_join( popmean_row_vector,
                      respmean_row_vector) %>%
            relocate(ttt, .after = last_col())
  # des_pop_respmean corresponds to finalfile in SAS code.
  des_pop_respmean  <<- full_join(vvv, temp_) %>%
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
}

# View(des_pop_respmean[1:20,])

# des_pop_respmean  <<-  des_pop_respmean %>%
#           mutate( responsesamp1 = 1 )

#H-----------------------------------
##>> 3 fn_overall_r_indicator_3 ----
#H-----------------------------------

fn_overall_r_indicator_3 <-  function() {

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
  names(temp)
  dim(temp)
  glimpse(temp)
  # head(temp)
  # View(temp[1:100, ])

  gh <<-  cbind(des_pop_respmean, temp)
  gh <<-  gh %>%
        relocate(c(responsesamp1, seq),
          .after = last_col())
  glimpse(gh)

  names(gh) ; dim(gh) ; class(gh)
  # View(gh[1:100, ])

  # removeobjects <- c("des", "respmean", "popmean", "rsam", "psam")
  # rm(list = Filter(exists, removeobjects))

  cat("--- End of Pre-matrix ---")
}

#H-----------------------------------
##>> 4 fn_overall_r_indicator_4 ----
#H-----------------------------------

fn_overall_r_indicator_4 <-  function() {

  #H----------------------------------------
  ## Matrix
  ## H. Use gh, convert to Matrix  ----
  #H----------------------------------------
  ls()
  #  "gh"   "numcat"  "popsize" "resppop"
  # View(gh)
  # Check, resppop (SK=1045581, NS=1047084)
  resppop ; popsize

  # converting the data frame into matrix
  x <<-  as.matrix(gh)
  class(x)

  colnames(x)
  rownames(x[c(1:5), ])  # first 5 rows

  # Prep :
  rsam_col    <<-  c(paste0("rsam", 1:numcat))
  psam_col    <<-  c(paste0("psam", 1:numcat))
  des_col     <<-  c(paste0("des",  1:numcat))
  popmean_col <<-  c(paste0("popmean", 1:numcat))
  rx          <<-  x[, rsam_col]
  px          <<-  x[, psam_col]
  r           <<-  x[, des_col]

  # in row vector
  zp <<-  t(x[1, popmean_col])
  nf <<-  x[, "responsesamp1"]

  # weightf changed to piinv on 12/01/2022.
  # weightf <<-  as.matrix(x[, "finalwgt"])
  weightf   <<-  as.matrix(x[, "piinv"])
  class(weightf) ; head(weightf)
    head(nf)

    from  <<-  numcat - 2
    from

    head(rx[1:3, 1:3]) ; head(rx[1:3, from:numcat])
    head(px[1:3, 1:3]) ; head(px[1:3, from:numcat])
    head(zp) ;  names(zp)
    # View(zp) ;

    #         rsam34       rsam35
    #   -0.000894592 -0.006574091

  #H--------------------------------------
  ##>> Calculate propensity scores  ----
  #H--------------------------------------
  # element-wise multiplication: *
  # mathematical(matrix) multiplication: %*%
  # View(px)

  # Error: non-conformable arrays
  # xxp  <<-  t(px) * px # Error!
  # View(xxp)

  xxp  <<-   t(px) %*% px   # psam
  xxr  <<-   t(rx) %*% rx   # rsam

  # zp is row vector.
  head(t(zp))
  zzp  <<-   t(zp) %*% zp  # fixed
    # head(xxp[1:6, 1:6]) # ok
    head(xxr[1:6, 1:6]) # ??
    head(zzp[1:6, 1:6]) # ok

  yyyp  <<-  (popsize/resppop) * xxp
  yyyr  <<-  (popsize/resppop) * xxr
  ttp   <<-   popsize * zzp  # fixed
    head(yyyp[1:6, 1:6])
    head(yyyr[1:6, 1:6])
    head(ttp[1:6, 1:6])  # ok

  gzpop <<-  yyyp + ttp
  gzmix <<-  yyyr + ttp
    head(gzpop[1:6, 1:6])
    # head(gzmix[1:6, 1:6])

  # computes the Moore-Penrose Generalized
  # INVerse of matrix
  library("MASS")
  bbp   <<-  ginv(gzpop)
  bbmix <<-  ginv(gzmix)
    head(bbp[1:6, 1:6])
    head(bbmix[1:6, 1:6])

  # column vectors: weightf, nf
  cnf    <<-  weightf * nf
    head(cnf) ;
    # View(cnf)

  ccw    <<-  t(r) %*% cnf  # Not element-wise
    head(ccw)
  ddpopw <<-  bbp   %*% ccw
  ddmix  <<-  bbmix %*% ccw

  p1  <<-  "The beta coefficients, ddpopw are:"
    head(ddpopw, n = 3)
    # [1,] 0.7697811
    # [2,] 0.4834853
    # [3,] 0.1480614

  p2  <<-  "ddmix are:"
    head(ddmix, n = 3)
    # [1,] 0.7593768
    # [2,] 0.5054273
    # [3,] 0.1547809
    #  View(ddpopw)

  roipop <<-  r %*% ddpopw
  roimix <<-  r %*% ddmix

  # column vector. To be saved later.
  prop_pop  <<-  roipop
  prop_mix  <<-  roimix

  # If matrix errors are present, one may see below:
  # Error: cannot allocate vector of size xxxx.x Gb
  # Remove objects where necessary.
  gc() ; memory.limit()
  # saves the data into files with names:
  # prop_pop from roipop
  # prop_mix from roimix,
  # Then, append
  # View(prop_mix)

  #H--------------------------------------
  ## I. Compute R-indicators ----
  #H--------------------------------------
  ee1b <<-  weightf * roimix
  ee1c <<-  weightf * roipop
      head(ee1c) ; head(weightf)
  s2term11b <<-  (1/popsize) * colSums(ee1b)
  s2term11c <<-  (1/popsize) * colSums(ee1c)
  s2term21  <<-  (1/popsize) * colSums(weightf)
     s2term11b ; s2term11c ; s2term21

  s2T11b    <<-  (popsize/(popsize-1)) *
                (s2term11b-(s2term21 * s2term21))
  s2T11c    <<-  (popsize/(popsize-1)) *
               (s2term11c-(s2term21 * s2term21))
  variances_ <<-  "The variances are:"
  (variances  <<-   c(s2T11b, s2T11c))

  r_ind1b <<-  1 - 2 * sqrt(s2T11b)
  r_ind1c <<-  1 - 2 * sqrt(s2T11c)

  R_indicators_ <<-  "And the R-indicators are:"
  R_indicators  <<-  c(r_ind1b, r_ind1c)
  R_indicators
  # (R_indicator_mixed  <<- R_indicators[1])
  # (R_indicator_popbased <<- R_indicators[2])
  cat("--- End of Overall R-indicators ---")
}

#H-----------------------------------
##>> 5 fn_save_propensity_scores ----
#H-----------------------------------

# fn_save_propensity_scores <-  function() {
#   #H--------------------------------------
#   ## J. Compute R-indicators ----
#   #H--------------------------------------

# }

### End ###