# functions to calculate performance metrics

# SSB metrics
get_ssb_metrics <- function(ssb = NULL, bmsy = NULL, nprojyrs = 40) {
  SSBmsy <- bmsy
  fyear <- length(ssb) - nprojyrs + 1
  shortyrs <- fyear:(fyear+5) 
  longyrs <- (fyear+20):length(ssb) 
  projyrs <- fyear:length(ssb)
  s_ssb_ssbmsy <- ssb[shortyrs]/SSBmsy
  l_ssb_ssbmsy <- ssb[longyrs]/SSBmsy
  metrics <- list(
    # s_is_less_01_bmsy = ifelse(any(ssb[shortyrs]<0.1*SSBmsy),1,0),
    # s_is_less_05_bmsy = ifelse(any(ssb[shortyrs]<0.5*SSBmsy),1,0),
    # s_is_ge_bmsy = ifelse(any(ssb[shortyrs]>=SSBmsy),1,0),
    s_n_less_01_bmsy = length(which(ssb[shortyrs]<0.1*SSBmsy)),
    s_n_less_05_bmsy = length(which(ssb[shortyrs]<0.5*SSBmsy)),
    s_n_ge_bmsy = length(which(ssb[shortyrs]>=SSBmsy)),
    
    # l_is_less_01_bmsy = ifelse(any(ssb[longyrs]<0.1*SSBmsy),1,0),
    # l_is_less_05_bmsy = ifelse(any(ssb[longyrs]<0.5*SSBmsy),1,0),
    # l_is_ge_bmsy = ifelse(any(ssb[longyrs]>=SSBmsy),1,0),
    l_n_less_01_bmsy = length(which(ssb[longyrs]<0.1*SSBmsy)),
    l_n_less_05_bmsy = length(which(ssb[longyrs]<0.5*SSBmsy)),
    l_n_ge_bmsy = length(which(ssb[longyrs]>=SSBmsy)),
    
    s_avg_ssb_ssbmsy = mean(s_ssb_ssbmsy,na.rm=TRUE),
    l_avg_ssb_ssbmsy = mean(l_ssb_ssbmsy,na.rm=TRUE)
  )
  return(metrics)
}

# catch metrics
get_catch_metrics <- function(catch = NULL, msy = NULL, nprojyrs = 40) {
  msy <- msy
  #msy <- 5000  #GF hardwire a dummy value now
  fyear <- length(catch) - nprojyrs + 1
  shortyrs <- fyear:(min(c(length(catch),fyear+5)))
  longyrs <- (min(c(fyear+20,length(catch)))):length(catch)
  projyrs <- fyear:length(catch)
  
  s_catch_msy <- catch[shortyrs]/msy
  l_catch_msy <- catch[longyrs]/msy
  l_is_g_msy <- ifelse(l_catch_msy>1, 1, 0)
  rollsum_g_msy <- RcppRoll::roll_sum(l_is_g_msy, 3)
  l_prop_g_msy_2_of_3 <- sum(rollsum_g_msy>2)/length(rollsum_g_msy)
  
  metrics <- list(
    s_avg_catch = mean(catch[shortyrs],na.rm=TRUE),
    l_avg_catch = mean(catch[longyrs],na.rm=TRUE),
    s_avg_catch_msy = mean(s_catch_msy,na.rm=TRUE),
    l_avg_catch_msy = mean(l_catch_msy,na.rm=TRUE),    
    s_sd_catch = sd(catch[shortyrs],na.rm=TRUE),
    l_sd_catch = sd(catch[longyrs],na.rm=TRUE),
    l_iav_catch = sqrt(sum(diff(catch[longyrs])^2)/(length(longyrs-1)))/(sum(catch[longyrs])/length(longyrs)),
    s_iav_catch = sqrt(sum(diff(catch[shortyrs])^2)/(length(shortyrs-1)))/(sum(catch[shortyrs])/length(shortyrs)),
    a_iav_catch = sqrt(sum(diff(catch[projyrs])^2)/(length(projyrs-1)))/(sum(catch[projyrs])/length(projyrs)),
    #s_catch_msy = s_catch_msy,
    #l_catch_msy = l_catch_msy,
    #l_is_g_msy = l_is_g_msy,
    #rollsum_g_msy = rollsum_g_msy,
    l_prop_g_msy_2_of_3 = l_prop_g_msy_2_of_3)
  return(metrics)
} 

#F metrics
get_F_metrics <- function(F = NULL, fmsy = NULL, nprojyrs = 40) {
  Fmsy <- fmsy
  fyear <- length(F) - nprojyrs + 1
  shortyrs <- fyear:(fyear+5)
  longyrs <- (fyear+20):length(F)
  projyrs <- fyear:length(F)
  s_f_fmsy <- F[shortyrs]/Fmsy
  l_f_fmsy <- F[longyrs]/Fmsy
  metrics <- list(
    # s_is_gr_fmsy = ifelse(any(F[shortyrs]>Fmsy),1,0),
    # s_is_less_fmsy = ifelse(any(F[shortyrs]<=Fmsy),1,0),
    s_n_gr_fmsy = length(which(F[shortyrs]>Fmsy)),
    s_n_less_fmsy = length(which(F[shortyrs]<=Fmsy)),

    # l_is_gr_fmsy = ifelse(any(F[longyrs]>Fmsy),1,0),
    # l_is_less_fmsy = ifelse(any(F[longyrs]<=Fmsy),1,0),
    l_n_gr_fmsy = length(which(F[longyrs]>Fmsy)),
    l_n_less_fmsy = length(which(F[longyrs]<=Fmsy)),

    s_avg_f_fmsy = mean(s_f_fmsy,na.rm=TRUE),
    l_avg_f_fmsy = mean(l_f_fmsy,na.rm=TRUE)
  )
  return(metrics)
}

