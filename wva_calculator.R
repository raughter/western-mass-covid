# CDC methodology here: https://www.cdc.gov/nwss/about-data.html#data-method

# Standard, returns vector
cdc_wva <- function(ww){
  ww_log = log(ww)
  baseline = quantile(ww_log, probs=.1)
  stdev = sd(ww_log)
  diff = ww_log-baseline
  WVA_log = diff/stdev
  WVA_lin = exp(WVA_log)
  WVA_level = factor(
    case_when(
      WVA_lin <= 1.5 ~ "Minimal",
      WVA_lin > 1.5 & WVA_lin <=3 ~ "Low",
      WVA_lin > 3 & WVA_lin <= 4.5 ~ "Moderate",
      WVA_lin > 4.5 & WVA_lin <= 8  ~ "High",
      WVA_lin > 8 ~ "Very High"), 
    levels=c("Minimal", "Low", "Moderate", "High", "Very High"))
  return(WVA_level)}

# Weighted WVA 
wtd_wva <- function(ww, wts){
  ww_log = log(ww)
  baseline = as.numeric(Hmisc::wtd.quantile(ww_log, weights=wts, probs=.1, normwt = TRUE))
  # as.numeric(quantile(ww_log, probs=.1))
  stdev = sqrt(Hmisc::wtd.var(ww_log, wts))
  # sd(ww_log)
  diff = ww_log-baseline
  WVA_log = diff/stdev
  WVA_lin = exp(WVA_log)
  WVA_level = factor(
    case_when(
      WVA_lin <= 1.5 ~ "Minimal",
      WVA_lin > 1.5 & WVA_lin <=3 ~ "Low",
      WVA_lin > 3 & WVA_lin <= 4.5 ~ "Moderate",
      WVA_lin > 4.5 & WVA_lin <= 8  ~ "High",
      WVA_lin > 8 ~ "Very High"), 
    levels=c("Minimal", "Low", "Moderate", "High", "Very High"))
  return(WVA_level)}

# returns WW threshold for a given raw wva score
ww_threshold <- function(ww, wva_score){
  as.numeric(exp(log(wva_score) * sd(log(ww)) + quantile(log(ww), probs=.1)))
}

# returns weighted WW threshold given a ww vector, a raw wva score, and a weights vector
wtd_ww_threshold <- function(ww, wva_score, wts){
  library(Hmisc)
  as.numeric(exp(log(wva_score) * sqrt(wtd.var(log(ww), wts)) + wtd.quantile(log(ww), weights=wts, probs=.1, normwt = TRUE)))
}


## using above, returns vector of thresholds for a vector of raw wva scores
# defaults to CDC levels
ww_threshold_breaks <- function(ww, level_values = c(0, 1.5, 3, 4.5, 8, Inf)){
  sapply(level_values, function(wva_score) ww_threshold(ww, wva_score))
}

wtd_ww_threshold_breaks <- 
  function(ww, wts, level_values = c(0, 1.5, 3, 4.5, 8, Inf)){
  sapply(level_values, function(wva_score) wtd_ww_threshold(ww, wva_score, wts))
}


# returns raw CDC score
wva_raw <-
  function(ww){
    ww_log = log(ww)
    baseline = quantile(ww_log, probs=.1)
    stdev = sd(ww_log)
    diff = ww_log-baseline
    WVA_log = diff/stdev
    WVA_lin = exp(WVA_log)
    return(WVA_lin)}

# returns weighted raw CDC score
wtd_wva_raw <-
  function(ww, wts){
    library(Hmisc)
    ww_log = log(ww)
    baseline = as.numeric(Hmisc::wtd.quantile(ww_log, weights=wts, probs=.1, normwt = TRUE))
    stdev = sqrt(Hmisc::wtd.var(ww_log, wts))
    diff = ww_log-baseline
    WVA_log = diff/stdev
    WVA_lin = exp(WVA_log)
    return(WVA_lin)}

# Create smooth_operator function as described above, for individual sewer systems
smooth_operator <- function(df, time_years){
  # filter sample to western mass and # of years
  sample = df %>% filter(date > max(date) - years(time_years) & wmass)
  # Set span so it = 6 weeks -- this means the final point is influenced by the 3 weeks prior
  weeks_between <- interval(min(sample$date), max(sample$date)) / weeks(1)
  fit_span = 6/weeks_between
  
  # Generate the loess smooth for the regional data
  fit <- loess(log(avg_7d_covid_conc) ~ as.numeric(date),
               span=fit_span,
               data= sample,
               weights = pop_day)
  ### Create new dataset with smooth for plotting
  dfnew <- sample
  smooth <- predict(fit, newdata = dfnew, se=T)
  dfnew$y_smoothed <- exp(smooth[[1]])
  dfnew$se <- exp(smooth[[2]])
  dfnew$se_Low <- exp(log(dfnew$y_smoothed) - 1.96 * log(dfnew$se))
  dfnew$se_High <- exp(log(dfnew$y_smoothed) + 1.96 * log(dfnew$se))
  ### get wva levels
  thresholds_reg <- wtd_ww_threshold_breaks(dfnew$avg_7d_covid_conc, wts=dfnew$pop_day)
  # get wva for smooth
  dfnew$wva_lvl <- cut(dfnew$y_smoothed, breaks = thresholds_reg, labels = c("Minimal", "Low", "Moderate", "High", "Very High"))
  # get wva for points
  dfnew$wva_point <- cut(dfnew$avg_7d_covid_conc, breaks = thresholds_reg, labels = c("Minimal", "Low", "Moderate", "High", "Very High"))
  dfnew$wva_score <- round(wtd_wva_raw(dfnew$y_smoothed, wts = dfnew$pop_day), 1)
  return(dfnew)
}


# as.numeric(quantile(ww_log, probs=.1))

### Not currently used -----
# # Rolling wva -- for rolling functions, returns single value (last item on vector)
# cdc_wva_single <- function(ww){
#   WVA_lin = last(
#     exp(
#       ( log(ww) - quantile(log(ww), probs=.1) ) / sd(log(ww))
#     )
#   )
#   WVA_level = factor(
#     case_when(
#       WVA_lin <= 1.5 ~ "Minimal",
#       WVA_lin > 1.5 & WVA_lin <=3 ~ "Low",
#       WVA_lin > 3 & WVA_lin <= 4.5 ~ "Moderate",
#       WVA_lin > 4.5 & WVA_lin <= 8  ~ "High",
#       WVA_lin > 8 ~ "Very High"), 
#     levels=c("Minimal", "Low", "Moderate", "High", "Very High"))
#   return(WVA_level)}
# 
# # single value of wva_raw for rolling functions
# wva_raw_single <-
#   function(ww){
#     weighted_mean_log = log(ww)
#     baseline = quantile(weighted_mean_log, probs=.1)
#     stdev = sd(weighted_mean_log)
#     diff = weighted_mean_log-baseline
#     WVA_log = diff/stdev
#     WVA_lin = last(exp(WVA_log))
#     return(WVA_lin)}
# 
# 
# # returns percentile rank
# wva_perc <-
#   function(ww){
#     perc = percent_rank(ww)
#     return(perc)}
# 
# wva_perc_single <-
#   function(ww){
#     perc = percent_rank(ww)
#     return(last(perc))}
# 
# # returns risk level if upper threshold for Low status is a raw CDC level of 2 (rather than 3)
# wva_2_is_Low <- function(ww){
#   weighted_mean_log = log(ww)
#   baseline = quantile(weighted_mean_log, probs=.1)
#   stdev = sd(weighted_mean_log)
#   diff = weighted_mean_log-baseline
#   WVA_log = diff/stdev
#   WVA_lin = exp(WVA_log)
#   WVA_level = factor(
#     case_when(
#       WVA_lin <= 1.5 ~ "Minimal",
#       WVA_lin > 1.5 & WVA_lin <=2 ~ "Low",
#       WVA_lin > 2 & WVA_lin <= 4.5 ~ "Moderate",
#       WVA_lin > 4.5 & WVA_lin <= 8  ~ "High",
#       WVA_lin > 8 ~ "Very High"), 
#     levels=c("Minimal", "Low", "Moderate", "High", "Very High"))
#   return(WVA_level)}
