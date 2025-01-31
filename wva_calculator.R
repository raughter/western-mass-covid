# CDC methodology here: https://www.cdc.gov/nwss/about-data.html#data-method

# Standard, returns vector
cdc_wva <- function(x){
  weighted_mean_log = log(x)
  baseline = quantile(weighted_mean_log, probs=.1)
  stdev = sd(weighted_mean_log)
  diff = weighted_mean_log-baseline
  WVA_log = diff/stdev
  WVA_lin = exp(WVA_log)
  WVA_level = factor(
    case_when(
      WVA_lin <= 1.5 ~ "minimal",
      WVA_lin > 1.5 & WVA_lin <=3 ~ "low",
      WVA_lin > 3 & WVA_lin <= 4.5 ~ "moderate",
      WVA_lin > 4.5 & WVA_lin <= 8  ~ "high",
      WVA_lin > 8 ~ "very high"), 
    levels=c("minimal", "low", "moderate", "high", "very high"))
  return(WVA_level)}

# house level (low-medium threshold is 2 instead of 3)




# For rolling functions, returns single value (last item on vector)
cdc_wva_single <- function(x){
  WVA_lin = last(
    exp(
      ( log(x) - quantile(log(x), probs=.1) ) / sd(log(x))
    )
  )
  WVA_level = factor(
    case_when(
      WVA_lin <= 1.5 ~ "minimal",
      WVA_lin > 1.5 & WVA_lin <=3 ~ "low",
      WVA_lin > 3 & WVA_lin <= 4.5 ~ "moderate",
      WVA_lin > 4.5 & WVA_lin <= 8  ~ "high",
      WVA_lin > 8 ~ "very high"), 
    levels=c("minimal", "low", "moderate", "high", "very high"))
  return(WVA_level)}

# returns WW threshold for a given raw CDC level
ww_threshold <- function(x, wva_score){
  result = exp(log(wva_score) * sd(log(x)) + quantile(log(x), probs=.1))
  return(as.numeric(result))
}

# ww_threshold(df_wts$weighted_mean, wva_score=1.5)
# returns raw CDC level
wva_raw <-
  function(x){
    weighted_mean_log = log(x)
    baseline = quantile(weighted_mean_log, probs=.1)
    stdev = sd(weighted_mean_log)
    diff = weighted_mean_log-baseline
    WVA_log = diff/stdev
    WVA_lin = exp(WVA_log)
    return(WVA_lin)}

# single value of above for rolling functions
wva_raw_single <-
  function(x){
    weighted_mean_log = log(x)
    baseline = quantile(weighted_mean_log, probs=.1)
    stdev = sd(weighted_mean_log)
    diff = weighted_mean_log-baseline
    WVA_log = diff/stdev
    WVA_lin = last(exp(WVA_log))
    return(WVA_lin)}

# returns percentile rank
wva_perc <-
  function(x){
    perc = percent_rank(x)
    return(perc)}

wva_perc_single <-
  function(x){
    perc = percent_rank(x)
    return(last(perc))}


# returns risk level if upper threshold for Low status is a raw CDC level of 2 (rather than 3)
wva_2_is_low <- function(x){
  weighted_mean_log = log(x)
  baseline = quantile(weighted_mean_log, probs=.1)
  stdev = sd(weighted_mean_log)
  diff = weighted_mean_log-baseline
  WVA_log = diff/stdev
  WVA_lin = exp(WVA_log)
  WVA_level = factor(
    case_when(
      WVA_lin <= 1.5 ~ "minimal",
      WVA_lin > 1.5 & WVA_lin <=2 ~ "low",
      WVA_lin > 2 & WVA_lin <= 4.5 ~ "moderate",
      WVA_lin > 4.5 & WVA_lin <= 8  ~ "high",
      WVA_lin > 8 ~ "very high"), 
    levels=c("minimal", "low", "moderate", "high", "very high"))
  return(WVA_level)}
