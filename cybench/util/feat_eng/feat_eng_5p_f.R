# Monique Oliveira - moniquepgoliveira@gmail.com

# Libraries ---------------------------------------------------------------
libraries <- c("here", "tidyverse", "data.table")

lapply(libraries, require, character.only = TRUE)

# Functions ---------------------------------------------------------------
truncate_series <- function(x, calendar){
  
  x_mod <- merge(x, calendar, all = TRUE)
  x_mod <- x_mod[, lead_date := (sos_date - 30)][, keep := (date >= lead_date) & (date <= eos_date)]
  x_mod <- x_mod[keep == TRUE][, keep := NULL]
  
  # Create date
  x_mod <- x_mod[, !c("cutoff_date", "season_window_length", "lead_date",
                      "eos_date", "sos_date")]
  
  return(x_mod)
  
}

dates_dvs <- function(x, tbase, tupper, thresholds, calendar){

  # Returns date for each predefined threshold of accumulated thermal 
  # time in the cycle
  thresholds_name <- c("dvs1", "dvs2")
  
  # Version 1: tt larger than tbase and lower than tupper
  # Does not differentiate between "never crossed the threshold because 
  # temperatures were low"
  x[, tt := ifelse(tavg - tbase < 0, 0, 
                   ifelse(tavg > tupper, tupper - tbase, tavg - tbase))]
  
  # Calculates accumulated thermal time per season
  x <- x[order(adm_id, year, date)][, tt_sum := cumsum(tt), keyby = c("adm_id", "year")]

  # Prepare threshold columns
  x[, (thresholds_name) := as.list(thresholds
                                      # 0.1*max(tt), 0.25*max(tt), 1/3)*max(tt), 0.5*max(tt), 800
  ),
  keyby = adm_id]
  x[, (thresholds_name) := lapply(.SD, as.numeric), .SDcols = thresholds_name]

  # Identifies dates closest to the thresholds
  x_tt <- data.table::melt(x, 
                           id.vars = c("adm_id", "year", "date", "tt", "tt_sum"),
                           measure.vars = thresholds_name,
                           variable.name = "accum_tt")
  x_tt[, dif:= abs(tt_sum-value)][, value := NULL]
  x_tt <- x_tt[, flag := (dif == min(dif)), keyby = c("adm_id", "year", "accum_tt")]
  x_tt <- x_tt[flag == TRUE]
  
  # Retrieves the first row in each group. Likely in case dif leads to more
  # than one date. I don't remember.
  x_tt <- x_tt[x_tt[, .I[flag][1], by = .(adm_id, accum_tt, year)][, V1]]
  x_tt <- x_tt[, .(adm_id, date, tt, accum_tt, year)]
  x_tt <- unique(x_tt, by = c("date", "adm_id", "year"))
  x_tt <- data.table::dcast(x_tt, adm_id + year ~ accum_tt, value.var = "date")
  setkey(x_tt, NULL)
  
  # Add periods that surround DVS change dates
  x_dates <- merge(x_tt, calendar[, !c("cutoff_date", "season_window_length")], 
                   all = TRUE)
  setnames(x_dates, old = "sos_date", new = "dvs0")
  setnames(x_dates, old = "eos_date", new = "eos")
  
  x_dates <- x_dates[, p0_s := dvs0 - 30][, p0_e := dvs0][, p1_s := dvs0 - 7][, p1_e := dvs0 + 7]
  x_dates <- x_dates[, p2_s := dvs0][, p2_e := dvs1][, p3_s := dvs1 - 7][, p3_e := dvs1 + 7]
  x_dates <- x_dates[, p4_s := dvs1][, p4_e := dvs2][, p5_s := dvs2 - 7][, p5_e := max(dvs2 + 7, eos)]
  x_dates <- x_dates[, !c("dvs0", "dvs1", "dvs2", "eos")]
  
  x_periods <- data.table::melt(x_dates, 
                                id.vars = c("adm_id", "year"),
                                measure.vars = setdiff(colnames(x_dates), c("adm_id", "year")),
                                variable.name = "divisions")[order(adm_id, year, divisions)]
  x_periods <- x_periods[, c("period", "moment") := tstrsplit(divisions, "_")][, divisions := NULL]
  x_periods <- data.table::dcast(x_periods, adm_id + year + period ~ moment, value.var = "value")
  
  return(x_periods)

}

feat_eng <- function(x, periods_dates, var_, funcs){
  
  # Returns aggregated time-series for each threshold of tt of the variable
  # periods_dates: ID, date and correspondent tt
  # var_: variable to which dataset refers
  
  # Create groups by thresholded period
  
  cols <- c("adm_id", "year", "date", var_)
  x_mod <- merge(x[, ..cols], periods_dates, by = c("adm_id", "year"), all = TRUE,
                 allow.cartesian=TRUE)
  x_mod <- x_mod[order(adm_id, year, date)][, period_ := ((date >= s) & (date <= e))][period_ == TRUE]
  x_mod <- x_mod[, !c("e", "s", "period_")]
  
  setnames(x_mod, old = var_, new = "value")

  x_all <- data.table(unique(x_mod[, c("adm_id", "year")]))
  setkey(x_all, NULL) # Remove any grouping
  
  # Generates aggregation
  for (func_name in names(funcs)) {
    # Here we generate the aggregated series for each period and
    # join them to the aggregation for the whole season
    x_periods <- x_mod[, .(funcs[[func_name]](value, na.rm=TRUE)), 
                       by = c("adm_id", "year", "period")]
    x_periods <- x_periods[, V1 := round(V1, 4)]
    x_periods[, new_name := paste(var_, func_name,
                                  period, sep = "_")][, period := NULL]
    x_periods <- data.table::dcast(x_periods, 
                                   adm_id + year ~ new_name, value.var = "V1")
    setkey(x_periods, NULL)
    
    # Whole season
    x_full <- x_mod[, .(funcs[[func_name]](value, na.rm=TRUE)), 
                    by = c("adm_id", "year")]
    setkey(x_full, NULL)
    x_full <- x_full[, V1 := round(V1, 4)]
    x_full[, new_name := paste(var_, func_name, "season", sep = "_")]
    x_full <- data.table::dcast(x_full, adm_id + year ~ new_name, value.var = "V1")
    setkey(x_full, NULL)
    
    temp <- cbind(x_periods, x_full[, adm_id := NULL][, year := NULL])
    x_all <- merge(x_all, temp, all = TRUE)
    setkey(x_all, NULL)
    
  }
  
  return(x_all)
  
}

agg_funs <- function(variable_){
  
  count_extreme <- function(x, na.rm = FALSE) {
    
    sd_ <- sd(x, na.rm = na.rm)
    avg_ <- mean(x, na.rm = na.rm)
    thresh_inf <- avg_ - sd_
    thresh_up <- avg_ + sd_
    count_ <- as.numeric(sum((x > thresh_up) | (x < thresh_inf), na.rm = na.rm))
    
    return(count_)
  }
  
  if(variable_ == "tavg"){
    funcs <- list("avg" = mean, "min" = min, "max" = max)
  } else if (variable_ == "tmax"){
    funcs <- list("avg" = mean, "max" = max, "count" = count_extreme)
  } else if (variable_ == "tmin"){
    funcs <- list("avg" = mean, "min" = min, "count" = count_extreme)
  } else if (variable_ == "rad"){
    funcs <- list("avg" = mean, "sum" = sum)
  } else if (variable_ == "prec"){
    funcs <- list("sum" = sum, "sd" = sd, "count" = count_extreme)
  } else if (variable_ == "et0"){
    funcs <- list("sum" = sum, "max" = max)
  } else if (variable_ == "cwb"){
    funcs <- list("avg" = mean, "min" = min)
  } else if (variable_ == "fpar"){
    funcs <- list("avg" = mean, "max" = max)
  } else if (variable_ == "ndvi"){
    funcs <- list("avg" = mean)
  } else if (variable_ == "ssm"){
    funcs <- list("avg" = mean, "count" = count_extreme)
  } else if (variable_ == "rsm"){
      funcs <- list("avg" = mean, "min" = min)
  } else {
    funcs <- NULL
  }
  
  return(funcs)
  
}

generate_set <- function(x, periods_dates){
  
  list_out <- list()
  
  cols <- c("adm_id", "year", "date", "crop_name", "date", "doy")
  var_cols <- colnames(x)
  var_cols <- var_cols[!var_cols %in% cols]
  
  list_out <- list()
  
  var_ <- var_cols[1]
  for(var_ in var_cols){
    
    funcs <- agg_funs(var_)
    temp <- feat_eng(x, periods_dates, var_, funcs)
    temp <- data.table::melt(temp, id.vars = c("adm_id", "year"))
    
    list_out[[var_]] <- temp
    
  }
  
  list_out <- rbindlist(list_out)
  list_out <- data.table::dcast(list_out, adm_id + year ~ variable,
                                 value.var = "value")
  
  return(list_out)
  
}

