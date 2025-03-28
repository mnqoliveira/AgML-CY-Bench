# Monique Oliveira - moniquepgoliveira@gmail.com

# Libraries ---------------------------------------------------------------
libraries <- c("here", "tidyverse", "data.table")

lapply(libraries, require, character.only = TRUE)

# Functions ---------------------------------------------------------------
truncate_series <- function(x, calendar, keep_date=FALSE){
  
  x_mod <- merge(x, calendar, all = TRUE)
  x_mod <- x_mod[, keep := (date >= sos_date) & (date <= eos_date)]
  x_mod <- x_mod[keep == TRUE][, keep := NULL]
  
  # Create das
  x_mod[, das := as.numeric(date - sos_date) + 1, by = c("adm_id", "year")]
  x_mod <- x_mod[, !c("sos_date", "eos_date", "cutoff_date",
                      "season_window_length")]
  
  if(!keep_date){
      
      x_mod <- x_mod[, !c("date")]
      
  }
  
  return(x_mod)
  
}

def_tt <- function(x, tbase, tupper, thresholds){
  
  # Returns das for each predefined threshold of accumulated thermal 
  # time in the cycle
  thresholds_name <- paste0("tt", thresholds)
  
  # Version 1: tt larger than tbase and lower than tupper
  # Does not differentiate between "never crossed the threshold because 
  # temperatures were low"
  x[, tt := ifelse(tavg - tbase < 0, 0, 
                   ifelse(tavg > tupper, tupper - tbase, tavg - tbase))]
  
  # Calculates accumulated thermal time per season
  x <- x[order(adm_id, das)][, tt := cumsum(tt), keyby = c("adm_id", "year")]
  # x <- x[order(adm_id, das)][, ttsum := cumsum(tt), keyby = c("adm_id", "year")]
  
  # Prepare threshold columns
  x[, (thresholds_name) := as.list(thresholds
                                      # 0.1*max(tt), 0.25*max(tt), 1/3)*max(tt), 0.5*max(tt), 800
  ),
  keyby = adm_id]
  x[, (thresholds_name) := lapply(.SD, as.numeric), .SDcols = thresholds_name]
  
  # Identifies dates closest to the thresholds
  x_tt <- data.table::melt(x, id.vars = c("adm_id", "year", "das", "tt"),
                           measure.vars = thresholds_name, 
                           variable.name = "accum_tt")
  x_tt[, dif:= abs(tt-value)][, value := NULL]
  x_tt[, flag := (dif == min(dif)), keyby = c("adm_id", "year", "accum_tt")]
  
  x_tt <- x_tt[flag == TRUE]
  # Retrieves the first row in each group. Likely in case dif leads to more
  # than one date. I don't remember.
  x_tt <- x_tt[x_tt[, .I[flag][1], by = .(adm_id, accum_tt, year)][, V1]]
  x_tt <- x_tt[, .(adm_id, das, tt, accum_tt, year)]
  x_tt <- unique(x_tt, by = c("das", "adm_id", "year"))
  setkey(x_tt, NULL)
  
  return(x_tt)
  
}

feat_eng <- function(x, tt_das, var_, funcs){
  
  # Returns aggregated time-series for each threshold of tt of the variable
  # tt_das: ID, das and correspondent tt
  # var_: variable to which dataset refers
  
  # Create groups by thresholded period
  
  cols <- c("adm_id", "year", "das", var_)
  x_mod <- merge(x[, ..cols], tt_das, by = c("adm_id", "year", "das"), all = TRUE)
  x_mod <- x_mod[order(adm_id, year, das)]
  x_mod[, accum_tt := as.numeric(accum_tt)][, tt := NULL]
  # We will ascribe the groups from last day to first, so we use 99 as a
  # placeholder for group code to the last day if it exceeded the larger threshold,
  # carry the value until the previous group code is found and
  # end x_mod with ID, das, tt value and accum_tt group
  x_mod[, accum_tt := ifelse(das == max(das) & is.na(accum_tt), 99, accum_tt),
        keyby = c("adm_id", "year")]
  x_mod[, accum_tt := zoo::na.locf(accum_tt, fromLast=TRUE)]
  # Optional to truncate the cycle further as a function of thermal time 
  # instead of the calendar. Comment to remove this and include the final days as
  # an additional period.
  x_mod <- x_mod[accum_tt != 99]
  setnames(x_mod, old = var_, new = "value")

  x_all <- data.table(unique(x_mod[, c("adm_id", "year")]))
  setkey(x_all, NULL) # Remove any grouping
  
  if (!is.null(tt_das)){

    if(var_ == "tavg"){
      # We then add, just for analysis, which day of the cycle corresponds
      # to the date of each threshold
      tt_das <- data.table::dcast(tt_das, adm_id + year ~ accum_tt,
                                  value.var = "das")[, c("adm_id", "year") := NULL]
      x_all <- cbind(x_all, tt_das)
    }
  }
  
  rm(tt_das)
  
  # Generates aggregation
  for (func_name in names(funcs)) {
    # Here we generate the aggregated series for each period and
    # join them to the aggregation for the whole season
    x_periods <- x_mod[, .(funcs[[func_name]](value, na.rm=TRUE)), 
                       by = c("adm_id", "year", "accum_tt")]
    x_periods[, new_name := paste(var_, func_name,
                                  accum_tt, sep = "_")][, accum_tt := NULL]
    x_periods <- data.table::dcast(x_periods, 
                                   adm_id + year ~ new_name, value.var = "V1")
    setkey(x_periods, NULL)
    
    # Whole season
    x_full <- x_mod[, .(funcs[[func_name]](value, na.rm=TRUE)), 
                    by = c("adm_id", "year")]
    setkey(x_full, NULL)
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
  
  if(variable_ == "tavg"){
    funcs <- list("avg" = mean, "min" = min, "max" = max)
  } else if (variable_ == "tmax"){
    funcs <- list("avg" = mean, "max" = max)
  } else if (variable_ == "tmin"){
    funcs <- list("avg" = mean, "min" = min)
  } else if (variable_ == "rad"){
    funcs <- list("avg" = mean, "sum" = sum)
  } else if (variable_ == "prec"){
    funcs <- list("sum" = sum, "sd" = sd)
  } else if (variable_ == "et0"){
    funcs <- list("sum" = sum, "max" = max)
  } else if (variable_ == "cwb"){
    funcs <- list("avg" = mean, "min" = min)
  } else if (variable_ == "fpar"){
    funcs <- list("avg" = mean, "max" = max)
  } else if (variable_ == "ndvi"){
    funcs <- list("avg" = mean)
  } else if (variable_ == "ssm"){
    funcs <- list("avg" = mean)
  } else if (variable_ == "rsm"){
      funcs <- list("avg" = mean, "min" = min)
  } else {
    funcs <- NULL
  }
  
  return(funcs)
  
}

generate_set <- function(x, tt_das){
  
  list_out <- list()
  
  cols <- c("adm_id", "year", "das", "crop_name", "date", "doy")
  var_cols <- colnames(x)
  var_cols <- var_cols[!var_cols %in% cols]
  
  list_out <- list()
  
  var_ <- var_cols[3]
  for(var_ in var_cols){
    
    funcs <- agg_funs(var_)
    temp <- feat_eng(x, tt_das, var_, funcs)
    temp <- data.table::melt(temp, id.vars = c("adm_id", "year"))
    
    list_out[[var_]] <- temp
    
  }
  
  list_out <- rbindlist(list_out)
  list_out <- data.table::dcast(list_out, adm_id + year ~ variable,
                                 value.var = "value")
  
  return(list_out)
  
}

