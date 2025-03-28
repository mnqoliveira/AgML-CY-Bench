# Monique Oliveira - moniquepgoliveira@gmail.com

# Libraries ---------------------------------------------------------------
libraries <- c("here", "tidyverse", "data.table", "zoo")

lapply(libraries, require, character.only = TRUE)

source(here("feat_eng_2p_f.R"))

# Analyses raw ------------------------------------------------------------
path_ <- "../../data/loaded/"
original_files <- list.files(path_, recursive = TRUE, full.names = TRUE)
original_files <- original_files[!grepl("bkp|features|maize", original_files)]
files_names <- gsub(".csv", "", gsub(".*/", "", original_files))

# Yield -------------------------------------------------------------------
yield_l <- list()
for (it in 1:length(original_files)){
  
  if(grepl("yield", files_names[[it]])){
    
    crop <- str_extract(files_names[[it]], "wheat")
    country <- str_extract(files_names[[it]], "FR|DE|ES")
    
    temp <- fread(original_files[it])
    yield_l[[files_names[[it]]]] <- temp %>%
      mutate(crop = crop, country = country)
    
  }
  
}

yield <- rbindlist(yield_l) %>%
  filter(year >= 2004, year <= 2020)

ggplot() +
  facet_grid(crop ~ country, scales = "free_y") +
  geom_boxplot(data = yield, aes(x = as.factor(year), y = yield), 
               position = "dodge") +
  geom_vline(xintercept = as.factor(2015)) +
  theme(axis.text.x = element_text(angle= 90))

# Metrics -----------------------------------------------------------------
path_file <- "../../output/agmip10/tests_20250327_221514.csv"
path_folder <- "../../output/agmip10/tests_20250327_221514/"

metrics_ <- read.csv(path_file)
metrics <- metrics_ %>%
  mutate(r2 = if_else(r2 < -1, -1, r2),
         score = as.numeric(score), it = NA, features_ = NA) %>%
  pivot_longer(c("normalized_rmse", "mape", "r2", "score"),
               names_to = "metric", values_to = "value") %>%
  mutate(eval = if_else(metric == "score", "oob", "test"),
         metric = if_else(metric == "score", "r2", metric)) %>%
  filter(technique != "skrid", country != "NL", eval != "oob")

ggplot() +
  facet_grid(metric ~ country, scales = "free") +
  geom_col(data = filter(metrics, technique == "rf"), 
           aes(x = source, y = value, fill = eval), position = "dodge") +
  geom_hline(data = filter(metrics, technique == "naive", source == "2periods"), 
           aes(yintercept = value)) 

# Outputs -----------------------------------------------------------------
files_list <- list.files(path_folder, full.names = TRUE)

feat_df_l <- error_l <- train_l <- test_l<- list()
it <- 19
metrics_nofeat <- select(metrics_, -features, -features_)

for (it in 1:nrow(metrics_)){
  
  metrics_[it, "it"] <- it
  
  # Selected features
  if (metrics_[it, "technique"] == "rf"){

    feat_it <- unlist(metrics_[it, "features"])
    feat_it <- unlist(strsplit(gsub("\\[|\\]| |'", "", feat_it), ","))
    feat_df <- data.frame(features = feat_it[1:(length(feat_it)/2)],
                          imp = feat_it[(length(feat_it)/2 + 1):length(feat_it)])
    feat_df <- feat_df %>%
      mutate(imp_cum = cumsum(imp), it = it,
             features = gsub(">|<", "\\.", features)) 
    # %>%
    #   # filter((imp_cum <= 0.5) | (row_number() == 1)) %>%
    #   filter((row_number() <= 15))

    feat_df_l[[it]] <- feat_df

    metrics_[it, "features_"] <- paste(feat_df$features, collapse = ", ")

  }

  if (metrics_[it, "technique"] == "skrid"){

    feat_it <- unlist(metrics_[it, "features"])
    feat_it <- unlist(strsplit(gsub("\\[|\\]| |'", "", feat_it), ","))
    metrics_[it, "features_"] <- paste(feat_it, collapse = ", ")

  }

  if (metrics_[it, "technique"] != "rf"){
    
    next
    
  }
  # Feature values
  run_it <- paste(c(metrics_$crop[it], metrics_$country[it], 
                    metrics_$source[it]), collapse ="_")
  train_path <- grep(paste("train", paste0(run_it, ".csv"), sep = "_"), files_list, value = TRUE)
  test_path <- grep(paste("test", paste0(run_it, ".csv"), sep = "_"), files_list, value = TRUE)
  
  # Filter training sets by most important features
  train <- read.csv(train_path)
  feat <- metrics_[it, "features_"] %>%
    str_split(", ") %>%
    unlist()
  
  temp <- train %>%
    select("adm_id", "year", all_of(feat), any_of(starts_with("tt"))) %>%
    mutate(crop = metrics_$crop[it], country = metrics_$country[it],
           source = metrics_$source[it])
    
  if (any(grepl("tt", colnames(temp)))){
    temp <- temp %>%
      pivot_longer(starts_with(("tt")), names_to = "tt", values_to = "das") %>%
      mutate(tt = as.numeric(gsub("tt", "", tt))) %>%
      group_by(adm_id, year) %>%
      mutate(tt = paste0("p0", rank(tt))) %>%
      ungroup() %>%
      pivot_wider(names_from = "tt", values_from = "das")
  }
  
  train_l[[it]] <- temp
  
  test <- read.csv(test_path)
  temp <- test %>%
    select("adm_id", "year", all_of(feat), any_of(starts_with("tt"))) %>%
    mutate(crop = metrics_$crop[it], country = metrics_$country[it],
           source = metrics_$source[it])
  
  if (any(grepl("tt", colnames(temp)))){
    temp <- temp %>%
      pivot_longer(starts_with(("tt")), names_to = "tt", values_to = "das") %>%
      mutate(tt = as.numeric(gsub("tt", "", tt))) %>%
      group_by(adm_id, year) %>%
      mutate(tt = paste0("p0", rank(tt))) %>%
      ungroup() %>%
      pivot_wider(names_from = "tt", values_from = "das")
  }
  
  test_l[[it]] <- temp
  
  # Errors
  run_it <- paste(c(metrics_$crop[it], metrics_$country[it], ".*", 
                    metrics_$source[it]), collapse ="_")
  error_files <- grep(paste0("/", run_it), files_list, value = TRUE)
  file_it <- error_files[1]
  for (file_it in error_files){
    
    temp <- read.csv(file_it)
    error_l[[file_it]] <- temp %>%
      mutate(country = metrics_$country[it], crop = metrics_$crop[it],
             features = metrics_$source[it], tech = metrics_$technique[it])
  }
  
}

errors_ <- rbindlist(error_l)
errors <- errors_ %>%
  filter(tech == "rf", country != "NL") %>%
  mutate(error = abs(pred - obs))

ggplot() +
  facet_grid(country ~ year) +
  geom_violin(data = errors, aes(x = as.factor(features), y=error)) +
  theme(axis.text.x = element_text(angle= 90))

# Count feature types -----------------------------------------------------
count_feat <- metrics_ %>%
  select(source, crop, country, features_) %>%
  separate_rows(features_, sep = ", ") %>%
  mutate(type = str_extract(features_, 
                            "prec|tavg|tmin|tmax|rad|fpar|ndvi|gdd|ssm|bulk"))

count_feat <- table(count_feat$type, count_feat$source) %>%
  as.data.frame() %>%
  pivot_wider(names_from = "Var2", values_from = "Freq") %>%
  rename(feature = Var1) %>%
  mutate(sum_ =  rowSums(across(-feature))) %>%
  arrange(desc(sum_))

count_feat

# Raw weather -------------------------------------------------------------
meteo_l <- ndvi_l <- calendar_l <- list()
files_names <- gsub(".csv", "", gsub(".*/", "", original_files))

it <- 3
# Meteo
for (it in 1:length(original_files)){
    
    crop <- str_extract(files_names[it], "maize|wheat")
    country <- str_extract(files_names[it], "ES|DE|FR")
    crop_country <- paste(crop, country, sep ="_")
    
    if(grepl("meteo", files_names[it])){
        
        calendar <- fread(gsub("meteo", "crop_season", original_files[it]))
        calendar_l[[crop_country]] <- calendar
        
        # Weather data
        crop <- str_extract(files_names[it], "maize|wheat")
        country <- str_extract(files_names[it], "FR|DE|ES")
        
        temp <- fread(original_files[it])
        temp <- truncate_series(x=copy(temp), calendar, keep_date = TRUE)
        
        meteo_l[[crop_country]] <- temp %>%
            mutate(crop = crop, country = country)
        
    }
    
}

meteo <- rbindlist(meteo_l) 
rm(meteo_l)

# Example feature per period ----------------------------------------------
train_all <- rbindlist(train_l, fill = TRUE) %>%
  mutate(split = "train")
test_all <- rbindlist(test_l, fill = TRUE) %>%
  mutate(split = "test")

features <- rbind(train_all, test_all)

country_ <- "DE"

features_ <- filter(features, country == country_, crop == "wheat")
pheno2p <- filter(features_, source == "2periods", country == country_)

crop_country <- meteo %>%
    filter(country == country_, crop == "wheat", year <= 2020)

dates_vline <- crop_country %>%
    select(adm_id, year, date, das) %>%
    group_by(adm_id, year) %>%
    mutate(vline_ = if_else(day(date) == 1, das, NA),
           vline_ = vline_ - min(vline_, na.rm = TRUE),
           month_ = month(date)) %>%
    ungroup() %>%
    filter(!is.na(vline_))
    
monthly <- features_ %>%
    filter(source == "monthly", country == country_) %>%
    select(adm_id, year, contains("_tmin")) %>%
    pivot_longer(contains("tmin"), names_to = "month_", values_to = "tmin") %>%
    filter(!is.na(tmin)) %>%
    mutate(month_ = as.numeric(str_extract(month_, "[0-9]+")),
           period = paste0("m", str_pad(month_, 2, "left", "0"))) %>%
    left_join(dates_vline) %>%
    filter(!is.na(das)) %>%
    pivot_wider(names_from = period, values_from = vline_)

ggplot() +
  facet_wrap("year", scales = "free") +
  geom_line(data = crop_country, aes(x = das, y = tmin, group = adm_id), 
            colour = "darkblue", alpha = 0.01) +
  geom_vline(data = pheno2p, aes(xintercept = p01), alpha = 0.3) +
    geom_vline(data = pheno2p, aes(xintercept = p02), alpha = 0.3) +
  geom_point(data = pheno2p, aes(x = p02, y = tmin_avg_2), colour = "blue") +
    geom_vline(data = filter(dates_vline, month_ == 5), 
               aes(xintercept = vline_), alpha = 0.3, color = "purple") +
  geom_point(data = monthly, aes(x = m06, y = tmin), colour = "plum")

# Raw data ----------------------------------------------------------------

# Other feature
for (it in 1:length(original_files)){
  
  crop <- str_extract(files_names[it], "maize|wheat")
  country <- str_extract(files_names[it], "ES|DE|FR")
  crop_country <- paste(crop, country, sep ="_")
  
  if(grepl("ndvi", files_names[it])){
    
   calendar <- calendar_l[[crop_country]]
    
  temp <- fread(original_files[it])
  temp <- truncate_series(x=copy(temp), calendar)
    
  ndvi_l[[crop_country]] <- temp %>%
    mutate(crop = crop, country = country)
    
  }
  
}


ndvi <- rbindlist(ndvi_l) 
rm(ndvi_l)

# ggplot() +
#   facet_grid(crop ~ country, scales = "free") +
#   geom_boxplot(data = predictor, aes(x = as.factor(year), y = tavg), 
#                position = "dodge") +
#   theme(axis.text.x = element_text(angle= 90))
# 
# ggplot() +
#   facet_grid(crop ~ country, scales = "free") +
#   geom_smooth(data = predictor, aes(x = das, y = tmin, 
#                                 colour = as.factor(year), fill = as.factor(year))) +
#   geom_smooth(data = predictor, aes(x = das, y = tmax, 
#                                 colour = as.factor(year), fill = as.factor(year)))

# From raw features -------------------------------------------------------
path_ <- "../../data/features/2p/"
files_list <- list.files(path_, recursive = TRUE, full.names = TRUE)

features_l <- list()

for (it in 1:length(files_list)){

  # Weather data
  crop <- str_extract(files_list[it], "maize|wheat")
  country <- str_extract(files_list[it], "FR|DE|ES")

  temp_ <- fread(files_list[it])

  temp <- temp_ %>%
    mutate(crop = crop, country = country) %>%
    pivot_longer(starts_with(("tt")), names_to = "tt", values_to = "das") %>%
    mutate(tt = as.numeric(gsub("tt", "", tt))) %>%
    group_by(adm_id, year) %>%
    mutate(tt = paste0("p0", rank(tt))) %>%
    ungroup() %>%
    pivot_wider(names_from = "tt", values_from = "das")

  features_l[[files_names[[it]]]] <- temp

}

features <- rbindlist(features_l) %>%
  filter(year >= 2004, year <= 2020)

rm(features_l)

crop_country <- filter(meteo, 
                       #country == "DE", crop == "wheat", 
                       year <= 2020)
features_ <- filter(features, country == "DE", crop == "wheat")

ggplot() +
  facet_wrap("year", scales = "free") +
  geom_smooth(data = crop_country, aes(x = das, y = tavg), colour = "darkblue") +
  geom_vline(data = features_, aes(xintercept = p01), alpha = 0.3) +
  # geom_point(data = features_, aes(x = p01, y = tmin_min_1), colour = "darkblue") +
  geom_point(data = features_, aes(x = p01, y = tavg_avg_1), colour = "darkgreen") +
  geom_vline(data = features_, aes(xintercept = p02), alpha = 0.3) +
  # geom_point(data = features_, aes(x = p02, y = tmin_min_2), colour = "darkblue") +
  geom_point(data = features_, aes(x = p02, y = tavg_avg_2), colour = "darkgreen")


