# Monique Oliveira - moniquepgoliveira@gmail.com

# Libraries ---------------------------------------------------------------
libraries <- c("here", "tidyverse", "data.table", "zoo")

lapply(libraries, require, character.only = TRUE)

# Functions ---------------------------------------------------------------
source(here("feat_eng_5p_f.R"))
source(here("load_files_f.R"))

# Process -----------------------------------------------------------------
crop_ <- c("maize")
country_ <- c("NL")

data_folder <- "../../data/loaded/"
list_crops <- list.dirs(data_folder, full.names = FALSE)
list_crops <- list_crops[grepl("/", list_crops) & !grepl("bkp", list_crops)]
list_crops <- data.frame(str_split_fixed(list_crops, "/", 2))
colnames(list_crops) <- c("crop", "country")

list_crops <- list_crops %>%
  filter(crop %in% crop_, country %in% country_)

files_list <- list.files(data_folder, recursive = TRUE, full.names = TRUE)
files_list <- files_list[!grepl("bkp|features", files_list)]

# Time-series features ----------------------------------------------------
pheno_info <- read.csv("./pheno_info.csv")

crop_it <- list_crops[1, "crop"]
country_it <- list_crops[1, "country"]
ttsum <- NULL

files_list_ <- grep(paste(crop_it, country_it, sep = "/"), files_list, value = TRUE)

pheno_info_ <- pheno_info %>%
  filter(crop == crop_it, country == country_it)

tbase_ <- pull(pheno_info_, "tbase")
tupper_ <- pull(pheno_info_, "tupper")
thresh_ <- pheno_info_ %>%
  select(-crop, -country, -tbase, -tupper) %>%
  select(where(~ all(!is.na(.)))) %>%
  unlist(use.names = FALSE)

# Calendar
calendar <- fread(grep("season", files_list_, value = TRUE))

# Weather data
filename_ <- grep("meteo", files_list_, value=TRUE)
meteo <- fread(filename_)
meteo <- truncate_series(x=copy(meteo), calendar)

periods_dates <- dates_dvs(x=copy(meteo), 
                           tbase=tbase_, tupper=tupper_, thresholds=thresh_, calendar)
print(Sys.time())
print("tt")

meteo <- generate_set(x=meteo, periods_dates)

print(Sys.time())
print("meteo")

# FPAR
filename_ <- grep("fpar", files_list_, value=TRUE)
fpar <- fread(filename_)

fpar <- truncate_series(x=copy(fpar), calendar)
fpar <- generate_set(x=copy(fpar), periods_dates)

print(Sys.time())
print("fpar")

# NDVI
filename_ <- grep("ndvi", files_list_, value=TRUE)
num_rows <- length(count.fields(filename_, skip = 0))
ndvi <- fread(filename_)

ndvi <- truncate_series(x=copy(ndvi), calendar)
ndvi <- generate_set(x=copy(ndvi), periods_dates)

print(Sys.time())
print("ndvi")

# Soil moisture
filename_ <- grep("soil_moisture", files_list_, value=TRUE)
sm <- fread(filename_)

sm <- truncate_series(x=copy(sm), calendar)
sm <- generate_set(x=copy(sm), periods_dates)

print(Sys.time())
print("sm")

# Soil characteristics
soil <- fread(grep("soil_", files_list_[!grepl("moisture", files_list_)],
                   value=TRUE)) %>%
  mutate(awc = round(awc, 4),
         bulk_density = round(bulk_density, 4))
# %>%
#   mutate(drainage_class = as.character(drainage_class))
# 
# encoded <- model.matrix(~ drainage_class - 1, data = soil)
# 
# soil <- soil %>%
#   cbind(encoded) %>%
#   select(-drainage_class, -crop_name) %>%
#   as.data.table()

print(Sys.time())
print("soil")

# Yield
yield_ <- read.csv(grep("yield", files_list_, value=TRUE))
yield_ <- yield_ %>%
  select(adm_id, yield, year) %>%
  as.data.table()

print(Sys.time())
print("yield")

ids <- c("adm_id", "year")
cols <- c("adm_id", "year", "yield")
dataset <- yield_
dataset <- meteo[dataset, on=ids]
dataset <- fpar[dataset, on=ids]
dataset <- ndvi[dataset, on=ids]
# Remove rows for which there is no predictor data available, even if the label is
dataset <- sm[dataset, on=ids][rowSums(!is.na(dataset[, !cols, with = FALSE])) > 0]
dataset <- soil[dataset, on="adm_id"]

dataset_mod <- dataset %>%
  select(where(~ !all(is.na(.)))) %>%
  mutate(across(.cols=starts_with("drainage"), .fns = ~replace_na(.x, 0)),
         across(.cols=-c("adm_id", "year"), .fns = ~na_if(.x, Inf)),
         across(.cols=-c("adm_id", "year"), .fns = ~na_if(.x, -Inf)),
         flag_period0 = if_else(if_any(ends_with("p0"), is.na), 0, 1),
         flag_period1 = if_else(if_any(ends_with("p1"), is.na), 0, 1),
         flag_period2 = if_else(if_any(ends_with("p2"), is.na), 0, 1),
         flag_period3 = if_else(if_any(ends_with("p3"), is.na), 0, 1),
         flag_period4 = if_else(if_any(ends_with("p4"), is.na), 0, 1),
         flag_period5 = if_else(if_any(ends_with("p5"), is.na), 0, 1),
         across(.cols=everything(), .fns = ~replace_na(.x, -9999)),
         across(.cols=contains("rad"), .fns = ~round(.x, 0)))

filename <- paste0(crop_it, "_", country_it, ".csv")
filepath <- paste0("../../data/features/5p/", filename)
write.csv(dataset_mod, file=filepath, row.names = FALSE)