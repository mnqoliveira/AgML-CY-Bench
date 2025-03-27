# Monique Oliveira - moniquepgoliveira@gmail.com

# Libraries ---------------------------------------------------------------
libraries <- c("here", "tidyverse", "data.table", "zoo")

lapply(libraries, require, character.only = TRUE)

# Functions ---------------------------------------------------------------
source(here("feat_eng_2p_f.R"))
source(here("load_files_f.R"))

# Process -----------------------------------------------------------------
crop_ <- "wheat"
country_ <- "ES"

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

tt_das <- def_tt(x=copy(meteo), 
                 tbase=tbase_, tupper=tupper_, thresholds=thresh_)
print(Sys.time())
print("tt")

meteo <- generate_set(x=meteo, tt_das)

print(Sys.time())
print("meteo")

# FPAR
filename_ <- grep("fpar", files_list_, value=TRUE)
fpar <- fread(filename_)

fpar <- truncate_series(x=copy(fpar), calendar)
fpar <- generate_set(x=copy(fpar), tt_das)

print(Sys.time())
print("fpar")

# NDVI
filename_ <- grep("ndvi", files_list_, value=TRUE)
num_rows <- length(count.fields(filename_, skip = 0))
ndvi <- fread(filename_)

ndvi <- truncate_series(x=copy(ndvi), calendar)
ndvi <- generate_set(x=copy(ndvi), tt_das)

print(Sys.time())
print("ndvi")

# Soil moisture
filename_ <- grep("soil_moisture", files_list_, value=TRUE)
sm <- fread(filename_)

sm <- truncate_series(x=copy(sm), calendar)
sm <- generate_set(x=copy(sm), tt_das)

print(Sys.time())
print("sm")

# Soil characteristics
soil <- fread(grep("soil_", files_list_[!grepl("moisture", files_list_)],
                   value=TRUE)) 
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
         flag_period1 = if_else(if_any(ends_with("1"), is.na), 0, 1),
         flag_period2 = if_else(if_any(ends_with("2"), is.na), 0, 1),
         across(.cols=everything(), .fns = ~replace_na(.x, mean(.x, na.rm=TRUE))),
         across(.cols=where(is.numeric), .fns = ~round(.x, 4)),
         across(.cols=contains("rad"), .fns = ~round(.x, 0)))

filename <- paste0(crop_it, "_", country_it, ".csv")
filepath <- paste0("../../data/features/2p/", filename)
write.csv(dataset_mod, file=filepath, row.names = FALSE)


# Feature selection -------------------------------------------------------
# Define target variable and predictors
target <- "yield"
predictors <- setdiff(names(dataset_mod), target)
ids <- c("adm_id", "year")
predictors <- setdiff(predictors, c(ids, grep("flag", colnames(dataset_mod), value = TRUE)))
dataset_mod <- data.frame(dataset_mod)

# Compute absolute Spearman correlation matrix
corr_matrix <- abs(cor(dataset_mod[predictors], method = "spearman"))
target_corr <- abs(cor(dataset_mod[, c(target, predictors)], method = "spearman"))[target, -1]

to_drop <- c()

# Loop over all unique pairs of predictors
comb <- combn(predictors, 2, simplify = FALSE)
for (pair in comb) {
  feat1 <- pair[1]
  feat2 <- pair[2]
  
  # Skip if either feature is already marked for removal
  if (feat1 %in% to_drop || feat2 %in% to_drop) next
  
  # Check correlation threshold
  if (corr_matrix[feat1, feat2] > 0.7) {
    if (target_corr[feat1] < target_corr[feat2]) {
      to_drop <- c(to_drop, feat1)
    } else {
      to_drop <- c(to_drop, feat2)
    }
  }
}

selected_features <- setdiff(predictors, to_drop)
final_features <- selected_features[target_corr[selected_features] >= 0.1]

dataset_mod_ <- dataset_mod[, c(ids, selected_features, target)]

# Save
filename <- paste0(crop_it, "_", country_it, ".csv")
filepath <- paste0("../../data/features/sel2p/", filename)
write.csv(dataset_mod, file=filepath, row.names = FALSE)

# Plot the heatmap using ggplot2
plot_features <- c(final_features, target)
corr <- cor(dataset_mod[, plot_features], method = "spearman")

corr_df <- as.data.frame(as.table(corr)) %>%
  rename(Feature1 = Var1, Feature2 = Var2, Correlation = Freq)

ggplot(corr_df, aes(x = Feature1, y = Feature2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Spearman Correlation Heatmap")