import os
import pandas as pd
import numpy as np
import datetime
import itertools

from cybench.datasets.dataset import Dataset
from cybench.config import PATH_DATA_DIR
from cybench.config import (
    KEY_LOC,
    KEY_YEAR,
    KEY_TARGET,
    KEY_COMBINED_FEATURES,
    FORECAST_LEAD_TIME
)

def save_loaded():
    crop_it = "wheat"
    country_it = "BR"
    crop_country = crop_it + "_" + country_it

    dataset_ = Dataset.load(crop_country)

    dfs_x = dataset_._dfs_x
    for x, df in dfs_x.items():
        filename = x + "_" + crop_it + "_" + country_it + ".csv"
        df.to_csv(os.path.join(PATH_DATA_DIR, "loaded", crop_it, country_it, filename), index=True)
    
    df_y = dataset_._df_y
    filename = "yield_" + crop_it + "_" + country_it + ".csv"
    df_y.to_csv(os.path.join(PATH_DATA_DIR, "loaded", crop_it, country_it, filename), index=True)

save_loaded()
