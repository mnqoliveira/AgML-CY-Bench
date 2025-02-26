import os
# import torch
import pandas as pd
import numpy as np
import datetime
import itertools

from cybench.datasets.dataset import Dataset
from cybench.models.naive_models import AverageYieldModel
from cybench.models.trend_models import TrendModel
from cybench.models.sklearn_models import SklearnRidge, SklearnRandomForest
from cybench.models.residual_models import RidgeRes
# from cybench.models.nn_models import BaselineLSTM
from cybench.evaluation.eval import evaluate_predictions

from cybench.config import PATH_DATA_DIR, PATH_OUTPUT_DIR
from cybench.config import (
    KEY_LOC,
    KEY_YEAR,
    KEY_TARGET,
    KEY_COMBINED_FEATURES
)


def test_both(crop_it, country_it, tech_it, source_it):
    
    '''
    tech_l: list, list of techniques to test
    type_l: original or modified
    
    '''
    crop_country = crop_it + "_" + country_it

    all_years = list(range(2001, 2021))
    test_years = list(range(2015, 2021))
    train_years = [yr for yr in all_years if yr not in test_years]
    
    if source_it == "orig":
        # Test 1: Test with raw data
        dataset_ = Dataset.load(crop_country)
        train_years = [yr for yr in all_years if yr not in test_years]
        train_dataset, test_dataset = dataset_.split_on_years(
            (train_years, test_years)
            )
        feat_cols_=None
    
    else:
        # Test 2: Test with predesigned features
        if source_it == "2periods":
            data_path = os.path.join(PATH_DATA_DIR, "features", "2p")
        else:
            data_path = os.path.join(PATH_DATA_DIR, "features", "5p")
        
        # Training dataset
        train_csv = os.path.join(data_path, (crop_country + ".csv"))
        train_df = pd.read_csv(train_csv, index_col=[KEY_LOC, KEY_YEAR])

        train_yields = train_df[[KEY_TARGET]].copy()
        feature_cols = [c for c in train_df.columns if c != KEY_TARGET]
        train_features = train_df[feature_cols].copy()
        train_dataset = Dataset(
            crop_it, train_yields, {KEY_COMBINED_FEATURES: train_features}
        )
        train_dataset, test_dataset = train_dataset.split_on_years(
            (train_years, test_years)
        )
        feat_cols_=feature_cols
    
    targets = test_dataset.targets()
    
    if tech_it == "skrid":
        model_ = SklearnRidge(feature_cols=feat_cols_)
        tech_ = "SklearnRidge"
    
    elif tech_it == "ridres":
        model_ = RidgeRes(feature_cols=feat_cols_)
        tech_ = "RidgeRes"
    
    elif tech_it == "rf":
        model_ = SklearnRandomForest(feature_cols=feat_cols_)
        tech_ = "RF"

    model_.fit(train_dataset)
    model_preds, _ = model_.predict(test_dataset)
    metrics_ = evaluate_predictions(targets, model_preds)
    
    return metrics_


crop_l = ["wheat", "maize"]
crop_l = ["maize"]
country_l = ["NL"]
tech_l = ['skrid', 'ridres', 'rf']
#tech_l = ['rf']
source_l = ["2periods", "orig"]
# source_l = ["2periods"]

comb = {'crop': crop_l, 'country': country_l, 'tech': tech_l, 'source': source_l}
comb = pd.DataFrame.from_records(itertools.product(*comb.values()), columns=comb.keys())
comb.sort_values(by=['country', 'crop', 'tech', 'source'], ascending = [False, False, True, True], 
    inplace = True, ignore_index = True)

col_names_ = ['normalized_rmse', 'mape', 'r2', 'technique', 'source', 'crop', 'country', 'time']

filename = "tests_" + str(datetime.datetime.now().strftime('%Y%m%d_%H%M%S') + ".csv")

for it in range(comb.shape[0]):
    
    crop_it = comb.crop[it]
    country_it = comb.country[it]
    tech_it = comb.tech[it]
    source_it = comb.source[it]

    res_ = test_both(crop_it, country_it, tech_it, source_it)
    
    results = pd.DataFrame([res_])
    
    results["technique"] = tech_it
    results["source"] = source_it
    results["crop"] = crop_it
    results["country"] = country_it
    results["time"] = str(datetime.datetime.now().strftime('%Y%m%d_%H%M%S'))
    
    header_ = None
    if it == 0:
        header_ = col_names_

    results.to_csv(os.path.join(PATH_OUTPUT_DIR, "agmip10", filename), index=False, mode='a', header = header_)
