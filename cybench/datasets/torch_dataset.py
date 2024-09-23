import pandas as pd
import numpy as np
import torch
import torch.utils.data

from cybench.config import (
    KEY_LOC,
    KEY_YEAR,
    KEY_TARGET,
    KEY_DATES,
    KEY_CROP_SEASON,
    CROP_CALENDAR_DATES,
    TIME_SERIES_PREDICTORS,
    ALL_PREDICTORS,
)

from cybench.datasets.dataset import Dataset
from cybench.util.torch import batch_tensors
from cybench.datasets.alignment import (
    interpolate_time_series_data,
    aggregate_time_series_data,
)


class TorchDataset(Dataset, torch.utils.data.Dataset):
    def __init__(
        self,
        dataset: Dataset,
        interpolate_time_series: bool = False,
        aggregate_time_series_to: str = None,
        max_season_window_length: int = None,
    ):
        """
        PyTorch Dataset wrapper for compatibility with torch DataLoader objects
        :param dataset: Dataset
        :param interpolate_time_series: whether to interpolate time series data
        :param aggregate_time_series_to: aggregation resolution for time series data
                                         ("week" and "dekad" are supported)
        :param max_season_window_length: maximum length of time series
        """
        self._crop = dataset.crop
        self._df_y = dataset._df_y

        if max_season_window_length is None:
            self._max_season_window_length = dataset.max_season_window_length
        else:
            self._max_season_window_length = max_season_window_length

        self._aggregate_time_series_to = aggregate_time_series_to
        if (interpolate_time_series):
            assert self._max_season_window_length is not None
            self._dfs_x = {}
            ts_inputs = {}
            for x in dataset._dfs_x:
                if "date" not in dataset._dfs_x[x].index.names:
                    self._dfs_x[x] = dataset._dfs_x[x]
                else:
                    ts_inputs.append(dataset._dfs_x[x])

            df_ts = interpolate_time_series_data(ts_inputs, self._dfs_x[KEY_CROP_SEASON],
                                                 self._max_season_window_length)

            if aggregate_time_series_to is not None:
                if aggregate_time_series_to not in ["week", "dekad"]:
                    raise Exception(
                        f"Unsupported time series aggregation resolution {aggregate_time_series_to}"
                    )
                # aggregate
                df_ts.reset_index(inplace=True)
                df_ts = aggregate_time_series_data(df_ts, self._aggregate_time_series_to)

            # Add time series to self._dfs_x
            self._dfs_x["combined_ts"] = df_ts.set_index(
                [KEY_LOC, KEY_YEAR, "date"]
            ).sort_index()
        else:
            self._dfs_x = dataset._dfs_x

        # Bool value that specifies whether missing data values are allowed
        # For now always set to False
        self._allow_incomplete = False

    def __getitem__(self, index) -> dict:
        """
        Get a sample from the dataset
        Data is cast to PyTorch tensors where required
        :param index: index that is passed to the dataset
        :return: a dict containing the data sample
        """
        sample = super(TorchDataset, self).__getitem__(index)
        if self._aggregate_time_series_to is not None:
            if self._aggregate_time_series_to == "week":
                num_time_steps = int(np.ceil(self._max_season_window_length / 7))
            else:
                num_time_steps = int(np.ceil(self._max_season_window_length / 10))

            # make sure time series data has the same number of time steps
            for k in TIME_SERIES_PREDICTORS:
                if sample[k].shape[0] > num_time_steps:
                    sample[k] = sample[k][-num_time_steps:]
                elif sample[k].shape[0] < num_time_steps:
                    sample[k] = np.pad(
                        sample[k],
                        (num_time_steps - sample[k].shape[0], 0),
                        mode="constant",
                        constant_values=0.0,
                    )

        return self._cast_to_tensor(sample)

    @classmethod
    def _cast_to_tensor(cls, sample: dict) -> dict:
        """
        Create a sample with all data cast to torch tensors
        :param sample: the sample to convert
        :return: the converted data sample
        """
        nontensors1 = {
            KEY_LOC: sample[KEY_LOC],
            KEY_YEAR: sample[KEY_YEAR],
            KEY_DATES: sample[KEY_DATES],
        }

        # crop calendar dates are datetime objects
        nontensors2 = {k: sample[k] for k in CROP_CALENDAR_DATES if k in sample}

        tensors1 = {
            KEY_TARGET: torch.tensor(sample[KEY_TARGET], dtype=torch.float32),
        }

        tensors2 = {
            key: torch.tensor(sample[key], dtype=torch.float32)
            for key in ALL_PREDICTORS
        }  # TODO -- support nonnumeric data?

        return {**nontensors1, **nontensors2, **tensors1, **tensors2}

    @classmethod
    def collate_fn(cls, samples: list) -> dict:
        """
        Function that takes a list of data samples (as dicts, containing torch tensors)
        and converts it to a dict of batched torch tensors
        :param samples: a list of data samples
        :return: a dict with batched data
        """
        assert len(samples) > 0

        nontensors1 = {
            KEY_LOC: [sample[KEY_LOC] for sample in samples],
            KEY_YEAR: [sample[KEY_YEAR] for sample in samples],
            KEY_DATES: samples[0][KEY_DATES],
        }

        # crop calendar dates are datetime objects
        nontensors2 = {
            k: [sample[k] for sample in samples if k in sample]
            for k in CROP_CALENDAR_DATES
        }

        tensors1 = {
            KEY_TARGET: batch_tensors(*[sample[KEY_TARGET] for sample in samples])
        }

        tensors2 = {
            key: batch_tensors(*[sample[key] for sample in samples])
            for key in ALL_PREDICTORS
        }

        return {**nontensors1, **nontensors2, **tensors1, **tensors2}
