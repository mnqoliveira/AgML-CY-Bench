# Agrometeorological indicators from 1979 to present derived from reanalysis

## Short description
This dataset provides daily surface meteorological data for the period from 1979 to present as input for agriculture and agro-ecological studies.This dataset is based on the hourly ECMWF ERA5 data at surface level and is referred to as AgERA5.

## Link
https://doi.org/10.24381/cds.6c68c9bb

## Dataset publisher
European Centre for Medium-Range Weather Forecasts (ECMWF)

## Dataset owner
European Centre for Medium-Range Weather Forecasts (ECMWF)

## Data card author
Abdelrahman Amr Ali, Dilli R. Paudel

## Dataset overview

**Spatial resolution** : 0.1° (see horizontal resolution below)

| Attribute                | Description                                          |
|--------------------------|------------------------------------------------------|
| Data type                | Gridded                                              |
| Projection               | Regular latitude-longitude grid                      |
| Horizontal coverage      | Global                                               |
| Horizontal resolution    | 0.1° x 0.1°                                          |
| Vertical coverage        | Variables are provided on a single level which may differ among variables |
| Temporal coverage        | From 1979 to present                                 |
| Temporal resolution      | Daily                                                |
| File format              | NetCDF-4                                             |
| Conventions              | Climate and Forecast (CF) Metadata Convention v1.7   |
| Versions                 | 1.0, 1.1                                             |
| Update frequency         | Monthly                                              |

## Main variables

| Name                                 | Units         | Description |
|--------------------------------------|---------------|-------------|
| 10m wind speed                       | m s-1         | Mean wind speed at a height of 10 metres above the surface over the period 00h-24h local time. |
| 2m dewpoint temperature              | K             | Mean dewpoint temperature at a height of 2 metres above the surface over the period 00h-24h local time. The dew point is the temperature to which air must be cooled to become saturated with water vapor. In combination with the air temperature, it is used to assess relative humidity. |
| 2m relative humidity                 | %             | Relative humidity at 06h, 09h, 12h. 15h, 18h (local time) at a height of 2 metres above the surface. This variable describes the amount of water vapour present in air expressed as a percentage of the amount needed for saturation at the same temperature. |
| 2m temperature                       | K             | Air temperature at a height of 2 metres above the surface. |
| Cloud cover                          | Dimensionless | The number of hours with clouds over the period 00h-24h local time divided by 24 hours. |
| Liquid precipitation duration fraction | Dimensionless | The number of hours with precipitation over the period 00h-24h local time divided by 24 hours and per unit of area. Liquid precipitation is equivalent to the height of the layer of water that would have formed from precipitation had the water not penetrated the soil, run off, or evaporated. |
| Precipitation flux                   | mm day-1      | Total volume of liquid water (mm³) precipitated over the period 00h-24h local time per unit of area (mm²), per day. |
| Snow thickness                       | cm            | Mean snow depth over the period 00h-24h local time measured as volume of snow (cm³) per unit area (cm²). |
| Snow thickness LWE                   | cm            | Mean snow depth liquid water equivalent (LWE) over the period 00h-24h local time measured as volume of snow (cm³) per unit area (cm²) if all the snow had melted and had not penetrated the soil, runoff, or evaporated. |
| Solar radiation flux                 | J m-2 day-1   | Total amount of energy provided by solar radiation at the surface over the period 00-24h local time per unit area and time. |
| Solid precipitation duration fraction | Dimensionless | The number of hours with solid precipitation (freezing rain, snow, wet snow, mixture of rain and snow, and ice pellets) over the period 00h-24h local time divided by 24 hours and per unit of area. |
| Vapour pressure                      | hPa           | Contribution to the total atmospheric pressure provided by the water vapour over the period 00-24h local time per unit of time. |

## Provenance
[Documentation](https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.6c68c9bb?tab=doc) includes
* [product user guide and specification](https://confluence.ecmwf.int/x/3FmaE)
* [algorithm theoretical basis](https://confluence.ecmwf.int/x/yFmaE)
* [downscaling and bias correction](https://confluence.ecmwf.int/x/4lmaE)

## License
[License to use Copernicus Products](https://cds.climate.copernicus.eu/api/v2/terms/static/licence-to-use-copernicus-products.pdf)

## How to cite
Cite the dataset as follows:
Boogaard, H., Schubert, J., De Wit, A., Lazebnik, J., Hutjes, R., Van der Grijn, G., (2020): Agrometeorological indicators from 1979 to present derived from reanalysis. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). DOI: 10.24381/cds.6c68c9bb (Accessed on DD-MMM-YYYY)
