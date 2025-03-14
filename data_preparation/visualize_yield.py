import os
import pandas as pd
import geopandas as gpd
import argparse
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.colors import Normalize
from PIL import Image

from cybench.config import KEY_LOC, KEY_TARGET, KEY_YEAR, PATH_DATA_DIR

AGML_ROOT = r"/path/to/agml"

EU_COUNTRY_CODE_KEY = "CNTR_CODE"
EU_ADMIN_LEVEL_KEY = "LEVL_CODE"

# Country codes to admin level
# Austria (AT), Belgium (BE), Bulgaria (BG), Czech Republic (CZ), Germany (DE), Denmark (DK),
# Estonia (EE), Greece (EL), Spain (ES), Finland (FI), France (FR), Croatia (HR), Hungary (HU),
# Ireland (IE), Italy (IT), Lithuania (LT), Latvia (LV), The Netherlands (NL), Poland (PL),
# Portugal (PT), Romania (RO), Sweden (SE), Slovakia (SK)
EU_COUNTRIES = {
    "AT": 2,
    "BE": 2,
    "BG": 2,
    "CZ": 3,
    "DE": 3,
    "DK": 3,
    "EE": 3,
    "EL": 3,
    "ES": 3,
    "FI": 3,
    "FR": 3,
    "HR": 2,
    "HU": 3,
    "IE": 2,
    "IT": 3,
    "LT": 3,
    "LV": 3,
    "NL": 2,
    "PL": 2,
    "PT": 2,
    "RO": 3,
    "SE": 3,
    "SK": 3,
}

# Angola (AO), Burkina Faso (BF), Ethiopia (ET), Lesotho (LS), Madagascar (MG), Malawi (MW),
# Mozambique (MZ), Niger (NE), Senegal (SN), Chad (TD), South Africa (ZA), Zambia (ZM)
FEWSNET_COUNTRIES = [
    "AO",
    "BF",
    "ET",
    "LS",
    "MG",
    "MW",
    "MZ",
    "NE",
    "SN",
    "TD",
    "ZA",
    "ZM",
]
FEWSNET_ADMIN_ID_KEY = "adm_id"


def get_shapes(region="US"):
    """
    @author: Dilli R. Paudel
    Get admin unit boundaries.

    :param region: region code or 2-letter country code
    :return: a dataframe with adm_id and boundaries
    """
    sel_shapes = pd.DataFrame()
    if region == "EU":
        geo_df = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_EU.zip")
        )
        for cn in EU_COUNTRIES:
            cn_shapes = geo_df[
                (geo_df[EU_COUNTRY_CODE_KEY] == cn)
                & (geo_df[EU_ADMIN_LEVEL_KEY] == EU_COUNTRIES[cn])
            ]
            sel_shapes = pd.concat([sel_shapes, cn_shapes], axis=0)

        sel_shapes["adm_id"] = sel_shapes["NUTS_ID"]
    elif region in EU_COUNTRIES:
        geo_df = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_EU.zip")
        )
        sel_shapes = geo_df[
            (geo_df[EU_COUNTRY_CODE_KEY] == region)
            & (geo_df[EU_ADMIN_LEVEL_KEY] == EU_COUNTRIES[region])
        ]
        sel_shapes["adm_id"] = sel_shapes["NUTS_ID"]

    elif region == "AR":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_AR.zip")
        )
        sel_shapes["adm_id"] = sel_shapes["ADM2_PCODE"]
    elif region == "AU":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_AU.zip")
        )
        sel_shapes["adm_id"] = "AU" + "-" + sel_shapes["AAGIS"].astype(str)
    elif region == "BR":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_BR.zip")
        )
        sel_shapes["adm_id"] = sel_shapes["ADM2_PCODE"]
    elif region == "CN":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_CN.zip")
        )
        sel_shapes["adm_id"] = sel_shapes["ADM1_PCODE"]
    # FEWSNET countries: Already have adm_id
    elif region == "FEWSNET":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_FEWSNET.zip")
        )
    elif region in FEWSNET_COUNTRIES:
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_FEWSNET.zip")
        )
        sel_shapes = sel_shapes[sel_shapes["adm_id"].str[:2] == region]
    # IN: Already has adm_id
    elif region == "IN":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_IN.zip")
        )
    # ML: Already has adm_id
    elif region == "ML":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_ML.zip")
        )
    # MX: Already has adm_id
    elif region == "MX":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_MX.zip")
        )
        # adm_id in shapefile has a hyphen. Yield data does not have one.
        sel_shapes["adm_id"] = sel_shapes["adm_id"].str.replace("-", "")
    elif region == "US":
        sel_shapes = gpd.read_file(
            os.path.join(AGML_ROOT, "shapefiles", "shapefiles_US.zip")
        )
        sel_shapes["adm_id"] = (
            "US" + "-" + sel_shapes["STATEFP"] + "-" + sel_shapes["COUNTYFP"]
        )

    # Project to EPSG 4326
    # shapes for BR don't have crs info. See #343.
    if region not in ["BR"]:
        sel_shapes = sel_shapes.to_crs(4326)

    return sel_shapes


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="visualize_yield.py", description="Visualize yield"
    )
    parser.add_argument("-c", "--crop", type=str, default="wheat")
    args = parser.parse_args()

    crop = args.crop

    world_shapefile_path = os.path.join(
        os.path.abspath(os.path.join(__file__, os.pardir)),
        "ne_110m_admin_0_countries",
        "ne_110m_admin_0_countries.shp",
    )

    world = gpd.read_file(world_shapefile_path)

    countries = [
        country_code
        for country_code in os.listdir(os.path.join(PATH_DATA_DIR, crop))
        if os.path.isdir(os.path.join(PATH_DATA_DIR, crop, country_code))
    ]

    # countries = ["CN", "IN", "US"]  # Replace with your actual country codes
    # Create an empty list to store the merged data
    all_country_data = []
    for country_code in countries:
        geo_df = get_shapes(region=country_code)
        geo_df = geo_df[[KEY_LOC, "geometry"]]

        # targets
        yield_file = os.path.join(
            PATH_DATA_DIR, crop, country_code, f"yield_{crop}_{country_code}.csv"
        )
        df_y = pd.read_csv(yield_file, header=0)
        df_y = df_y.rename(columns={"harvest_year": KEY_YEAR})
        df_y = df_y[[KEY_LOC, KEY_YEAR, KEY_TARGET]]
        df_y = df_y.dropna(axis=0)
        df_y = df_y[df_y[KEY_TARGET] > 0.0]
        df_y = (
            df_y.reset_index()
        )  # Reset the multi-index to have the columns explicitly

        merged_country_df = geo_df.merge(df_y, on=KEY_LOC, how="left")
        merged_country_df["country_code"] = country_code
        # Append the merged country data to the list
        all_country_data.append(merged_country_df)
    # Combine all the country data into one large DataFrame
    merged_df = pd.concat(all_country_data, ignore_index=True)

    # Create a folder to store individual frames (images)
    output_dir = "frames"
    os.makedirs(output_dir, exist_ok=True)

    # Normalize the yield data across all years
    norm = Normalize(
        vmin=min(0, round(merged_df[KEY_TARGET].quantile(0.0025))),
        vmax=min(14, 14),  # round(merged_df[KEY_TARGET].quantile(0.9975))),
    )
    # Define the color map (using "viridis" as an example)
    cmap = cm.viridis

    # Determine the global bounding box of the entire dataset to use consistent axis limits
    minx, miny, maxx, maxy = merged_df.total_bounds  # Total bounding box of the dataset

    # Create a consistent aspect ratio for the plot by setting the axis limits
    aspect_ratio = (maxy - miny) / (maxx - minx)

    # Iterate over unique years in the dataset and create plots
    years = sorted(
        [year for year in merged_df[KEY_YEAR].unique() if 2003 <= year <= 2023]
    )

    image_files = []

    for year in years:
        # Filter the data for the current year
        filtered_df = merged_df[merged_df[KEY_YEAR] == year]

        if not filtered_df.empty:
            # Create the plot
            fig, ax = plt.subplots(figsize=(15, 10))

            # Plot the base world map
            world.plot(
                ax=ax, color="lightgrey", edgecolor="black", linewidth=0.1
            )  # Light grey countries with black borders

            # Overlay the yield data for the selected year
            filtered_df.plot(
                column=KEY_TARGET,  # Use the actual column name for yield
                ax=ax,
                legend=False,
                cmap=cmap,
                edgecolor="black",
                linewidth=0.0,
                vmin=norm.vmin,
                vmax=norm.vmax,
            )

            # Set consistent axis limits across all years (same bounding box)
            ax.set_xlim(minx, maxx)
            ax.set_ylim(miny, maxy)
            ax.set_xticks([])  # Removes x-axis ticks (longitude)
            ax.set_yticks([])  # Removes y-axis ticks (latitude)
            # Ensure the aspect ratio is equal (prevents stretching)
            ax.set_aspect("equal", adjustable="box")
            ax.set_title(f"Yield {crop} {int(year)}", fontsize=10)
            # Add the color bar inside the plot (bottom-left corner)
            sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
            sm.set_array([])  # Empty array to create the colorbar
            cax = ax.inset_axes(
                [0.01, 0.07, 0.20, 0.03]
            )  # Positioning the color bar inside the plot
            cbar = fig.colorbar(sm, cax=cax, orientation="horizontal")
            cbar.ax.tick_params(labelsize=6, colors="grey")
            cbar.outline.set_linewidth(0.5)
            cbar.outline.set_edgecolor("grey")
            # cbar.set_label(f"Yield in {int(year)}", fontsize=12)

            # Save the plot as an image
            image_filename = os.path.join(output_dir, f"year_{int(year)}.png")
            image_files.append(image_filename)
            plt.savefig(image_filename, dpi=300, bbox_inches="tight", pad_inches=0.2)
            plt.close(fig)

    # Create the GIF using Pillow
    gif_filename = f"yield_{crop}.gif"
    images = [Image.open(image_file) for image_file in image_files]

    # Save the images as a GIF
    images[0].save(
        gif_filename, save_all=True, append_images=images[1:], duration=500, loop=0
    )
