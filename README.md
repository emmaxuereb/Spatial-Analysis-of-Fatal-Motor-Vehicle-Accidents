# Spatial Analysis of Fatal Motor Vehicle Accidents (Eastern U.S. - 2022)

This project analyzes the fatality rate of motor vehicle accidents across counties in the Eastern United States using spatial statistical methods in R.

## Files Used

- `2022_Fatal_Motor_Vehicle_Accidents_(FARS).shp`: Shapefile of fatal accidents from the Fatality Analysis Reporting System.
- `US_Accidents_2022_filtered.csv`: Dataset of Eastern U.S. traffic accidents in 2022 (filtered for relevant records).

## Data Source

The data was sourced from:  
[US Accidents (Kaggle)](https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents?resource=download)  
Only data from Eastern U.S. states was used for the analysis.

## Methods

This analysis involves:

- Data cleaning, aggregation per county, and merging multiple accident datasets
- Calculation of fatality rate as:  
  `fatality rate = total fatalities / total accidents`
- Exploratory spatial data analysis:
  - Distribution plots and spatial maps
  - Spatial clustering using Moran’s I and LISA
- Spatial modeling using:
  - Ordinary Least Squares (OLS)
  - Spatial Autoregressive Combined (SAC)
  - Geographically Weighted Regression (GWR)

## R Packages Used

Key packages include:

- `sf`, `tmap`, `ggplot2`, `dplyr`, `readr`, `spdep`, `spatialreg`, `spgwr`, `GWmodel`, `leaflet`, `cowplot`, `Matrix`

## Notes

- The study focuses on the eastern U.S. states.
- Counties with very few reported accidents were excluded to avoid distortions in fatality rate calculations.
