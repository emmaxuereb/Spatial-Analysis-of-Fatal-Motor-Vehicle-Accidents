# Load necessary libraries
library(sf)
library(tmap)
library(ggplot2)
library(viridis)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(sp)
library(spdep)
library(Matrix)
library(spatialreg)
library(spgwr)
library(geostan)
library(mapview)
library(car)
library(RColorBrewer)
library(cowplot)
library(leafsync)
library(leaflet.extras2)
library(lmtest)
library(tseries)
library(GWmodel)
library(tidyverse)
library(gridExtra)

#### Dowload Datasets

#setwd("C:/Users/isabe/Documents/Applied Data Science/Spatial and ML/Term Project")
#Globalpath<- "C:/Users/emmax/OneDrive/Desktop/ADS/Spatial/Spatial R Directory/Term Project" #change the path to your folder
#setwd(Globalpath) #use to set to your directoryshp <- st_read("Term Project/2022_Fatal_Motor_Vehicle_Accidents_(FARS).shp")

# Read a shapefile - Fatalities Dataset
shp <- st_read("2022_Fatal_Motor_Vehicle_Accidents_(FARS).shp")

# Check the coordinate reference system 
st_crs(shp)

# Reading Accidents dataset and converting to a Simple Features (sf) object using coordinates
accidents2 <- read.csv("US_Accidents_2022_filtered.csv")
US_Accidents_sf <- accidents2 %>%
  st_as_sf(coords = c("Start_Lng", "Start_Lat"), crs = 4326)  # EPSG:4326 is WGS84 (latitude/longitude)

############### Exploratory Analysis ##############

# Summary statistics for fatals
summary(shp$FATALS)

ggplot(shp) +
  geom_histogram(aes(x = FATALS), bins = 30) +
  labs(title = "Distribution of Fatalities", x = "Fatalities", y = "Count")

ggplot(data = shp) +
  geom_sf(aes(color = FATALS), size = 0.5) +
  scale_color_viridis_c() +
  ggtitle("Spatial Distribution of Fatalities by Severity")

############ Aggregating per COUNTY, Eastern ################
# Download county boundaries
counties_US <- map_dfr(state.abb, function(state) {
  counties(state = state, year = 2022, class = "sf")
})

# Ensure the CRS is set for counties data to 5070 - suitable for US
counties_US <- st_transform(counties, 5070)

# Transform the CRS for US data to match the counties' CRS
us <- st_transform(shp, st_crs(counties_US))

# Join the dataset the counties
points_neigh <- st_join(us, counties_US)

# Changing the format of County to use count_state to join with population dataset 
points_neigh$count_state <- paste(points_neigh$NAMELSAD, points_neigh$STATENAME, sep = ", ")
sort(unique(points_neigh$count_state))

# Aggregating data by COUNTY and calculating total fatalities
agg_data_US <- points_neigh %>%
  group_by(count_state) %>%  # Use COUNTY as the identifier
  summarise(total_fatalities = sum(FATALS, na.rm = TRUE))

# Join the aggregated data with the county boundaries
counties_agg_US <- st_join(counties_US, agg_data_US) %>%
  filter(!is.na(total_fatalities)) # remove where total_Fatalities were zero in the county

# Transform points to match CRS 5070 (NAD83 / Conus Albers)
US_Accidents_5070 <- st_transform(US_Accidents_sf, crs = 5070)

# Check to confirm the CRS has been successfully transformed
st_crs(US_Accidents_5070)

# Perform spatial join: Finding which point belongs to which county
US_Accidents_Joined <- st_join(US_Accidents_5070, counties_agg_US["count_state"], 
                               join = st_within, left = FALSE)

# Keep only the relevant columns: ID, geometry, count_state
US_Accidents_Final <- US_Accidents_Joined %>%
  select(ID, geometry, count_state)

# Plot the points
ggplot() +
  geom_sf(data = US_Accidents_Final, color = "red", size = 1) +
  labs(title = "Accident Locations in the US") +
  theme_minimal()

# Group by count_state and count the number of accidents for each
counties_accidents_summary <- US_Accidents_Final %>%
  group_by(count_state) %>%
  summarize(Total_Accidents = n())

# Perform a left join to add the Total_Accidents column to counties_agg_US
counties_agg_US_updated<- st_join(counties_agg_US,counties_accidents_summary)

# remove the counties with no matches between the datasets
counties_agg_US_updated <- counties_agg_US_updated[!is.na(counties_agg_US_updated$Total_Accidents), ]

counties_ps_US <- counties_agg_US_updated
counties_ps_US <- counties_ps_US %>%
  mutate(fatality_rate = total_fatalities / Total_Accidents)

backup_counties_ps_US <- counties_ps_US

# FIPS codes for Eastern U.S. states
eastern_fips <- c("23", "33", "50", "25", "09", "36", "34", "42", 
                  "10", "24", "11", "51", "54", "37", "45", "13", "12", 
                  "01", "21", "22", "28", "47", "17", "18", "26", "39", 
                  "55", "44")

## represents these:
#                 c("ME", "NH", "VT", "MA", "CT", "NY", "NJ", "PA", 
#                 "DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL",
#                 "AL", "KY", "LA", "MS", "TN",
#                  "IL", "IN", "MI", "OH", "WI","RI")

# Filter dataset based on STATEFP (FIPS code)
counties_ps_US <- counties_ps_US[counties_ps_US$STATEFP %in% eastern_fips, ]

####### Fatality Variable Analysis #########

### Checking potential outliers
counties_ps_US %>%
  arrange(desc(fatality_rate)) %>%  # Sort in descending order
  slice_head(n = 20) %>% 
  select(count_state.x, total_fatalities, Total_Accidents, fatality_rate) 
# they are cleary a problem of data collection from different sources

## Filtering counties with only 1 accident
counties_ps_US2 <- subset(counties_ps_US,counties_ps_US$Total_Accidents > 5)

### Checking potential outliers
counties_ps_US2 %>%
  arrange(desc(fatality_rate)) %>%  # Sort in descending order
  slice_head(n = 20) %>% 
  select(count_state.x, total_fatalities, Total_Accidents, fatality_rate)

backup <- counties_ps_US #<- backup
counties_ps_US <- counties_ps_US2

## Density Distribution
ggplot(data = counties_ps_US) +
  geom_density(alpha=0.8, colour="black", fill="lightblue", aes(x = fatality_rate)) +
  theme_classic()

## Distribution
summary(counties_ps_US$fatality_rate) 

### Colors defining
counties_ps_US$rate_category <- cut(
  counties_ps_US$fatality_rate,
  breaks = c(0, 0.03, 0.08, 0.2, 0.7, 3),
  labels = c("0-0.03", "0.03-0.0.8", "0.08-0.2", "0.2-0.7", "0.7-3"),
  include.lowest = TRUE
)

my_colors <- c(
  "0-0.03" = "#d4eeff",
  "0.03-0.08" = "#7abfff",
  "0.08-0.2" = "#1f5ea8",
  "0.2-0.7" = "#08306b",
  "0.7-3" = "#4b004b"
)

ggplot(data = counties_ps_US) +
  geom_sf(aes(fill = rate_category), color = "black", size = 0.1) +
  scale_fill_manual(values = my_colors, na.value = "white") +
  theme_minimal() +
  labs(title = "Fatality Rate per County", fill = "Fatality Rate Range")


###### Spatial Autocorrelation Analysis #########

#creating adjacency matrix
nbq <- poly2nb(counties_ps_US, queen=TRUE) #Queen's Contiguity neighborhood
nbq_w <- nb2listw(nbq, style="W", zero.policy = TRUE)

#### Moran's MC test
mc_global_with_outliers <- moran.mc(counties_ps_US$fatality_rate , nbq_w, 999, alternative = "greater")
mc_global_with_outliers

#plot the  Moran's I
plot(mc_global_with_outliers, main="Moran's I for Fatality Rate", xlab="Fatality Rate")

##### Local Moran's I
LISA <- localmoran(counties_ps_US$fatality_rate , nbq_w)  #using Queen's contiguity 
summary(LISA)

# extract local Moran's I values and attache them to our sf object 
counties_ps_US$LISA <- LISA[,1] 
# extract p-values
counties_ps_US$LISA_p <- LISA[,5] 

#Here we can map the local Moran's I with t-map, and show which areas have significant clusters
# map_LISA <- tm_shape(counties_ps_US) + 
#   tm_polygons(col= "LISA", title= "Local Moran's I", midpoint=0,
#               palette = "RdBu", breaks= c(-3, 0, 0.1, 1, 20)) 
# 
# #map_LISA_p <- tm_shape(counties_ps_US) + 
# #  tm_polygons(col= "LISA_p", title= "p-values",
# #              breaks= c(0, 0.01, 0.05, 0.1, 1), palette = "YlGnBu") 
# 
# map_LISA

## HH, LL, HL, LH 
significanceLevel <- 0.05; # 95% confidence, define what level of significance we will accept for a cluster to be statistically significant
meanVal <- mean(counties_ps_US$fatality_rate ); #take the mean value of the variable under consideration

LISA_type <- LISA 

#create an object storing the LISA values into different classes of significance
LISA_mapping <- LISA_type %>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr(z > 0)` > 0.05 ~ "Insignificant", #greater than 0.05, thus Insignificant 
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & counties_ps_US$fatality_rate >= meanVal ~ "HH", #High-High local Moran's I values
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & counties_ps_US$fatality_rate < meanVal ~ "LL", #Low-Low local Moran's I values
    `Pr(z > 0)` <= 0.05 & Ii < 0 & counties_ps_US$fatality_rate >= meanVal ~ "HL", #High-Low local Moran's I values
    `Pr(z > 0)` <= 0.05 & Ii < 0 & counties_ps_US$fatality_rate < meanVal ~ "LH" #Low-High local Moran's I values
  ))

#Join back to the main polygon data
counties_ps_US$coType <- LISA_mapping$coType %>% tidyr::replace_na("Insignificant")

#map it
ggplot(counties_ps_US) +
  geom_sf(aes(fill=coType)) +
  scale_fill_manual(values = c('red','#D3D3D3','cyan','blue'), name='Clusters & \nOutliers') +
  labs(title = "Fatality Rate clusters") + 
  theme (panel.background = element_rect(fill='transparent'))



################## AGGREGATION ###########
# Rename count_state to join
counties_ps_US_df$count_state <- counties_ps_US_df$count_state.x

####### Aggregating variables per County #####
### PERSONS ####
## Average of persons involved in the accident
pe_total <- points_neigh %>%
  group_by(count_state) %>%
  summarise(pe_total_ag = mean(PERSONS, na.rm = TRUE))
pe_total_df <- st_drop_geometry(pe_total)

### WEEK DAY ####
# Creating a new column WEEK_TYPE based on DAY_WEEKNAME
points_neigh$WEEK_TYPE <- ifelse(points_neigh$DAY_WEEKNA %in% 
                                   c("Friday", "Saturday", "Sunday"),
                                 "weekend", "weekday")
# Group by COUNTY and WEEK_TYPE, then count occurrences
county_weektype_counts <- points_neigh %>%
  group_by(count_state, WEEK_TYPE) %>%
  summarise(Count = n(), .groups = 'drop')

# Majority category per county (weekend or weekday)
week_total <- county_weektype_counts %>%
  group_by(count_state) %>%
  summarise(week_type = WEEK_TYPE[which.max(Count)], .groups = 'drop')
week_total_df <- st_drop_geometry(week_total)

### RUSH HOUR ####
# Creating a new column Rush Hour based on HOUR
points_neigh$Rush_Hour <- ifelse(points_neigh$HOUR == 99, "Unknown",
                                 ifelse(points_neigh$HOUR >= 6 & points_neigh$HOUR <= 9, 
                                        "Morning Rush Hour",
                                        ifelse(points_neigh$HOUR >= 16 & points_neigh$HOUR <= 19, 
                                               "Evening Rush Hour",
                                               "Not Rush Hour")))

# Group by COUNTY and Time, then count occurrences
county_time_counts <- points_neigh %>%
  group_by(count_state, Rush_Hour) %>%
  summarise(Count = n(), .groups = 'drop')

# For each county, find the majority category 
county_majority_time <- county_time_counts %>%
  group_by(count_state) %>%
  summarise(Rush_Hour = Rush_Hour[which.max(Count)], .groups = 'drop')
county_majority_time_df <- st_drop_geometry(county_majority_time)

### ROUTE CATEGORY ####
# Creating a new column Route_Category based on ROUTENAME
points_neigh$Route_Category <- ifelse(points_neigh$ROUTENAME %in% c("Interstate",
                                                                    "State Highway", "US Highway"),"Highways",
                                      ifelse(points_neigh$ROUTENAME %in% c("County Road", 
                                             "Local Street - Municipality", "Local Street - Township",
                                             "Local Street - Frontage Road"),"Local Roads", 
                                             "Other"))


# Group by COUNTY and ROUTE_CATEGORY, then count occurrences
county_route_counts <- points_neigh %>%
  group_by(count_state, Route_Category) %>%
  summarise(Count = n(), .groups = 'drop')

# For each county, find the majority route category
county_majority_route <- county_route_counts %>%
  group_by(count_state) %>%
  summarise(route_category = Route_Category[which.max(Count)], .groups = 'drop')
county_majority_route_df <- st_drop_geometry(county_majority_route)

### RURAL OR URBAN ####

# Creating a new column WEEK_TYPE based on DAY_WEEKNAME
points_neigh$RUR_URBNAM2 <- ifelse(points_neigh$RUR_URBNAM %in% 
                                   c("Urban"),
                                 "Urban", "Rural")

# Group by COUNTY and RUR_URBNAM, then count occurrences
county_rural_urban_counts <- points_neigh %>%
  group_by(count_state, RUR_URBNAM2) %>%
  summarise(Count = n(), .groups = 'drop')

# For each county, find the majority category (Rural or Urban)
county_majority_rural_urban <- county_rural_urban_counts %>%
  group_by(count_state) %>%
  summarise(Rur_Urb = RUR_URBNAM2[which.max(Count)], .groups = 'drop')

county_majority_rural_urban_df <- st_drop_geometry(county_majority_rural_urban)



### VEHICLES ####
ve_total <- points_neigh %>%
  group_by(count_state) %>%
  summarise(ve_total_ag = mean(VE_TOTAL, na.rm = TRUE))
ve_total_df <- st_drop_geometry(ve_total)

### SEASONS BASED ON MONTH ####
season_data <- points_neigh %>%
  mutate(season = case_when(
    MONTH %in% c(12, 1, 2)  ~ "Winter",
    MONTH %in% c(3, 4, 5)   ~ "Spring",
    MONTH %in% c(6, 7, 8)   ~ "Summer",
    MONTH %in% c(9, 10, 11) ~ "Fall",
    TRUE ~ NA_character_   # Handle NA cases
  )) %>%
  group_by(count_state, season) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(count_state) %>%
  slice_max(count, n = 1, with_ties = FALSE) %>%  # Keep most frequent season
  select(count_state, season)
season_data_df <- st_drop_geometry(season_data)

### NHS ####
# Group by COUNTY and NHS, then count occurrences
county_NHS_counts <- points_neigh %>%
  group_by(count_state, NHS) %>%
  summarise(Count = n(), .groups = 'drop')

# For each county, find the majority category (Rural or Urban)
county_NHS_counts <- county_NHS_counts %>%
  group_by(count_state) %>%
  summarise(nhs = NHS[which.max(Count)], .groups = 'drop')
county_NHS_counts_df <- st_drop_geometry(county_NHS_counts)

### FIRST HARM EVENT ####
## Aggregating some categories
categorize_harm_ev <- function(code) {
  code <- as.integer(code)
  if (code %in% c(12, 13, 14, 54, 55)) {
    return("Collision with Vehicle")
  } else if (code %in% c(8, 9, 11, 15, 49)) {
    return("Non-Motorist Involved")
  } else if (code %in% c(19:26, 28, 30:43, 46, 50, 52, 53, 59)) {
    return("Fixed Object Collision")
  } else if (code %in% c(1:7, 16:18, 31, 45, 72, 73)) {
    return("Non-Collision Event")
  } else if (code %in% c(34:36, 44, 58, 91, 93)) {
    return("Environmental / Road Condition")
  } else {
    return("Unknown")
  }
}

# Apply to your data
points_neigh$Harm_Category <- sapply(points_neigh$HARM_EV, categorize_harm_ev)

# Group by COUNTY and RUR_URBNAM, then count occurrences
county_HARM_EV_counts <- points_neigh %>%
  group_by(count_state, Harm_Category) %>%
  summarise(Count = n(), .groups = 'drop')

# For each county, find the majority category (Rural or Urban)
county_HARM_EV_counts <- county_HARM_EV_counts %>%
  group_by(count_state) %>%
  summarise(harm_ev = Harm_Category[which.max(Count)], .groups = 'drop')
county_HARM_EV_counts_df <- st_drop_geometry(county_HARM_EV_counts)


### LIGHT CONDITIONS #####
# Group by COUNTY and RUR_URBNAM, then count occurrences
points_neigh <- points_neigh %>%
  mutate(lgt_status = case_when(
    LGT_COND %in% c(1, 3, 4) ~ "Lighted",
    LGT_COND %in% c(2, 5) ~ "Not Lighted",
    TRUE ~ "Unknown"
  ))

county_LGT_COND_counts <- points_neigh %>%
  group_by(count_state, lgt_status) %>%
  summarise(Count = n(), .groups = 'drop')

county_LGT_COND_counts <- county_LGT_COND_counts %>%
  group_by(count_state) %>%
  summarise(lgt_status = lgt_status[which.max(Count)], .groups = 'drop')

county_LGT_COND_counts$lgt_status <- as.factor(county_LGT_COND_counts$lgt_status)

table(county_LGT_COND_counts$lgt_status)

# Join with your spatial data
county_LGT_COND_counts_df <- st_drop_geometry(county_LGT_COND_counts)

### WEATHER OLD ####
# Creating a new column Time based on HOUR
points_neigh$WEATHER <-  as.factor(points_neigh$WEATHER)
points_neigh$WEATHER_new <- ifelse(points_neigh$WEATHER %in% c("99","98"), "Unknown",
                                   ifelse(points_neigh$WEATHER %in% c("1"), "Clear",
                                   ifelse(points_neigh$WEATHER %in% c("2", "3", "4", "11", "12"), "Rain/Snow",
                                   ifelse(points_neigh$WEATHER %in% c("5"), "Fog/Smog/Smoke",
                                   ifelse(points_neigh$WEATHER %in% c("6", "7"), "Wind/Dust Conditions",
                                   ifelse(points_neigh$WEATHER %in% c("10"), "Cloudy",
                                   "Other"))))))


# Group by COUNTY and Time, then count occurrences
county_weather_counts <- points_neigh %>%
  group_by(count_state, WEATHER_new) %>%
  summarise(Count = n(), .groups = 'drop')

# For each county, find the majority category 
county_weather_counts <- county_weather_counts %>%
  group_by(count_state) %>%
  summarise(weather = WEATHER_new[which.max(Count)], .groups = 'drop')

county_weather_counts$weather <- as.factor(county_weather_counts$weather)
# Join with your spatial data
county_weather_counts_df <- st_drop_geometry(county_weather_counts)




### WEATHER Less categories ####
# Creating a new column Time based on HOUR
points_neigh$WEATHER_LESS_CAT <- ifelse(!(points_neigh$WEATHER == 1), "Not Clear",
                                                                      "Clear")

# Group by COUNTY and Time, then count occurrences
county_weather_counts2 <- points_neigh %>%
  group_by(count_state, WEATHER_LESS_CAT) %>%
  summarise(Count = n(), .groups = 'drop')

# For each county, find the majority category 
county_weather_counts2 <- county_weather_counts2 %>%
  group_by(count_state) %>%
  summarise(weather2 = WEATHER_LESS_CAT[which.max(Count)], .groups = 'drop')

county_weather_counts2$weather2 <- as.factor(county_weather_counts2$weather2)
# Join with your spatial data
county_weather_counts2 <- st_drop_geometry(county_weather_counts2)




######## Join all aggregated variables ####

# Keep the geometry separately before dropping
counties_ps_US_geom <- st_geometry(counties_ps_US)

counties_ps_US$count_state <- counties_ps_US$count_state.x

# Drop geometry to perform the join
counties_ps_US_df <- st_drop_geometry(counties_ps_US)

# Join All Aggregations
result_df <- counties_ps_US_df %>%
  inner_join(pe_total_df, by = "count_state") %>%
  inner_join(week_total_df, by = "count_state") %>%
  inner_join(county_majority_time_df, by = "count_state") %>%
  inner_join(county_majority_route_df, by = "count_state") %>%
  inner_join(county_majority_rural_urban_df, by = "count_state") %>%
  inner_join(ve_total_df, by = "count_state") %>%
  inner_join(season_data_df, by = "count_state") %>%
  inner_join(month_data_df, by = "count_state") %>%
  inner_join(county_NHS_counts_df, by = "count_state") %>%
  inner_join(county_HARM_EV_counts_df, by = "count_state") %>%
  inner_join(county_LGT_COND_counts_df, by = "count_state") %>%
  inner_join(county_weather_counts_df, by = "count_state") %>%
  inner_join(county_weather_counts2, by = "count_state")

# Reattach geometry and set CRS
st_geometry(result_df) <- counties_ps_US_geom
st_crs(result_df) <- st_crs(counties_ps_US)

result_df$week_type <- as.factor(result_df$week_type)
result_df$Rush_Hour <- as.factor(result_df$Rush_Hour)
result_df$route_category <- as.factor(result_df$route_category)
result_df$Rur_Urb <- as.factor(result_df$Rur_Urb)
result_df$season <- as.factor(result_df$season)
result_df$harm_ev <- as.factor(result_df$harm_ev)
result_df$nhs <- as.factor(result_df$nhs)
result_df$MONTH <- as.factor(result_df$MONTH)
summary(result_df)




################ SAC & OLS ########

# Changing reference
result_df$Rush_Hour <- relevel(result_df$Rush_Hour, ref = "Not Rush Hour")
result_df$Rur_Urb <- relevel(result_df$Rur_Urb, ref = "Urban")
result_df$harm_ev <- relevel(result_df$harm_ev, ref = "Non-Collision Event")

model_all <- fatality_rate ~ pe_total_ag +  ve_total_ag +
  week_type + Rush_Hour + route_category + Rur_Urb +
  harm_ev  + nhs + season + weather

#creating adjacency matrix
adj_matrix <- poly2nb(result_df, queen=TRUE) #Queen's Contiguity neighborhood, if we put queen= FALSE, it will use Rook's case contiguity
#Print the summary of the matrix
#summary(adj_matrix)

adj_matrix_w <- nb2listw(adj_matrix, style="W", zero.policy = TRUE) #Queen's neighborhood wights; # W is row standardized (sums over all links to n); zero.policy = 1 means that some obs may have no adjacent obs
#summary(greendata_nbq_w)

#run the model
linearMod <- lm (model_all, data = result_df) 
summary(linearMod)

#Usually we use the moran.test function to get the Moran's I, but that method is sensitive to irregularly distributed polygons. So we would use Monte Carlo method to bootstrap different polygon distribution. Here moran.mc() funtion does the job
mc_global_OLS <- moran.mc(linearMod$residuals, adj_matrix_w, 2999, zero.policy= TRUE, alternative="greater")
mc_global_OLS
#plot the  Moran's I
plot(mc_global_OLS)

#Now plot the residual on the polygon
# result_df$res_lm <- residuals(linearMod)
# lmres <- qtm(result_df, "res_lm")
# lmres 

res <- lm.RStests(linearMod, listw = adj_matrix_w, test = "all") #here we give the model and W matrix w created earlier
tres <- t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value))) #create a table format from the results of lm.LMtests
colnames(tres) <- c("Statistic", "df", "p-value") 
tres

######## SAC

#Fit SAC Model
sac_model <- sacsarlm(model_all, data = result_df, listw = adj_matrix_w, type="sac",zero.policy = TRUE)
summary(sac_model)

# Order we choose each variables, run one by one
# Remove pe_total_ag
# Remove ve_total_ag
# Remove week_type           
# Remove route_category
# Remove season
# Remove weather

model_all_reduced <- fatality_rate ~ 
    Rush_Hour+ Rur_Urb +
    harm_ev  + nhs

#Fit SAC Model
sac_model_reduced <- sacsarlm(model_all_reduced, data = result_df, listw = adj_matrix_w, type="sac",zero.policy = TRUE)
summary(sac_model_reduced)

#check residual autocorrelation
mcSEM_global_sac_reduced <-moran.mc(sac_model_reduced$residuals, adj_matrix_w, 2999, alternative="greater")
mcSEM_global_sac_reduced

# Add SAC residuals to spatial dataset
result_df$res_sac <- residuals(sac_model)

# Plot residuals using tmap
# sac_res_map <- qtm(result_df, "res_sac")
# sac_res_map

################ GWR ####

# Changing reference
result_df$Rush_Hour <- relevel(result_df$Rush_Hour, ref = "Not Rush Hour")
result_df$Rur_Urb <- relevel(result_df$Rur_Urb, ref = "Urban")
result_df$harm_ev <- relevel(result_df$harm_ev, ref = "Non-Collision Event")

model_all <- fatality_rate ~ 
  Rush_Hour+ Rur_Urb +
  harm_ev  + nhs

result_df_sp <- as_Spatial(result_df)

result_df_sp <- result_df_sp[!is.na(result_df_sp$fatality_rate), ]

abw <- bw.gwr(model_all,
              approach = "AIC", #specified by CV for cross-validation approach or by AIC corrected (AICc), we used AIC 
              adaptive = TRUE,
              kernel="gaussian", #this can be different function e.g., bisquare,exponential, depend on the prior understanding or choice of the modeler
              data=result_df_sp) #give the sp data created earlier
abw
#fitting the model with gwr.basic function
a.gwr <- gwr.basic(model_all, #the equation
                   adaptive = TRUE,
                   kernel="gaussian", #indicate the Kernel again
                   bw = abw, #give the optimal bandwidth we found in the last stage
                   data=result_df_sp) 

#print the model result
a.gwr

#get the SDF out of modle object as an sf object using st_as_sf function
agwr_sf = st_as_sf(a.gwr$SDF)

## Plots
plot1 <- ggplot(agwr_sf) +
  geom_sf(aes(fill = `Rush_HourMorning Rush Hour`)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Morning Rush Hour vs. Non-Rush", fill = "Coefficient") +
  theme_minimal()

plot2 <- ggplot(agwr_sf) +
  geom_sf(aes(fill = `Rush_HourEvening Rush Hour`)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Evening Rush Hour vs. Non-Rush", fill = "Evening Rush Hour Coefficient") +
  theme_minimal()

plot3 <- ggplot(agwr_sf) +
  geom_sf(aes(fill = `Rur_UrbRural`)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Rural vs. Urban Effect", fill = "Rural Coefficient") +
  theme_minimal()

plot4 <- ggplot(agwr_sf) +
  geom_sf(aes(fill = `harm_evCollision with Vehicle`)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Collision with Vehicle vs Non-Collision", fill = "Coefficient") +
  theme_minimal()

plot5 <- ggplot(agwr_sf) +
  geom_sf(aes(fill = `harm_evCollision with Vehicle`)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Collision with Fixed Object vs Non-Collision", fill = "Coefficient") +
  theme_minimal()

plot6 <- ggplot(agwr_sf) +
  geom_sf(aes(fill = `nhs1`)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "From NHS vs. Not NHS", fill = "Coefficient") +
  theme_minimal()

# Arrange the plots in a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2)

plot7 <- ggplot(agwr_sf) +
  geom_sf(aes(fill = `Local_R2`)) +
  scale_fill_gradient2(low = "white", high = "darkgreen", midpoint = 0) +
  labs(title = "Local R²", fill = "Coefficient") +
  theme_minimal()
plot7
