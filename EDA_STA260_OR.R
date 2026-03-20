rm(list = ls())
library(dplyr); library(ggplot2); library(sf)
library(rnaturalearth); library(did)

setwd("C:/Users/Oscar R/Desktop")

#Importing yearlyPM25_wide data set with inclusion of commodities
yearlyPM25_wide <- readRDS("./STA 260/Final Project/yearlyPM25_commodities_wide.rds")

countryList <- readRDS("./STA 260/Final Project/12-3-25WBcountries.rds")

num_mines <- yearlyPM25_wide %>% nrow()

#----Filtering and modifying data set-------------------------------------
##---Familiarizing with data sets---------------------------------------

# Check which variables are in the data set
print(yearlyPM25_wide %>% glimpse())
print(countryList %>% glimpse())

# Checking how many countries and income levels are there
yearlyPM25_wide %>% select(USGScountry) %>% distinct(USGScountry) #127 countries
countryList %>% select(USGScountry)
countryList %>% distinct(Income) # 5 levels (One is NA)

# Checking whether countries in yearlyPM25_wide match those in countryList
countries <- yearlyPM25_wide %>% distinct(USGScountry)
all.equal(countries$USGScountry,countryList$USGScountry)

countryList2 <- countryList %>% select(-WBeconomy,-meanPM25,-aqs_numeric_ug.m3,-aqs_slices) 

# Adding some columns of countryList onto yearlyPM25_wide
yearlyPM25_wide = yearlyPM25_wide %>%  
  left_join(countryList2, by = "USGScountry")

#saveRDS(yearlyPM25_wide, file = "./STA 260/Final Project/yearlyPM25_wide.rds")

# formatting yearlyPM25_wide in long format
yearlyPM25 <- yearlyPM25_wide %>%
  select(OBJECTID, minLossYear, USGScountry, Region, Income, starts_with("pm25_")) %>%
  filter(!is.na(minLossYear)) %>%
  tidyr::pivot_longer(cols = starts_with("pm25_"),
                      names_to = "year",
                      names_prefix = "pm25_",
                      names_transform = list(year = as.numeric),
                      values_to = "pm25")

yearlyPM25 %>% distinct(OBJECTID) %>% nrow() 
yearlyPM25 %>% nrow()

# OBJECTID 47580 is the only mine in Western Sahara 
# Has missing values for country income level
yearlyPM25_wide %>% filter(USGScountry=="Western Sahara") %>% select(OBJECTID)

# Removing OBJECTID 47580
yearlyPM25_wide <- yearlyPM25_wide %>% filter(USGScountry!="Western Sahara")
yearlyPM25 <- yearlyPM25 %>% filter(USGScountry!="Western Sahara")

##---Finding mines with missing values for minLossYear and pm25----------

yearlyPM25 %>% filter(is.na(pm25) & !is.na(minLossYear)) %>% distinct(OBJECTID) %>% nrow()

# 52 mines have NaNs for pm25
IDwNAN <- yearlyPM25 %>% select(OBJECTID,year,pm25) %>% 
  filter(is.na(pm25)) %>% distinct(OBJECTID) 
IDwNAN <- IDwNAN$OBJECTID

# Now yearlyPM25 consists of obs with no NaN values for pm25
yearlyPM25 <- yearlyPM25 %>% filter(!OBJECTID %in% IDwNAN) 
yearlyPM25 %>% distinct(OBJECTID) %>% nrow() #35381 mines w/ pm25 values
MineID35381 <- yearlyPM25 %>% distinct(OBJECTID); MineID35381 <- c(MineID35381$OBJECTID)

saveRDS(yearlyPM25, file = "./STA 260/Final Project/yearlyPM25_35381.rds")

# Now yearlyPM25_wide consists of obs with no NaN values for pm25
yearlyPM25_wide <- yearlyPM25_wide %>% filter(OBJECTID %in% MineID35381) 
yearlyPM25_wide %>% nrow()

##---Balancing data by event-times -5 to  10----------

# Adding a variable for event time
#yearlyPM25 <- yearlyPM25 %>% mutate(EventTime = year - minLossYear)

min_yr <- min(yearlyPM25$year)
max_yr <- max(yearlyPM25$year)

###----Balanced---------------------------------------

# Filter mines to keep a balanced panel for the event window
yearlyPM25_balanced <- yearlyPM25 %>%
  filter(minLossYear - 5 >= min_yr & minLossYear + 10 <= max_yr)

# Filter mines to keep a balanced panel for the event window
yearlyPM25_balanced_wide <- yearlyPM25_wide %>%
  filter(minLossYear - 5 >= min_yr & minLossYear + 10 <= max_yr)

#----EDA----------------------------------------
##---Distribution of mines per region-----------

#Contingency table consisting of count of mines per region
freq <- yearlyPM25_wide %>% group_by(Region) %>% 
  summarise(count = n(), prop = n()/num_mines) %>% arrange(desc(count))
print(freq)

#Contingency table consisting of proportion of mines per countries in South Asia
yearlyPM25_wide %>% filter(Region=="East Asia & Pacific") %>% 
  group_by(USGScountry) %>% summarise(count = n()/nrow(yearlyPM25_wide))

#Barplot of mines for each region 
yearlyPM25_wide %>%
  group_by(Region) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = Region, y = count, fill = Region)) +
  geom_col() +
  labs(x = "Region", y = "Count", title = "Number of observations by region"
  ) + theme_minimal() + theme( axis.text.x = element_text(angle = 30, hjust = 1))

##----Summary Statistics for mines w/ PM2.5 measures-----------

mean(yearlyPM25$pm25) #mean PM2.5: 23.14356 (23.1432 when excluding Western Sahara)
sd(yearlyPM25$pm25) #sd Pm2.5: 16.17703 (16.17681 when excluding Western Sahara)

###----Heterogeneity of PM2.5 across Regions-------------------

# Mean PM2.5 of mine-years for each region 
yearlyPM25 %>% group_by(Region) %>% summarise(Mean = mean(pm25)) %>% 
 arrange(desc(Mean))
# <- South Asia had the highest mean PM2.5

# Times series of yearly mean PM2.5 of mines for each region 
yearlyPM25 %>% group_by(year,Region) %>% summarise(Mean = mean(pm25)) %>% 
ggplot(aes(x = year, y = Mean, color = Region, group = Region)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",y = "Temporal Mean PM2.5",
    #title = "Yearly mean PM2.5 by region"
  ) +
  theme_minimal()

yearlyPM25 %>% distinct(minLossYear) %>% arrange(minLossYear) %>% max()

##---Spatial Maps---------------------------------------------------

world_poly_mines <- st_read("C:/Users/Oscar R/Desktop/STA 260/Final Project/tang2023/74548_projected polygons.shp")
st_crs(world_poly_mines)

world_poly_mines <- st_transform(world_poly_mines, 4326) 
yearlyPM25_wide <- readRDS("./STA 260/Final Project/12-1-25tang2023_PM.rds")

#yearlyPM25_wide_PM25only = yearlyPM25_wide %>% select()

world_poly_mines <- world_poly %>% left_join(left_join(countryList2, by = "USGScountry"))

ggplot(world_poly) +
  geom_sf(fill = "steelblue", color = "black", linewidth = 0.2) +
  theme_minimal() +
  labs(title = "Global Polygon Map")

ggplot(world_poly) +
  geom_sf(fill = "steelblue", color = "black", linewidth = 0.2) +
  coord_sf(crs = "+proj=robin") +
  theme_minimal() +
  labs(title = "Global Polygon Map (Robinson Projection)")
