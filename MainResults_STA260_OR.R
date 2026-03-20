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

saveRDS(yearlyPM25, file = "./STA 260/Final Project/yearlyPM25_35381.rds")

# Now yearlyPM25_wide consists of obs with no NaN values for pm25
yearlyPM25_wide <- yearlyPM25_wide %>% filter(!OBJECTID %in% IDwNAN) 
yearlyPM25_wide %>% nrow()

#----Using Unconditional Parallel trends Did -----------------------------------------------

# Adding a variable for event time
#yearlyPM25 <- yearlyPM25 %>% mutate(EventTime = year - minLossYear)

min_yr <- min(yearlyPM25$year)
max_yr <- max(yearlyPM25$year)

##----Balanced---------------------------------------

# Filter mines to keep a balanced panel for the event window
yearlyPM25_balanced <- yearlyPM25 %>%
  filter(minLossYear - 5 >= min_yr & minLossYear + 10 <= max_yr)

saveRDS(yearlyPM25_balanced, file = "./STA 260/Final Project/yearlyPM25_balanced.rds")

# Estimating ATT's using unconditional paralell trends
gt_att_pm25_B <- att_gt(
  yname   = "pm25",
  tname   = "year",
  idname  = "OBJECTID",
  gname   = "minLossYear",
  data    = yearlyPM25_balanced,#yearlyPM25,
  xformla = NULL,
  base_period = "varying",
  clustervars = "OBJECTID",
  control_group = "notyettreated",
  bstrap = TRUE,
  cband = TRUE,
  anticipation = 0 ### try this 
)

# Obtaining aggregate group time ATT estimates
gt_att_est_pm25_B <- aggte(gt_att_pm25_B, type = "dynamic", min_e = -5, max_e = 10)

p_B <- ggdid(gt_att_est_pm25_B) + theme(axis.text.x = element_text(size = 7)) + 
  labs(x="Event-Time (Number of year since mining onset)",
       y = "PM 2.5 GT Avg. Treatment Effects") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) + 
  theme_bw() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      color = "steelblue",
      face = "bold",
      size = 20
    ),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.position = "top",
    legend.title = element_blank(), 
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.4, "lines"),
    panel.grid.minor = element_blank()
  ) + scale_x_continuous(
    breaks = -5:10,
    labels = -5:10
  )
  #geom_vline(xintercept = 0,linetype="dashed") +
  #theme_minimal()
print(p_B)

###----Analysis of Heterogeneity by Region----------------
# Regions
conditions <- list(
  "East Asia & Pacific" = quote(Region == "East Asia & Pacific"),
  
  "Europe & Central Asia" = quote(Region == "Europe & Central Asia"),
  
  "North America" = quote(Region == "North America"),
  
  "Sub-Saharan Africa" = quote(Region == "Sub-Saharan Africa"),
  
  "Latin America & Caribbean" = quote(Region == "Latin America & Caribbean"),
  
  "South Asia" = quote(Region == "North America"),
  
  "Middle East, North Africa, Afghanistan & Pakistan" =
    quote(Region == "Middle East, North Africa, Afghanistan & Pakistan")
)

mod_list <- list()
agg_list <- list()

###----Balanced---------------------------------------

for (nm in names(conditions)) {
  cond <- conditions[[nm]]
  
  message("Processing: ", nm)
  
  filtered_ids <- yearlyPM25_wide %>%
    filter(rlang::eval_tidy(cond)) %>%
    pull(OBJECTID)
  
  filtered_data <- yearlyPM25_balanced %>%
    filter(OBJECTID %in% filtered_ids)
  
  mod_list[[nm]] <- did::att_gt(
    # yname   = "logpm25",
    yname   = "pm25",
    tname   = "year",
    idname  = "OBJECTID",
    gname   = "minLossYear",
    data    = filtered_data,
    xformla = NULL,
    base_period = "varying",
    clustervars = "OBJECTID",
    control_group = "notyettreated",
    bstrap = TRUE,
    cband = TRUE,
    #anticipation = 1 ### try this 
  )
  
  # 4. event-study estimates
  agg_list[[nm]] <- did::aggte(mod_list[[nm]], type = "dynamic", min_e = -5, max_e = 10)
}

DiD_att_Region <- list() 

for (nm in names(conditions)){
  DiD_att_Region[[nm]] <- did::ggdid(agg_list[[nm]], title = nm) +
    #theme(axis.text.x = element_text(size = 7)) + 
    labs(x="Event-Time (Number of years since mining onset)",
         y = "PM 2.5 GT Avg. Treatment Effects") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) + 
    theme_bw() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        color = "steelblue",
        face = "bold",
        size = 20
      ),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13),
      legend.position = "top",
      legend.title = element_blank(), 
      legend.text = element_text(size = 15),
      legend.key.size = unit(1.4, "lines"),
      panel.grid.minor = element_blank()
    ) + scale_x_continuous(
      breaks = -5:10,
      labels = -5:10
    )
}

# Obtaining 2 2 X 2 gridded plots of GT ATT for the 7 regions
DiD_Region_plot1 <- wrap_plots(DiD_att_Region[1:4], ncol = 2)
DiD_Region_plot2 <- wrap_plots(DiD_att_Region[5:7], ncol = 2)

# Saving 2 X 2 gridded plots as png files
ggsave("./STA 260/Final Project/MR_DiD_Region_Part1_STA260.png", DiD_Region_plot1, width = 12, height = 10, dpi = 300)
ggsave("./STA 260/Final Project/MR_DiD_Region_Part2_STA260.png", DiD_Region_plot2, width = 12, height = 10, dpi = 300)

##----Unbalanced---------------------------------------

gt_att_pm25_NB <- att_gt(
  yname   = "pm25",
  tname   = "year",
  idname  = "OBJECTID",
  gname   = "minLossYear",
  data    = yearlyPM25,#yearlyPM25,
  xformla = NULL,
  base_period = "varying",
  clustervars = "OBJECTID",
  control_group = "notyettreated",
  bstrap = TRUE,
  cband = TRUE,
  anticipation = 1 ### try this 
)

# Obtaining aggregate group time ATT estimates
gt_att_est_pm25_NB <- aggte(gt_att_pm25_NB, type = "dynamic")#, min_e = -10, max_e = 15)

p_NB <- ggdid(gt_att_est_pm25_NB) + theme(axis.text.x = element_text(size = 7)) + 
  labs(x="Event-Time (Number of year since mining onset)",y = "PM 2.5 GT Avg. Treatment Effects") + 
  geom_vline(xintercept = 0,linetype="dashed") + theme_minimal()
print(p_NB)

yearlyPM25_balanced %>% group_by(Region) %>% summarise(count=n()) 
