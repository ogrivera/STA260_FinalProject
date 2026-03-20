rm(list = ls())
library(dplyr); library(ggplot2); library(sf)
library(rnaturalearth); library(did)

setwd("/Users/bernabecanopaez/Desktop/Winter 2026/STA 260/Final Project")

#Importing yearlyPM25_wide data set with inclusion of commodities
yearlyPM25_wide <- readRDS("./data/yearlyPM25_commodities_wide.rds")

countryList <- readRDS("./data/12-3-25WBcountries.rds")

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

# formatting yearlyPM25_wide in long format
yearlyPM25 <- yearlyPM25_wide %>%
  select(OBJECTID, minLossYear, USGScountry, Region, Income, starts_with("pm25_"),commodity_list,
         has_crit, has_notcrit) %>% ## considering the Critical and Non critical mines
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

min_yr <- min(yearlyPM25$year)
max_yr <- max(yearlyPM25$year)

# Filter mines to keep a balanced panel for the event window
yearlyPM25_balanced <- yearlyPM25 %>%
  filter(minLossYear - 5 >= min_yr & minLossYear + 10 <= max_yr)

# Now yearlyPM25_wide consists of obs with no NaN values for pm25
yearlyPM25_wide <- yearlyPM25_wide %>% filter(!OBJECTID %in% IDwNAN) 
yearlyPM25_wide %>% nrow()

#----Using Did method-----------------------------------------------

# Adding a variable for event time
yearlyPM25 <- yearlyPM25 %>% mutate(EventTime = year - minLossYear)

# --------------------------------------------- #
# Running DiD by Critical vs Non Critical Mines #
# --------------------------------------------- #

yearlyPM25_crit_mines <- yearlyPM25_balanced %>% ## Only Critical mines 
  filter(has_crit == TRUE & has_notcrit == FALSE)  #%>% nrow() / 560534

yearlyPM25_balanced %>% ## Only Critical mines 
  filter(has_crit == TRUE & has_notcrit == FALSE)  %>% nrow() / 560534
yearlyPM25_balanced %>% ## Only Not Critical mines 
  filter(has_crit == FALSE & has_notcrit == TRUE)  %>% nrow() #/ 560534
yearlyPM25_balanced %>% ## Mines with both critical and non-critical commodities
  filter(has_crit == TRUE & has_notcrit == TRUE)  %>% nrow() / 560534
yearlyPM25_balanced  %>% ## Mines with only Unclassified commodities
  filter(has_crit == FALSE & has_notcrit == FALSE)  %>% nrow() / 560534

DiDgt_att_pm25_crit_mines <- att_gt(
  yname   = "pm25",
  tname   = "year",
  idname  = "OBJECTID",
  gname   = "minLossYear",
  data    = yearlyPM25_crit_mines,
  xformla = NULL,
  base_period = "varying",
  clustervars = "OBJECTID",
  control_group = "notyettreated",
  bstrap = TRUE,
  cband = TRUE,
  anticipation = 0 ### try this 
)

agg_att_pm25_crit_mines = did::aggte(DiDgt_att_pm25_crit_mines, type = "dynamic", min_e = -5, max_e = 10)

p1 <- did::ggdid(agg_att_pm25_crit_mines, title = "Event-Study DiD: PM2.5 Effects for Critical-Commodity Mines (e = -5 to 10)") +
  theme(
    axis.text.x = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, color = "steelblue", face = "bold")
  )
print(p1)

#----------Only Non-Critical mines 
yearlyPM25_non_crit_mines <- yearlyPM25_balanced %>% ## Only Non-Critical mines 
  filter(has_crit == FALSE & has_notcrit == TRUE)  #%>% nrow() / 919906

DiDgt_att_pm25_non_crit_mines <- att_gt(
  yname   = "pm25",
  tname   = "year",
  idname  = "OBJECTID",
  gname   = "minLossYear",
  data    = yearlyPM25_non_crit_mines,
  xformla = NULL,
  base_period = "varying",
  clustervars = "OBJECTID",
  control_group = "notyettreated",
  bstrap = TRUE,
  cband = TRUE,
  anticipation = 0 ### try this 
)

agg_att_pm25_non_crit_mines = did::aggte(DiDgt_att_pm25_non_crit_mines, type = "dynamic", min_e = -5, max_e = 10)

p2 <- did::ggdid(agg_att_pm25_non_crit_mines, title = "Event-Study DiD: PM2.5 Effects for Non-Critical-Commodity Mines (e = -5 to 10)") +
  theme(
    axis.text.x = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, color = "steelblue", face = "bold")
  )
print(p2)

#----------Only Unclassified mines 
yearlyPM25_unclas_mines <- yearlyPM25_balanced %>% ## Only Non-Critical mines 
  filter(has_crit == FALSE & has_notcrit == FALSE)  #%>% nrow() / 919906

DiDgt_att_pm25_unclas_mines <- att_gt(
  yname   = "pm25",
  tname   = "year",
  idname  = "OBJECTID",
  gname   = "minLossYear",
  data    = yearlyPM25_unclas_mines,
  xformla = NULL,
  base_period = "varying",
  clustervars = "OBJECTID",
  control_group = "notyettreated",
  bstrap = TRUE,
  cband = TRUE,
  anticipation = 0 ### try this 
)

agg_att_pm25_unclas_mines = did::aggte(DiDgt_att_pm25_unclas_mines, type = "dynamic", min_e = -5, max_e = 10)

p3 <- did::ggdid(agg_att_pm25_unclas_mines, title = "Event-Study DiD: PM2.5 Effects for Unclassified-Commodity Mines (e = -5 to 10)") +
  theme(
    axis.text.x = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, color = "steelblue", face = "bold")
  )
print(p3)


# _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
# -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
agg_to_df <- function(agg_obj, group_name) {
  # Usa el crítico del objeto (coherente con did::ggdid y cband=TRUE)
  crit <- if (!is.null(agg_obj$crit.val.egt)) {
    agg_obj$crit.val.egt
  } else if (!is.null(agg_obj$crit.val)) {
    agg_obj$crit.val
  } else {
    qnorm(0.975)  # fallback 95% punto a punto
  }
  
  data.frame(
    e = agg_obj$egt,
    att = agg_obj$att.egt,
    se = agg_obj$se.egt,
    lci = agg_obj$att.egt - crit * agg_obj$se.egt,
    uci = agg_obj$att.egt + crit * agg_obj$se.egt,
    group = group_name
  )
}


df_plot <- bind_rows(
  agg_to_df(agg_att_pm25_crit_mines, "Critical"),
  agg_to_df(agg_att_pm25_non_crit_mines, "Non-Critical")#,
  #agg_to_df(agg_att_pm25_unclas_mines, "Unclassified")
)

p_compare <- ggplot(df_plot, aes(x = e, y = att, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    width = 0.15, linewidth = 0.8,
    position = position_dodge(width = 0.25)
  ) +
  geom_point(
    size = 2.2,
    position = position_dodge(width = 0.25)
  ) +
  labs(
    title = "Event-Study DiD: PM2.5 Effects by Commodity Type",
    x = "Event-Time (Number of year since mining onset)",
    y = "PM 2.5 GT Avg. Treatment Effects",
    color = NULL
  ) +
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

print(p_compare)
p1/p2

ggsave(
  filename = "./DiD/crit_hetero.png",
  plot = p_compare,
  width = 10,      # pulgadas
  height = 6,      # pulgadas
  dpi = 300,
  units = "in",
  bg = "white"
)
