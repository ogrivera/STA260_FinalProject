# --- Sensitivity (Rambachan & Roth 2022) ---
# check if parallel trend holds. 
# get source from github if pkg not loading
source("https://raw.githubusercontent.com/asheshrambachan/HonestDiD/master/R/honest_did.R")

hd_results <- honest_did(
  es = gt_att_pm25_MLY,
  e = 0, # test effect at year 0
  type = "relative_magnitude", 
  Mbarvec = seq(0, 1, by = 0.1) 
)

# --- BJS (2024) Imputation check ---
# run this to compare with CS2021 results
bjs_results <- did_imputation(
  data = yearlyPM25_balanced,
  yname = "pm25",
  gname = "minLossYear",
  tname = "year",
  idname = "OBJECTID",
  cluster_var = "OBJECTID",
  pretrends = TRUE, # check pre-treatment trend
  horizon = TRUE
)

# clean bjs data for ggplot
bjs_plot_data <- bjs_results %>%
  mutate(term = as.numeric(as.character(term))) %>%
  filter(term >= -5 & term <= 9)