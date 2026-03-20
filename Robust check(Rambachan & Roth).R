source("https://raw.githubusercontent.com/asheshrambachan/HonestDiD/master/R/honest_did.R")
library(HonestDiD) 




# Run sensitivity analysis 
hd_results <- honest_did(
  es = gt_att_pm25_MLY,
  e = 0,                             
  type = "relative_magnitude",       
  Mbarvec = seq(0, 1, by = 0.1)      
)

# Plot 
sens_plot <- createSensitivityPlot_relativeMagnitudes(
  hd_results$robust_ci,
  hd_results$orig_ci
) +
  labs(
    title = "Sensitivity Analysis (Rambachan & Roth, 2022)",
    x = "M (Relative Bound on Pre-trend Deviations)",
    y = "ATT at e = 0",
    color = "Inference Method" 
  ) +
  # 
  scale_color_manual(
    values = c("red", "black"), 
    labels = c("Robust (HonestDiD)", "Baseline (DiD)")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") 

print(sens_plot)

# 
ggsave("sensitivity.png", plot = sens_plot, width = 8, height = 6, dpi = 300)
```