
library(did)
library(patchwork)
library(dplyr)


incs <- c("High income", "Upper middle income", "Lower middle income", "Low income")
p_list <- list()

for (i in incs) {
  tmp <- df_bal %>% filter(Income == i)
  if(length(unique(tmp$OBJECTID)) < 20) next # skip if n small
  
  # run did for each inc group
  m_tmp <- att_gt(yname="pm25", tname="year", idname="OBJECTID", gname="minLossYear",
                  data=tmp, control_group="notyettreated", bstrap=TRUE)
  
  a_tmp <- aggte(m_tmp, type="dynamic", min_e=-3, max_e=6)
  p_list[[i]] <- ggdid(a_tmp) + labs(title = i) + theme_minimal()
}

# combine plots (2x2)
(p_list[[1]] | p_list[[2]]) / (p_list[[3]] | p_list[[4]])
ggsave("./plots/inc_hetero.png", width = 12, height = 8)


# --- Baseline Pollution Split ---
# calc pre-treatment avg for each mine
base_pm <- df_bal %>%
  filter(year < minLossYear) %>% # check EventTime < 0
  group_by(OBJECTID) %>%
  summarize(m_pre = mean(pm25, na.rm = TRUE))

# split by median (Dirty vs Clean)
med_val <- median(base_pm$m_pre, na.rm = TRUE)
base_pm <- base_pm %>% mutate(Group = ifelse(m_pre >= med_val, "High", "Low"))

df_het <- df_bal %>% left_join(base_pm %>% select(OBJECTID, Group), by = "OBJECTID")

# compare high vs low base
m_high <- att_gt(yname="pm25", tname="year", idname="OBJECTID", gname="minLossYear", 
                 data=filter(df_het, Group=="High"), control_group="notyettreated")
m_low <- att_gt(yname="pm25", tname="year", idname="OBJECTID", gname="minLossYear", 
                data=filter(df_het, Group=="Low"), control_group="notyettreated")

# check summary in console
summary(aggte(m_high, type="dynamic"))
summary(aggte(m_low, type="dynamic"))