rm(list = ls())
library(dplyr); library(ggplot2)
library(stringr); library(purrr); library(tidyr)

#setwd("/Users/bernabecanopaez/Desktop/Winter 2026/STA 260/Final Project")
countryList <- readRDS("./data/12-3-25WBcountries.rds")
yearlyPM25_wide <- readRDS("./CleanData/yearlyPM25_wide.rds"); length(unique(yearlyPM25_wide$OBJECTID))
yearlyPM25 <- readRDS("./CleanData/yearlyPM25.rds"); length(unique(yearlyPM25$OBJECTID))

#-----------------------------------
# Filtering the 35381 selected mines
#-----------------------------------
filtered_mines = (unique(yearlyPM25_wide$OBJECTID) %in% unique(yearlyPM25$OBJECTID))
yearlyPM25_wide_filtered = yearlyPM25_wide[filtered_mines,]

#-----------------------------------
# Without filtering the 35381 selected mines
#-----------------------------------
# yearlyPM25_wide_filtered <- readRDS("./data/12-1-25tang2023_PM.rds")

## Preprosecing the commodities:
yearlyPM25_wide_filtered$USGScommodity <- yearlyPM25_wide_filtered$USGScommodity %>%
  str_to_lower() %>%
  str_replace_all("-", "|") %>%
  str_replace_all(",", "|") %>%
  str_remove_all(" ")

yearlyPM25_wide_filtered$USGScommodity %>% # unique() %>%
  .[!str_detect(., fixed("("))] %>% # length() ## 313 different commodities: Filtering those that don't have parenthesis "("
  str_split("\\|") %>%
  unlist() %>%
  str_squish() %>% unique() -> mins ## 111 different commodities

commodities_crit4renew <- c(
  "gold",
  "copper",
  "silver",
  "zinc",
  "lead",
  "iron ore",
  "nickel",
  "molybdenum",
  "cobalt",
  "platinum",
  "palladium",
  "lanthanides",
  "lithium",
  "tin",
  "tungsten",
  "manganese",
  "graphite",
  "vanadium",
  "bauxite",
  "tantalum",
  "chromite",
  "antimony",
  "titanium",
  "niobium",
  "zircon",
  "yttrium",
  "scandium",
  "chromium",
  "alumina",
  "aluminum",
  "platinum group metals", 
  "ree" ## added to critical commodities for renewable energies
)

commodities_notcrit4renew <- c(
  "coal",
  "U3O8",
  "diamonds",
  "gemstones", ## jewelry/decoration
  "phosphate",
  "potash",
  "potassium", ## added to not critical commodities for renewable energies
  "ilmenite",
  "rutile",
  "borates", 
  "halite",    ## Salt
  "phosphorus",## Agriculture (fertilizer) and manufacturing
  "sylvite",   ## Agriculture (fertilizer)
  "sulfur",    ## used to manufacture sulfuric acid for phosphate fertilizers
  "sodium",
  ## Used for construction:
  "gypsum", 
  "limestone", 
  "calcite", 
  "barite",
  "calciumcarbonate", 
  "marble",
  "dolomite",
  #"silica",   #?
  # Cosmetics, Jewery and Ceramics:
  "clay",
  "talc", 
  "kaolin", 
  "feldspar", 
  "mica",
  "magnesite", #plus medicine
  "bentonite", #cat litter
  "asbestos",  #fireproofing and thermal insulation in buildings
  "mercury",   #used in specialized industrial thermometers, in the extraction of gold
  "bromine"   #used in the production of flame retardants for electronics and textiles
)


intersect(mins, commodities_crit4renew) ## -> 25 (missing 3 more minerals) = (7) - (2){scandium and yttrium cosidered in REEs} - (2){platinum and palladium considered in the PGMs}
intersect(mins, commodities_notcrit4renew) ## -> 5 (missing 4 more minerals)
setdiff(mins, union(commodities_crit4renew, commodities_notcrit4renew)) # 78 unclassfiified commodities
setdiff(commodities_crit4renew, mins)

# 20 Special characters: "(...)"
yearlyPM25_wide_filtered$USGScommodity %>% .[str_detect(., fixed("("))] %>% unique()
yearlyPM25_wide_filtered$USGScommodity %>% .[str_detect(., fixed("("))] %>% length()

# NEW FINDINGS:
#   0) Niobium (formerly columbium, Cb) => 
#         we can change "columbium" and "columbium(niobium)" into "niobium"
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("columbium(niobium)"), "niobium") # Replacing "columbium(niobium)" by "niobium"
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("columbium"), fixed("niobium")) # Replacing "columbium" by "niobium"
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                                      fixed("kaolin.niobium"), fixed("niobium"))

# NEW FINDINGS:
#   2) "hectorite(lithium|richclaymineral)" => lithium ?
#   3) pge: platinum group elements → platinum group metals 
##          they are the same but PGE refers to Chemestry elements and PGM's to economic contexts
###                the elements are platinum (Pt), palladium (Pd), rhodium (Rh), ruthenium (Ru), iridium (Ir), and osmium (Os)
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("pge"), fixed("platinum group metals"))
#   4) uranium → corresponds to U3O8
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("uranium"), "U3O8") # Replacing "uranium" by "U3O8"
#   5) iron (Fe:pure element), magnetite (Mn: pure element)→ iron ore (mixed element extracted: not pure element)
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("iron"), fixed("iron ore"))
#   6) potassium(K): nutrition   →? potash (K_2SO_4): fertilizer => It does not matter as they are not important for renewable energies
#   7) sylvite (KCl): fertilizer →? Not Critical (as potash)
#   8) rareearthelements, rareearths, ree (Rare Earth Elements) ?? → Lanthanides
##                             REE = {lanthanides} union {scandium, yttrium}
###                                            but scandium and yttrium are not in the minerals
####                                    are considered sepparetaly in the critical elements for renewables
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("rareearthelements"), fixed("ree"))
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("rareearths"), fixed("ree"))
#   9a) zirconium →? zircon   ("Zircon is the primary source from which the metal zirconium is extracted")
#   DONT:     9b) chromium  →? chromite ("chromite is the primary source from which the metal chromium is extracted")
#  10) phosphaterock, phosphorus (P), phosphorous (P) → phosphate: fertilizers?
#  11) diamond, gemdiamond → Diamonds
yearlyPM25_wide_filtered$USGScommodity <- str_replace(yearlyPM25_wide_filtered$USGScommodity, 
                                             fixed("diamond"), fixed("diamonds"))
#  12) boron (B, not found in its pure state nature) → Borates (Borates are the commercially important, usable form of boron, typically appearing as mineral deposits like borax)


# NEW not critical commodities for renewable energies
#  13) halite should be included: it is salt for industrial consumption
#  14) gypsum: used to manufacture drywall (plasterboard), cement, and plaste
#  15) phosphaterock, phosphorus (P), phosphorous (P) : fertilizers


# Reglas de recodificacion para unclassified "faciles"
yearlyPM25_wide_filtered <- yearlyPM25_wide_filtered %>%
  mutate(
    USGScommodity = USGScommodity %>%
      # 1) Normalizaciones directas
      str_replace_all(fixed("halite(salt)"), "halite") %>%
      str_replace_all(fixed("phosphorous"), "phosphorus") %>%
      str_replace_all(fixed("phosphaterock"), "phosphate") %>%
      str_replace_all(fixed("clay(bentonite)"), "bentonite") %>%
      str_replace_all(fixed("clay(kaolin)"), "kaolin") %>%
      str_replace_all(fixed("kaolinite"), "kaolin") %>%
      #str_replace_all(fixed("fluorspar"), "fluorite") %>%
      str_replace_all(fixed("silicasand"), "silica") %>%
      str_replace_all(fixed("boron"), "borates") %>%
      str_replace_all(fixed("zirconium"), "zircon") %>%
      str_replace_all(fixed("gemdiamonds"), "diamonds") %>%
      str_replace_all(fixed("gem(emerald)"), "gemstones") %>%
      str_replace_all(fixed("gememerald"), "gemstones") %>%
      str_replace_all(fixed("gem(ruby"), "gemstones") %>%
      str_replace_all(fixed("sapphire)"), "gemstones") %>%
      str_replace_all(fixed("sapphire"), "gemstones") %>%
      str_replace_all(fixed("limestone(dolomite)"), "dolomite") %>%
      str_replace_all(fixed("dolomite)"), "dolomite")  %>%
      str_replace_all(fixed("limestone(marble"), "marble") %>%
      str_replace_all(fixed("marble)"), "marble") 
  )



#-------------------------#
# EDA for the commodities #
#-------------------------#

df <- yearlyPM25_wide_filtered %>%
  mutate(
    # split by '|'
    commodity_list = str_split(USGScommodity, "\\|"),
    commodity_list = map(commodity_list, ~ str_trim(.x)),
    # identify those with critical commodities & not critical commodities
    has_crit = map_lgl(commodity_list, ~ any(.x %in% commodities_crit4renew)),
    has_notcrit = map_lgl(commodity_list, ~ any(.x %in% commodities_notcrit4renew)),
    n_commodities = map_int(commodity_list, length),
    # conteo de cuántos críticos hay en la fila
    n_crit = map_int(commodity_list, ~ sum(.x %in% commodities_crit4renew)),
    n_notcrit = map_int(commodity_list, ~ sum(.x %in% commodities_notcrit4renew))
  )


## different sizes of commodities
df %>%
  count(n_commodities) %>%
  mutate(prop = round(n/sum(n) , 4) * 100) 

## How many different commodities

#df$commodity_list

## Proportion of places with critial, not critial & mixed commodities
df %>%
  summarise(
    share_has_crit = mean(has_crit),
    share_has_notcrit = mean(has_notcrit),
    share_mixed = mean(!has_crit & !has_notcrit)
  )

# Number of mines extracting CR, NCR or both types of commodities (out of all 38329 mining sites)
df %>%
  summarise(
    share_has_crit = sum(has_crit),
    share_has_notcrit = sum(has_notcrit),
    share_mixed = sum(has_crit & has_notcrit)
  )

# Proportion & number of mining sites extracting only unclassified commodities
mean(!df$has_crit & !df$has_notcrit)
sum( !df$has_crit & !df$has_notcrit)

# Proportions in the original paper:
# critical: 53186/(53186+10079) = 0.840686
# not critical: 0.159314


#################################################################
#################################################################
# Creating a dataset including information about CR, NCR and unclassified commdities within each mining site
# is_crit:          indicates whether it extracts CR commodities or not
# is_notcrit:       indicates whether it extracts NCR commodities or not
# is_unclassified:  indicates whether it extracts only unclassified commodities or not
df_long <- df %>%
  select(everything()) %>%
  unnest(commodity_list) %>% # make a copy of each place for each commodity included
  rename(commodity = commodity_list) %>%
  mutate(
    is_crit = commodity %in% commodities_crit4renew,
    is_notcrit = commodity %in% commodities_notcrit4renew,
    is_unclassified = !(is_crit | is_notcrit)
  )

# Proportion of mining sites extracting CR, NCR, mixed and unclassified commodities
df_long  %>%
  group_by(OBJECTID) %>%
  summarise(
    has_crit = any(is_crit),
    has_notcrit = any(is_notcrit),
    has_unclassified = any(is_unclassified),
    .groups = "drop"
  ) %>%
  summarise(
    share_has_crit = mean(has_crit),
    share_has_notcrit = mean(has_notcrit),
    share_mixed = mean(has_crit & has_notcrit),
    share_has_unclassified = mean(has_unclassified)
  )

# Total number of  of mining sites extracting CR, NCR, mixed and unclassified commodities
df_long  %>%
  group_by(OBJECTID) %>%
  summarise(
    has_crit = any(is_crit),
    has_notcrit = any(is_notcrit),
    has_unclassified = any(is_unclassified),
    .groups = "drop"
  ) %>%
  summarise(
    share_has_crit = sum(has_crit),
    share_has_notcrit = sum(has_notcrit),
    share_mixed = sum(has_crit & has_notcrit),
    share_has_unclassified = sum(has_unclassified)
  )

# Proportions in Sonter's paper:
# critical: 53186/(53186+10079) = 0.840686. (82%)
# not critical: 0.159314.                   (18%)


# top commodities (global)
df_long %>%
  count(commodity, sort = TRUE) %>%
  slice_head(n = 25)

# top critical
df_long %>%
  filter(is_crit) %>%
  count(commodity, sort = TRUE) %>% print(n = 27)%>% select(n) %>% sum()

# top not critical
df_long %>%
  filter(is_notcrit) %>%
  count(commodity, sort = TRUE)%>% print(n = 30) %>% select(n) %>% sum()

# no clasified (to decide for next steps)
df_long %>%
  filter(is_unclassified) %>%
  count(commodity, sort = TRUE) %>%
  slice_head(n = 50) %>% print(n = 60) %>% select(n) %>% sum()




#---------
# More EDA
#---------
library(dplyr)
library(tidyr)
library(purrr)

# 1) Co-ocurrencias commodity-commodity por mina
cooc_pairs <- df_long %>%
  distinct(OBJECTID, commodity) %>%
  group_by(OBJECTID) %>%
  summarise(
    pairs = list({
      x <- sort(unique(commodity))
      if (length(x) < 2) {
        tibble(c1 = character(), c2 = character())
      } else {
        as_tibble(t(combn(x, 2)), .name_repair = "minimal") %>%
          setNames(c("c1", "c2"))
      }
    }),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  count(c1, c2, sort = TRUE, name = "n_mines")

# Top 20 pares más frecuentes
cooc_pairs %>% slice_head(n = 20)


## Incomes and Countries:

# 0) Base por mina (evita duplicados por commodity)
site_flags <- df_long %>%
  group_by(OBJECTID, USGScountry) %>%
  summarise(
    has_crit = any(is_crit),
    has_notcrit = any(is_notcrit),
    has_unclassified = any(is_unclassified),
    n_commodities = n_distinct(commodity),
    .groups = "drop"
  ) %>%
  left_join(countryList %>% select(USGScountry, Income), by = "USGScountry")

site_flags <- site_flags %>%
  mutate(
    has_classified = has_crit | has_notcrit,
    only_unclassified = has_unclassified & !has_classified
  )

by_income <- site_flags %>%
  group_by(Income) %>%
  summarise(
    n_sites = n(),
    share_has_crit = mean(has_crit),
    share_has_notcrit = mean(has_notcrit),
    share_mixed = mean(has_crit & has_notcrit),
    share_unclassified = mean(only_unclassified),
    avg_n_commodities = mean(n_commodities),
    .groups = "drop"
  ) %>%
  arrange(desc(n_sites)); by_income

by_country <- site_flags %>%
  group_by(USGScountry, Income) %>%
  summarise(
    n_sites = n(),
    share_has_crit = mean(has_crit),
    share_has_notcrit = mean(has_notcrit),
    share_mixed = mean(has_crit & has_notcrit),
    share_has_unclassified = mean(has_unclassified),
    .groups = "drop"
  ) %>%
  arrange(desc(n_sites)); by_country

by_income %>%
  mutate(check_sum = share_has_crit + share_has_notcrit - share_mixed + share_unclassified)

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

plot_df <- by_income %>% filter(!is.na(Income)) %>%
  transmute(
    Income,
    `Only critical`      = share_has_crit - share_mixed,
    `Only not critical`  = share_has_notcrit - share_mixed,
    `Mixed`              = share_mixed,
    `Unclassified`  = share_unclassified
  ) %>%
  pivot_longer(-Income, names_to = "group", values_to = "share") %>%
  mutate(
    Income = factor(
      Income,
      levels = c("High income", "Upper middle income", "Lower middle income", "Low income", NA)
    ),
    # opcional: controla también orden de apilado/leyenda
    group = factor(group, levels = c("Only critical", "Mixed", "Only not critical", "Unclassified"))
  )

ggplot(plot_df, aes(x = Income, y = share, fill = group)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c(
    "Only critical" = "#1b9e77",
    "Mixed" = "#7570b3",
    "Only not critical" = "#d95f02",
    "Unclassified" = "#bdbdbd"
  )) +
  labs(
    x = NULL,
    y = "Proportion of mines",
    fill = NULL,
    title = "Commodity composition by Income group",
    subtitle = "(Disjoint categories)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

##NA?? Exclude Venezuela and Ethiopia
site_flags[is.na(site_flags$Income),] %>% group_by(USGScountry) %>% summarise(n())


#-----------------#
# By "Shape_Area" #
#-----------------#

# Analysis of proportion for CR, NCR, Mixed and Unclassified commodities based on the Mining Site size
site_flags_size <- df_long %>%
  group_by(OBJECTID) %>%
  summarise(
    USGScountry = first(USGScountry),
    Shape_Area = first(Shape_Area),   # constante por mina
    has_crit = any(is_crit),
    has_notcrit = any(is_notcrit),
    has_unclassified = any(is_unclassified),
    n_commodities = n_distinct(commodity),
    .groups = "drop"
  ) %>%
  mutate(
    mine_size = if_else(Shape_Area < 62000, "Small mines", "Large mines"),
    has_classified = has_crit | has_notcrit,
    only_unclassified = has_unclassified & !has_classified
  )

# Resumen por tamaño
by_size <- site_flags_size %>%
  group_by(mine_size) %>%
  summarise(
    n_sites = n(),
    share_has_crit = mean(has_crit),
    share_has_notcrit = mean(has_notcrit),
    share_mixed = mean(has_crit & has_notcrit),
    share_unclassified = mean(only_unclassified),
    avg_n_commodities = mean(n_commodities),
    .groups = "drop"
  )

by_size %>%
  mutate(check_sum = share_has_crit + share_has_notcrit - share_mixed + share_unclassified)

# Plot (igual que income, pero con mine_size)
plot_df_size <- by_size %>%
  transmute(
    mine_size,
    `Only critical` = share_has_crit - share_mixed,
    `Only not critical` = share_has_notcrit - share_mixed,
    `Mixed` = share_mixed,
    `Unclassified` = share_unclassified
  ) %>%
  pivot_longer(-mine_size, names_to = "group", values_to = "share") %>%
  mutate(
    mine_size = factor(mine_size, levels = c("Small mines", "Large mines")),
    group = factor(group, levels = c("Only critical", "Mixed", "Only not critical", "Unclassified"))
  )

ggplot(plot_df_size, aes(x = mine_size, y = share, fill = group)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = c(
    "Only critical" = "#1b9e77",
    "Mixed" = "#7570b3",
    "Only not critical" = "#d95f02",
    "Unclassified" = "#bdbdbd"
  )) +
  labs(
    x = NULL, y = "Proportion of mines", fill = NULL,
    title = "Commodity composition by mine size",
    subtitle = "(Disjoint categories)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

