rm(list = ls())
library(dplyr); library(ggplot2)
library(stringr); library(purrr); library(tidyr)

#setwd("/Users/bernabecanopaez/Desktop/Winter 2026/STA 260/Final Project")
#countryList <- readRDS("./data/12-3-25WBcountries.rds")
#yearlyPM25_wide <- readRDS("./CleanData/yearlyPM25_wide.rds"); length(unique(yearlyPM25_wide$OBJECTID))
#yearlyPM25 <- readRDS("./CleanData/yearlyPM25.rds"); length(unique(yearlyPM25$OBJECTID))

#-----------------------------------
# Filtering the 35381 selected mines
#-----------------------------------
#filtered_mines = (unique(yearlyPM25_wide$OBJECTID) %in% unique(yearlyPM25$OBJECTID))
#yearlyPM25_wide_filtered = yearlyPM25_wide[filtered_mines,]

#-----------------------------------
# Without filtering the 35381 selected mines
#-----------------------------------
yearlyPM25_wide_filtered <- readRDS("./data/12-1-25tang2023_PM.rds")

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



#------------------------
# EDA for the commodities
#------------------------
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

#----------------------
# Exporting the dataset
#----------------------
saveRDS(df, "./data/yearlyPM25_commofities_wide.rds")
