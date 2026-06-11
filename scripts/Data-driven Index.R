# Sam Wang's Updates - Spring 2026
# Data-driven index: summary statistics by PC quantile categories

library(gt)
library(sf)
library(dplyr)
library(readr)
library(gtsummary)
library(stringr)

geo <- st_read("../data_final/loud-cleaned.geojson")
df <- geo %>% st_drop_geometry()

dict <- read_csv("../data_final/DataDictionary.csv", show_col_types = FALSE)

raw_vars <- c(
  "EngProf",
  "CompHhldsP",
  "BbndInternetP",
  "LngTermP",
  "LibPer10k",
  "RlgPer10k",
  "RspRt",
  "abst2",
  "ssp2",
  "pastMethd10",
  "DisbP",
  "NoVehHHld",
  "CommTransit",
  "CommWalking",
  "MetTmDr2",
  "BupTmDr2",
  "NaltTmDr2",
  "BupPolRst",
  "OdMortRtAv",
  "PovP",
  "PrivateInsP",
  "MedPolProp",
  "supportive",
  "HistRstMMTOrd"
)

raw_vars <- raw_vars[raw_vars %in% names(df)]

#Define PC quantile category variables

pc_cat_vars <- paste0("PC", 1:9, "fCat")
pc_cat_vars <- pc_cat_vars[pc_cat_vars %in% names(df)]

#Function to make one table for one PC

make_pc_quantile_tbl <- function(pc_cat_var) {
  
  pc_name <- str_remove(pc_cat_var, "Cat$")
  
  df_tmp <- df %>%
    select(all_of(c(raw_vars, pc_cat_var))) %>%
    mutate(
      pc_quantile = factor(
        .data[[pc_cat_var]],
        levels = c(1, 2, 3, 4),
        labels = c("Quantile 1", "Quantile 2", "Quantile 3", "Quantile 4")
      )
    )
  
  df_tmp %>%
    tbl_summary(
      by = pc_quantile,
      include = all_of(raw_vars),
      statistic = all_continuous() ~ "{mean} ({sd}); {median} [{p25}, {p75}]; min={min}, max={max}",
      digits = all_continuous() ~ 2,
      missing = "no"
    ) %>%
    modify_caption(
      paste0("**Summary statistics: ", pc_name, " quantiles**")
    ) %>%
    modify_header(label = "**Raw measure**")
}


pc_tables <- lapply(pc_cat_vars, make_pc_quantile_tbl)
names(pc_tables) <- str_remove(pc_cat_vars, "Cat$")


pc_tables[["PC1f"]]
pc_tables[["PC2f"]]
pc_tables[["PC3f"]]
pc_tables[["PC4f"]]
pc_tables[["PC5f"]]
pc_tables[["PC6f"]]
pc_tables[["PC7f"]]
pc_tables[["PC8f"]]
pc_tables[["PC9f"]]
