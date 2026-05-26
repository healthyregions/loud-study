library(sf)
library(dplyr)
library(readr)
library(gtsummary)
library(stringr)
library(gt)

geo <- st_read("loud-cleaned.geojson")
df <- geo %>% st_drop_geometry()

dict <- read_csv("DataDictionary.csv", show_col_types = FALSE)

# data-driven raw measures
raw_dict <- dict %>%
  rename(variable = `Variable Name`) %>%
  filter(ConceptualIndex_Raw == 1) %>%
  filter(variable %in% names(df)) %>%
  filter(!str_detect(variable, "Sc$")) %>%          # not scaled
  filter(!str_detect(variable, "PPL$")) %>%         # not percentile ranks
  filter(!str_detect(variable, "Cat$")) %>%         # not category variables
  filter(!str_detect(variable, "^PC[0-9]+f$")) %>% # not PCA scores as raw rows
  filter(!str_detect(variable, "^Stage")) %>%
  filter(!str_detect(variable, "^AccessScore"))

raw_vars <- raw_dict$variable

# PC score variables
pc_vars <- paste0("PC", 1:9, "f")
pc_vars <- pc_vars[pc_vars %in% names(df)]

# Helper function to make the table
make_tbl <- function(vars) {
  if (length(vars) == 0) return(NULL)
  
  df %>%
    select(any_of(vars)) %>%
    tbl_summary(
      statistic = all_continuous() ~ "{mean} ({sd}); {median} [{p25}, {p75}]",
      digits = all_continuous() ~ 2,
      missing = "no"
    )
}

tbl_all <- make_tbl(raw_vars)

pc_tbls <- lapply(pc_vars, make_tbl)

tbl_list <- c(list(tbl_all), pc_tbls)
spanner_list <- c("**All**", paste0("**", pc_vars, "**"))

keep <- !sapply(tbl_list, is.null)

data_driven_tbl <- tbl_merge(
  tbls = tbl_list[keep],
  tab_spanner = spanner_list[keep]
) %>%
  modify_caption("**Summary statistics for initial draft of data-driven index**")

data_driven_tbl

data_driven_tbl %>%
  as_gt() %>%
  gtsave("data_driven_index_summary.html")