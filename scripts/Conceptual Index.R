library(gt)
library(sf)
library(dplyr)
library(readr)
library(gtsummary)
library(stringr)

geo <- st_read("loud-cleaned.geojson")
df <- geo %>% st_drop_geometry()

dict <- read_csv("DataDictionary.csv", show_col_types = FALSE)

raw_dict <- dict %>%
  rename(variable = `Variable Name`) %>%
  filter(ConceptualIndex_Raw == 1) %>%
  filter(variable %in% names(df)) %>%
  filter(!str_detect(variable, "Sc$")) %>%
  filter(!str_detect(variable, "PPL$")) %>%
  filter(!str_detect(variable, "Cat$")) %>%
  filter(!str_detect(variable, "^PC[0-9]+f$")) %>%
  filter(!str_detect(variable, "^Stage")) %>%
  filter(!str_detect(variable, "^AccessScore"))

raw_vars <- raw_dict$variable

raw_dict %>% count(Stage)

make_tbl <- function(vars) {
  if (length(vars) == 0) {
    return(NULL)
  }
  
  df %>%
    select(any_of(vars)) %>%
    tbl_summary(
      statistic = all_continuous() ~ "{mean} ({sd}); {median} [{p25}, {p75}]; min={min}, max={max}",
      missing = "no"
    )
}

tbl_all <- make_tbl(raw_vars)

stage1_vars <- raw_dict %>% filter(Stage == 1) %>% pull(variable)
stage2_vars <- raw_dict %>% filter(Stage == 2) %>% pull(variable)
stage3_vars <- raw_dict %>% filter(Stage == 3) %>% pull(variable)
stage4_vars <- raw_dict %>% filter(Stage == 4) %>% pull(variable)
stage5_vars <- raw_dict %>% filter(Stage == 5) %>% pull(variable)

tbl_stage1 <- make_tbl(stage1_vars)
tbl_stage2 <- make_tbl(stage2_vars)
tbl_stage3 <- make_tbl(stage3_vars)
tbl_stage4 <- make_tbl(stage4_vars)
tbl_stage5 <- make_tbl(stage5_vars)

tbl_access <- make_tbl("AccessScore")

# 5. Keep only non-empty tables
tbl_list <- list(
  tbl_all,
  tbl_stage1,
  tbl_stage2,
  tbl_stage3,
  tbl_stage4,
  tbl_stage5,
  tbl_access
)

spanner_list <- c(
  "**All**",
  "**Stage 1**",
  "**Stage 2**",
  "**Stage 3**",
  "**Stage 4**",
  "**Stage 5**",
  "**Access Score**"
)

keep <- !sapply(tbl_list, is.null)

final_tbl <- tbl_merge(
  tbls = tbl_list[keep],
  tab_spanner = spanner_list[keep]
) %>%
  modify_caption("**Summary statistics for initial draft of conceptual index**")

final_tbl

final_tbl %>%
  as_gt() %>%
  gtsave("conceptual_index_summary.html")
