library(tidyverse)
source(paste0(getwd(), "/src/etl/utils.R"))

comb_dat <- readRDS(paste0(getwd(), "/data/raw/combined_events_360.rds"))
match_info <- readRDS(paste0(getwd(), "/data/raw/match_info.rds"))

track_match_display_info <- vector("list", length = nrow(match_info))
for(i in 1:nrow(match_info)) {
  track_match_display_info[[i]] <- create_match_table(this_id = match_info$match_id[i])
}

track_match_display_info %>% bind_rows() %>% saveRDS(paste0(getwd(), "/data/processed/match_info_display_table.rds"))
