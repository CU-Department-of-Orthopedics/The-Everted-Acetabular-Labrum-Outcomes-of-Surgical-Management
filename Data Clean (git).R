### Data Clean 

# Trim to 2 years post-op 

library(readxl)
library(tidyverse)

# # Cohort Data 
# 

# 
# dat_el$group <- "Everted Labrum"
# 
# names(dat_el)
# 
# # Control Data 
# 
# dat_con <- read_excel("Data/evertedlabrum_carson.xlsx", sheet = "Control")
# 
# dat_con <- janitor::clean_names(dat_con)
# 
# dat_con <- dat_con %>% 
#   select(
#     bilateral, mrn, sex, age, pao,
#     contains("i_hot"), contains("non_arthritic")
#   )
# 
# dat_con$group <- "Control"
# 
# names(dat_con)
# 
# # Merge 
# common_columns <- intersect(names(dat_con), names(dat_el))
# 
# dat_con_subset <- dat_con[, common_columns]
# dat_el_subset <- dat_el[, common_columns]
# 
# dat_full <- rbind(dat_con_subset, dat_el_subset)
# 
# write.csv(dat_full, ..., row.names = F)

dat_full <- ...

## Separate and elongate 

dat_ihot <- dat_full %>% 
  select(
    mrn, group, contains("i_hot"), pao
  )


dat_ihot_long <- dat_ihot %>% 
  pivot_longer(
    cols = i_hot12_preop:i_hot12_24mpo,
    names_to = "time",
    values_to = "ihot_score"
  ) %>% 
  mutate(
    time_m = case_when(
      time == "i_hot12_10ypo" ~ 120, 
      time == "i_hot12_12mpo" ~ 12, 
      time == "i_hot12_18mpo" ~ 18, 
      time == "i_hot12_20ypo" ~ 240, 
      time == "i_hot12_24mpo" ~ 24, 
      time == "i_hot12_3mpo" ~ 3, 
      time == "i_hot12_5ypo" ~ 60, 
      time == "i_hot12_6mpo" ~ 6, 
      time == "i_hot12_6wpo" ~ 1.5, 
      time == "i_hot12_preop" ~ 0
    )
  ) %>% 
  filter(
    time_m <= 24
  )

dat_ihot_long_an <- dat_ihot_long %>% 
  filter(
    time_m > 0 
  )

dat_nahs <- dat_full %>% 
  select(
    mrn, group, contains("non_arthritic"), pao
  )

dat_nahs_long <- dat_nahs %>% 
  pivot_longer(
    cols = non_arthritic_hip_preop:non_arthritic_hip_24mpo,
    names_to = "time",
    values_to = "nahs_score"
  ) %>% 
  mutate(
    time_m = case_when(
      time == "non_arthritic_hip_10ypo" ~ 120, 
      time == "non_arthritic_hip_12mpo" ~ 12, 
      time == "non_arthritic_hip_18mpo" ~ 18, 
      time == "non_arthritic_hip_20ypo" ~ 240, 
      time == "non_arthritic_hip_24mpo" ~ 24, 
      time == "non_arthritic_hip_3mpo" ~ 3, 
      time == "non_arthritic_hip_5ypo" ~ 60, 
      time == "non_arthritic_hip_6mpo" ~ 6, 
      time == "non_arthritic_hip_6wpo" ~ 1.5, 
      time == "non_arthritic_hip_preop" ~ 0
    )
  )  %>% 
  filter(
    time_m <= 24
  ) 

dat_nahs_long_an <- dat_nahs_long %>% 
  filter(
    time_m > 0 
  )

# Add Demographic Table 

dat_id <- dat_full %>% 
  select(
    mrn, group
  )

keep_ids <- dat_id$mrn

# # Cohort Data

dat_el <- ...

dat_el <- janitor::clean_names(dat_el)

dat_el$group <- "Everted Labrum"

dat_el <- dat_el %>% 
  select(mrn, age_at_time_of_surgery, sex, group)

names(dat_el) <- c("mrn", "age", "sex", "group")

# Control Data

dat_con <- ...

dat_con <- janitor::clean_names(dat_con)

dat_con$group <- "Control"

dat_con <- dat_con %>% 
  select(mrn, age, sex, group)

dat_full_dem <- rbind(dat_el, dat_con)

dat_full_dem <- dat_full_dem %>% 
  filter(
    mrn %in% keep_ids
  )

dat_full_dem <- dat_full_dem %>% 
  distinct(mrn, .keep_all = T) %>% 
  mutate(
    sex = case_when(
      sex == "unknown" ~ NA,
      TRUE ~ sex
    )
  )


