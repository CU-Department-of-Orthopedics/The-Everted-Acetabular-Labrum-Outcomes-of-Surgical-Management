### Sub-analysis 

library(nlme)
library(emmeans)

rm(list = ls())

source(".../covariance_structure_test.R")

## Sub-analysis data 
# sub_dat <- ...
# sub_dat_control <- ...
# 
# sub_dat <- janitor::clean_names(sub_dat)
# sub_dat_control <- janitor::clean_names(sub_dat_control)
# 
# sub_dat <- sub_dat %>% 
#   select(
#     bilateral, mrn, sex, age_at_time_of_surgery, race, bmi, pao,
#     beightons, cyst_grafting:medial_joint_space_mm, contains("i_hot"), contains("non_arthritic")
#   )
# 
# sub_dat$main_group = "Everted Labrum"
# 
# sub_dat_control <- sub_dat_control %>% 
#   select(
#     bilateral, mrn, sex, age, pao,
#     contains("i_hot"), contains("non_arthritic")
#   )
# 
# sub_dat_control$main_group = "Control"
# 
# ## Sub-group creation 
# # 1.	Everted labrum+PAO= cohort tab with a 1 under PAO
# # 2.	Everted Labrum with Dysplasia and no PAO: These are patients in the cohort tab that had an everted labrum but had a 0 under PAO AND had a measurement of LCE less than 25 degrees. Sorry for that confusion; there is no way you would have known that. I should've been more clear. 
# # 3.	No Dysplasia with everted labrum repair= Cohort tab, with 0 for PAO and LCE greater than or equal to 25 degrees. 
# 
# sub_dat <- sub_dat %>% 
#   drop_na(pao) %>% 
#   mutate(
#     sub_group = case_when(
#       pao == 1 ~ "EL + PAO",
#       pao == 0 & lce < 25 ~ "EL + Dysplasia + No PAO",
#       pao == 0 & lce >= 25 ~ "EL + No Dysplasia + No PAO"
#     )
#   )
# 
# sub_dat_control <- sub_dat_control %>% 
#   drop_na(pao) %>% 
#   mutate(
#     sub_group = case_when(
#       pao == 1 ~ "Control + PAO",
#       pao == 0  ~ "Control + No PAO"
#       )
#   )
# 
# # Merge 
# common_columns_sub <- intersect(names(sub_dat), names(sub_dat_control))
# 
# sub_dat_subset <- sub_dat[, common_columns_sub]
# sub_dat_control_subset <- sub_dat_control[, common_columns_sub]
# 
# full_sub_dat <- rbind(sub_dat_subset, sub_dat_control_subset)
# 
# write.csv(full_sub_dat, ..., row.names = F)

sub_dat <- ...

## IHOT Selection 
sub_dat_ihot <- sub_dat %>% 
  select(
    mrn, contains("i_hot"), sub_group
  ) %>% 
  pivot_longer(
    i_hot12_preop:i_hot12_20ypo,
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

## iHOT Sub-plot 

sub_ihot_p <- ggplot(
  data = sub_dat_ihot,
  aes(
    x = as.factor(time_m),
    y = ihot_score,
    fill = sub_group
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
    ) + 
  labs(
    y = "iHOT-12 Score",
    x = "Months (Post-Op)",
    fill = ""
  )

sub_dat_ihot_an <- sub_dat_ihot %>% 
  filter(
    time_m > 0
    )

sub_dat_ihot_an$sub_group <- factor(sub_dat_ihot_an$sub_group, levels = c("No Dysplasia (No PAO)", "Dysplasia (No PAO)", "PAO"))

## iHOT Subanalysis
# cov_var_fn(y = sub_dat_ihot_an$ihot_score, S = sub_dat_ihot_an$mrn, time = sub_dat_ihot_an$time_m, pred = sub_dat_ihot_an$sub_group, sort_by = "BIC", try_un = FALSE)

sub_ihot_int <- gls(ihot_score ~ sub_group*as.factor(time_m), weights = varIdent(form = ~1), correlation = corAR1(form = ~1|mrn), data = sub_dat_ihot_an, method = "REML", na.action = na.omit)    
summary(sub_ihot_int)

sub_ihot_sum <- data.frame(anova(sub_ihot_int))

names(sub_ihot_sum) <- c("df", "F-value", "p-value")

rownames(sub_ihot_sum) <- c(
  "(Intercept)",
  "Subgroup Effect",
  "Time Effect",
  "Interaction"
)

sub_ihot_sum <- sub_ihot_sum %>% 
  mutate_if(
    is.numeric, round, digits = 3
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable(
    
  ) %>% 
  kable_classic(html_font = "cambria", full_width = F)

sub_emm_ihot1 <- emmeans(sub_ihot_int, ~sub_group|time_m, mode = "appx-satterthwaite")

sub_ihot_cont_table1 <- as.data.frame(summary(contrast(sub_emm_ihot1, method = "pairwise", adjust = "Tukey")))

sub_ihot_conf_table1 <- round((confint(contrast(sub_emm_ihot1, method = "pairwise", adjust = "Tukey")))[, 6:7], 2)
sub_ihot_conf_table1 <- paste0("(", sub_ihot_conf_table1[, 1], ", ", sub_ihot_conf_table1[, 2], ")")

sub_ihot_cont_table1 <- cbind(sub_ihot_cont_table1, sub_ihot_conf_table1)

names(sub_ihot_cont_table1) <- c("Contrast", "Months Post-Op", "Diff. Mean iHOT-12 Score", "Std. Err.", "df", "t-ratio", "p-value", "95% CI") 

sub_ihot_cont_table1 <- sub_ihot_cont_table1 %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  select(
    -c("df")
  ) %>% 
  kable() %>% 
  kable_classic(html_font = 'cambria', full_width = F)






### NAHS Selection 
sub_dat_nahs <- sub_dat %>% 
  select(
    mrn, contains("non_"), sub_group
  ) %>% 
  pivot_longer(
    non_arthritic_hip_preop:non_arthritic_hip_20ypo,
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
  ) %>% 
  filter(
    time_m <= 24
  )

## NAHS Sub-plot 
sub_nahs_p <- ggplot(
  data = sub_dat_nahs,
  aes(
    x = as.factor(time_m),
    y = nahs_score,
    fill = sub_group
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  ) + 
  labs(
    y = "NAHS Score",
    x = "Months (Post-Op)",
    fill = ""
  )

sub_dat_nahs_an <- sub_dat_nahs %>% 
  filter(
    time_m > 0
  )

sub_dat_nahs_an$sub_group <- factor(sub_dat_nahs_an$sub_group, levels = c("No Dysplasia (No PAO)", "Dysplasia (No PAO)", "PAO"))

## NAHS Subanalysis
# cov_var_fn(y = sub_dat_nahs_an$nahs_score, S = sub_dat_nahs_an$mrn, time = sub_dat_nahs_an$time_m, pred = sub_dat_nahs_an$sub_group, sort_by = "BIC", try_un = FALSE)

sub_nahs_int <- gls(nahs_score ~ sub_group*as.factor(time_m), weights = varIdent(form = ~1), correlation = corAR1(form = ~1|mrn), data = sub_dat_nahs_an, method = "REML", na.action = na.omit)    
summary(sub_nahs_int)

sub_nahs_sum <- data.frame(anova(sub_nahs_int))

names(sub_nahs_sum) <- c("df", "F-value", "p-value")

rownames(sub_nahs_sum) <- c(
  "(Intercept)",
  "Subgroup Effect",
  "Time Effect",
  "Interaction"
)

sub_nahs_sum <- sub_nahs_sum %>% 
  mutate_if(
    is.numeric, round, digits = 3
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable(
    
  ) %>% 
  kable_classic(html_font = "cambria", full_width = F)

sub_emm_nahs1 <- emmeans(sub_nahs_int, ~sub_group|time_m, mode = "appx-satterthwaite")

sub_nahs_cont_table1 <- as.data.frame(summary(contrast(sub_emm_nahs1, method = "pairwise", adjust = "Tukey")))

sub_nahs_conf_table1 <- round((confint(contrast(sub_emm_nahs1, method = "pairwise", adjust = "Tukey")))[, 6:7], 2)
sub_nahs_conf_table1 <- paste0("(", sub_nahs_conf_table1[, 1], ", ", sub_nahs_conf_table1[, 2], ")")

sub_nahs_cont_table1 <- cbind(sub_nahs_cont_table1, sub_nahs_conf_table1)

names(sub_nahs_cont_table1) <- c("Contrast", "Months Post-Op", "Diff. Mean NAHS Score", "Std. Err.", "df", "t-ratio", "p-value", "95% CI") 

sub_nahs_cont_table1 <- sub_nahs_cont_table1 %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` < 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  select(
    -c("df")
  ) %>% 
  kable() %>% 
  kable_classic(html_font = 'cambria', full_width = F)



