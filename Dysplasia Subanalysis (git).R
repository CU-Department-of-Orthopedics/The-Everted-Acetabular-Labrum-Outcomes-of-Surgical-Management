### Dysplasia Sub-Analysis 2 

### Sub-analysis 
library(tidyverse)
library(readxl)
library(nlme)
library(lme4)
library(lmerTest)
library(emmeans)
library(kableExtra)

rm(list = ls())

source(".../covariance_structure_test.R")

sub_dat <- ...

sub_dat <- sub_dat %>% 
  mutate_at(vars(6:27), as.numeric) %>% 
  filter(
    sub_group != "No PAO or Dysplasia"
  )

## IHOT Selection 
sub_dat_ihot <- sub_dat %>% 
  select(
    mrn, contains("i_hot"), sub_group
  ) %>% 
  pivot_longer(
    i_hot12_preop:i_hot12_24mpo,
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
  labs(
    y = "iHOT-12 Score",
    x = "Months (Post-Op)",
    fill = ""
  )

sub_ihot_p_sum <- sub_dat_ihot %>% 
  group_by(
    sub_group, time_m
  ) %>% 
  summarize(
    mean = mean(ihot_score, na.rm = T),
    sd = sd(ihot_score, na.rm = T)
  ) %>% 
  arrange(
    time_m
  )

names(sub_ihot_p_sum) <- c("Group", "Time Post-Op", "Mean iHOT-12", "SD")

sub_ihot_p_sum <- sub_ihot_p_sum  %>% 
  kable(digits = 3) %>% 
  kable_classic(
    html_font = 'cambria',
    full_width = F
  )

sub_dat_ihot_an <- sub_dat_ihot %>% 
  filter(
    time_m > 0
  )


sub_dat_ihot_an$sub_group <- factor(sub_dat_ihot_an$sub_group, levels = c("Dysplasia+No PAO", "Dysplasia+PAO"))

## iHOT Subanalysis

#cov_var_fn(y = sub_dat_ihot_an$ihot_score, S = sub_dat_ihot_an$mrn, time = sub_dat_ihot_an$time_m, pred = sub_dat_ihot_an$sub_group, sort_by = "BIC", try_un = FALSE)

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
    non_arthritic_hip_preop:non_arthritic_hip_24mpo,
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
  labs(
    y = "NAHS Score",
    x = "Months (Post-Op)",
    fill = ""
  )

sub_nahs_p_sum <- sub_dat_nahs %>% 
  group_by(
    sub_group, time_m
  ) %>% 
  summarize(
    mean = mean(nahs_score, na.rm = T),
    sd = sd(nahs_score, na.rm = T)
  ) %>% 
  arrange(
    time_m
  )

names(sub_nahs_p_sum) <- c("Group", "Time Post-Op", "Mean NAHS", "SD")

sub_nahs_p_sum <- sub_nahs_p_sum  %>% 
  kable(digits = 3) %>% 
  kable_classic(
    html_font = 'cambria',
    full_width = F
  )

sub_dat_nahs_an <- sub_dat_nahs %>% 
  filter(
    time_m > 0
  )

sub_dat_nahs_an$sub_group <- factor(sub_dat_nahs_an$sub_group, levels = c("No PAO or Dysplasia", "Dysplasia+No PAO", "Dysplasia+PAO"))

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


### Pre-latest post-op analysis 

## iHOT 

sub_dat_ihot_t <- sub_dat %>% 
  select(mrn, sub_group, pao, i_hot12_preop, i_hot12_12mpo, i_hot12_24mpo) 

sub_last_ihot <- sub_dat_ihot_t %>%
  select(mrn, pao, starts_with("i_hot12")) %>%
  gather(key, value, -mrn) %>%
  group_by(mrn) %>%
  filter(!is.na(value)) %>%
  slice_tail(n = 1) %>%
  rename(last_followup_score = value)

sub_dat_ihot_t <- left_join(sub_dat_ihot_t, sub_last_ihot, by = "mrn")


sub_dat_ihot_t <- sub_dat_ihot_t %>% 
  select(
    mrn, sub_group, contains("pre"), contains("last")
  ) %>% 
  drop_na(
    
  ) %>% 
  pivot_longer(
    i_hot12_preop:last_followup_score,
    names_to = "Time",
    values_to = "ihot_score"
  )


sub_dat_ihot_t$Time <- factor(sub_dat_ihot_t$Time, levels = c("i_hot12_preop", "last_followup_score"))
sub_dat_ihot_t$Time <- factor(sub_dat_ihot_t$Time, labels = c("Pre-Op", "Last Follow-Up"))

sub_pre_post_ihot_p <- ggplot(
  data = sub_dat_ihot_t,
  aes(
    x = Time,
    y = ihot_score,
    fill = sub_group
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  labs(
    x = "",
    y = "iHOT-12 Score",
    fill = ""
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  )

sub_pre_post_ihot_p_sum <- sub_dat_ihot_t %>% 
  group_by(
    sub_group, Time
  ) %>% 
  summarize(
    mean = mean(ihot_score, na.rm = T),
    sd = sd(ihot_score, na.rm = T)
  ) %>% 
  arrange(
    Time
  )

names(sub_pre_post_ihot_p_sum) <- c("Group", "Time", "Mean iHOT-12", "SD")

sub_pre_post_ihot_p_sum <- sub_pre_post_ihot_p_sum  %>% 
  kable(digits = 3) %>% 
  kable_classic(
    html_font = 'cambria',
    full_width = F
  )


sub_ihot_pre_post_mod <- lmer(ihot_score ~ sub_group*Time + (1|mrn), data = sub_dat_ihot_t)

## Pre-op vs Post-op for all groups 
library(broom)

sub_ihot_posthoc_time <- emmeans(sub_ihot_pre_post_mod, ~ Time | sub_group)

sub_pre_v_post_ihot_cont <- tidy(contrast(sub_ihot_posthoc_time, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

sub_pre_v_post_ihot_cont_conf <- as.data.frame(round(tidy(confint(contrast(sub_ihot_posthoc_time, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
sub_pre_v_post_ihot_cont_conf <- paste0("(", sub_pre_v_post_ihot_cont_conf[, 1], ", ", sub_pre_v_post_ihot_cont_conf[, 2], ")")

sub_pre_v_post_ihot_cont <- cbind(sub_pre_v_post_ihot_cont, sub_pre_v_post_ihot_cont_conf)

names(sub_pre_v_post_ihot_cont) <- c("Group", "Time Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

sub_pre_v_post_ihot_cont <- sub_pre_v_post_ihot_cont %>%
  mutate_if(
    is.numeric, round, digits = 2
  )  %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)



# Post hoc tests for between-group differences at each time point
sub_ihot_posthoc_group <- emmeans(sub_ihot_pre_post_mod, ~ sub_group | Time)

sub_ihot_cont_conf <- as.data.frame(round(tidy(confint(contrast(sub_ihot_posthoc_group, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
sub_ihot_cont_conf <- paste0("(", sub_ihot_cont_conf[, 1], ", ", sub_ihot_cont_conf[, 2], ")")

sub_ihot_cont_sum <- tidy(contrast(sub_ihot_posthoc_group, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

sub_ihot_cont_sum <- cbind(sub_ihot_cont_sum, sub_ihot_cont_conf)

names(sub_ihot_cont_sum) <- c("Time", "Group Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

sub_ihot_cont_sum <- sub_ihot_cont_sum %>%
  mutate_if(
    is.numeric, round, digits = 2
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  mutate_if(
    is.numeric, round, digits = 2
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)


## NAHS 

sub_dat_nahs_t <- sub_dat %>% 
  select(mrn, sub_group, pao, non_arthritic_hip_preop, non_arthritic_hip_12mpo, non_arthritic_hip_24mpo) 

sub_last_nahs <- sub_dat_nahs_t %>%
  select(mrn, pao, starts_with("non_")) %>%
  gather(key, value, -mrn) %>%
  group_by(mrn) %>%
  filter(!is.na(value)) %>%
  slice_tail(n = 1) %>%
  rename(last_followup_score = value)

sub_dat_nahs_t <- left_join(sub_dat_nahs_t, sub_last_nahs, by = "mrn")

sub_dat_nahs_t <- sub_dat_nahs_t %>% 
  select(
    mrn, sub_group, contains("pre"), contains("last")
  ) %>% 
  drop_na(
    
  ) %>% 
  pivot_longer(
    non_arthritic_hip_preop:last_followup_score,
    names_to = "Time",
    values_to = "nahs_score"
  )


sub_dat_nahs_t$Time <- factor(sub_dat_nahs_t$Time, levels = c("non_arthritic_hip_preop", "last_followup_score"))
sub_dat_nahs_t$Time <- factor(sub_dat_nahs_t$Time, labels = c("Pre-Op", "Last Follow-Up"))

sub_pre_post_nahs_p <- ggplot(
  data = sub_dat_nahs_t,
  aes(
    x = Time,
    y = nahs_score,
    fill = sub_group
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  labs(
    x = "",
    y = "NAHS Score",
    fill = ""
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  )

sub_pre_post_nahs_p_sum <- sub_dat_nahs_t %>% 
  group_by(
    sub_group, Time
  ) %>% 
  summarize(
    mean = mean(nahs_score, na.rm = T),
    sd = sd(nahs_score, na.rm = T)
  ) %>% 
  arrange(
    Time
  )

names(sub_pre_post_nahs_p_sum) <- c("Group", "Time", "Mean NAHS", "SD")

sub_pre_post_nahs_p_sum <- sub_pre_post_nahs_p_sum  %>% 
  kable(digits = 3) %>% 
  kable_classic(
    html_font = 'cambria',
    full_width = F
  )


sub_nahs_pre_post_mod <- lmer(nahs_score ~ sub_group*Time + (1|mrn), data = sub_dat_nahs_t)

## Pre-op vs Post-op for all groups 

sub_nahs_posthoc_time <- emmeans(sub_nahs_pre_post_mod, ~ Time | sub_group)

sub_pre_v_post_nahs_cont <- tidy(contrast(sub_nahs_posthoc_time, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

sub_pre_v_post_nahs_cont_conf <- as.data.frame(round(tidy(confint(contrast(sub_nahs_posthoc_time, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
sub_pre_v_post_nahs_cont_conf <- paste0("(", sub_pre_v_post_nahs_cont_conf[, 1], ", ", sub_pre_v_post_nahs_cont_conf[, 2], ")")

sub_pre_v_post_nahs_cont <- cbind(sub_pre_v_post_nahs_cont, sub_pre_v_post_nahs_cont_conf)

names(sub_pre_v_post_nahs_cont) <- c("Group", "Time Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

sub_pre_v_post_nahs_cont <- sub_pre_v_post_nahs_cont %>%
  mutate_if(
    is.numeric, round, digits = 2
  )  %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)



# Post hoc tests for between-group differences at each time point
sub_nahs_posthoc_group <- emmeans(sub_nahs_pre_post_mod, ~ sub_group | Time)

sub_nahs_cont_conf <- as.data.frame(round(tidy(confint(contrast(sub_nahs_posthoc_group, method = "pairwise", adjust = "sidak")), null.value = 0)[7:8], digits = 2))
sub_nahs_cont_conf <- paste0("(", sub_nahs_cont_conf[, 1], ", ", sub_nahs_cont_conf[, 2], ")")

sub_nahs_cont_sum <- tidy(contrast(sub_nahs_posthoc_group, method = "pairwise", adjust = "sidak"))[c(1, 3, 5,  6, 8:9)]

sub_nahs_cont_sum <- cbind(sub_nahs_cont_sum, sub_nahs_cont_conf)

names(sub_nahs_cont_sum) <- c("Time", "Group Contrast", "Mean Difference", "Std. Error", "t-ratio", "p-value", "95% CI")

sub_nahs_cont_sum <- sub_nahs_cont_sum %>%
  mutate_if(
    is.numeric, round, digits = 2
  ) %>% 
  mutate(
    `p-value` = case_when(
      `p-value` <= 0.001 ~ "p < 0.001",
      TRUE ~ as.character(`p-value`)
    )
  ) %>% 
  mutate_if(
    is.numeric, round, digits = 2
  ) %>% 
  kable() %>% kable_classic(html_font = 'cambria', full_width = F)

