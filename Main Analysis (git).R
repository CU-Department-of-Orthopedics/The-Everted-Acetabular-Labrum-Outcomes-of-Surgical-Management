### Initial Analysis 

library(nlme)
library(kableExtra)
library(emmeans)

source(".../covariance_structure_test.R")

### I-HOT12 Analysis 
{
  ihot_p <- ggplot(
    data = dat_ihot_long,
    aes(
      x = as.factor(time_m),
      y = ihot_score,
      fill = group
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
      legend.position = 'bottom'
    ) + 
    labs(
      x = "Months Post-op",
      y = "iHOT-12 Score",
      fill = ""
    )
  ihot_p
  
  ihot_p_sum <- dat_ihot_long %>% 
    group_by(group, time_m) %>% 
    summarize(
      mean = mean(ihot_score, na.rm = T), 
      sd = sd(ihot_score, na.rm = T)
    ) %>% 
    arrange(
      time_m
    )
  
  names(ihot_p_sum) <- c("Group", "Time Post-Op", "Mean iHOT-12", "SD")
  
  ihot_p_sum <- ihot_p_sum  %>% 
    kable(digits = 3) %>% 
    kable_classic(
      html_font = 'cambria',
      full_width = F
    )
}


# cov_var_fn(y = dat_ihot_long_an$ihot_score, S = dat_ihot_long_an$mrn, time = dat_ihot_long_an$time_m, pred = dat_ihot_long_an$group, sort_by = "BIC", try_un = FALSE)

# mod_ihot1 <- gls(ihot_score ~ group + time_m, weights = varIdent(form = ~1|time_m), correlation = corAR1(form = ~1|mrn), data = dat_ihot_long_an, method = "REML", na.action = na.omit)    
# summary(mod_ihot1)

mod_ihot_int <- gls(ihot_score ~ group*as.factor(time_m), weights = varIdent(form = ~1|time_m), correlation = corAR1(form = ~1|mrn), data = dat_ihot_long_an, method = "REML", na.action = na.omit)    
summary(mod_ihot_int)

ihot_sum <- data.frame(anova(mod_ihot_int))

names(ihot_sum) <- c("df", "F-value", "p-value")

rownames(ihot_sum) <- c(
  "(Intercept)",
  "Everted Labrum (1) vs Control (0)",
  "Time",
  "Interaction"
)

ihot_sum <- ihot_sum %>% 
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

emm_ihot1 <- emmeans(mod_ihot_int, ~group|time_m, mode = "appx-satterthwaite")

ihot_cont_table1 <- as.data.frame(summary(contrast(emm_ihot1, method = "pairwise", adjust = "Tukey")))

ihot_conf_table1 <- round((confint(contrast(emm_ihot1, method = "pairwise", adjust = "Tukey")))[, 6:7], 2)
ihot_conf_table1 <- paste0("(", ihot_conf_table1[, 1], ", ", ihot_conf_table1[, 2], ")")

ihot_cont_table1 <- cbind(ihot_cont_table1, ihot_conf_table1)

names(ihot_cont_table1) <- c("Contrast", "Months Post-Op", "Diff. Mean iHOT-12 Score", "Std. Err.", "df", "t-ratio", "p-value", "95% CI") 

ihot_cont_table1 <- ihot_cont_table1 %>% 
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



### NAHS 
{
nahs_p <- ggplot(
  data = dat_nahs_long,
  aes(
    x = as.factor(time_m),
    y = nahs_score,
    fill = group
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
    legend.position = 'bottom'
  ) + 
  labs(
    x = "Months Post-op",
    y = "NAHS Score",
    fill = ""
  )
  
  nahs_p_sum <- dat_nahs_long %>% 
    group_by(group, time_m) %>% 
    summarize(
      mean = mean(nahs_score, na.rm = T), 
      sd = sd(nahs_score, na.rm = T)
    ) %>% 
    arrange(
      time_m
    )
  
  names(nahs_p_sum) <- c("Group", "Time Post-Op", "Mean NAHS", "SD")
  
  nahs_p_sum <- nahs_p_sum  %>% 
    kable(digits = 3) %>% 
    kable_classic(
      html_font = 'cambria',
      full_width = F
    )
}

# cov_var_fn(y = dat_nahs_long_an$nahs_score, S = dat_nahs_long_an$mrn, time = dat_nahs_long_an$time_m, pred = dat_nahs_long_an$group, sort_by = "BIC", try_un = FALSE)

# mod_nahs1 <- gls(nahs_score ~ group + time_m, weights = varIdent(form = ~1|time_m), correlation = corAR1(form = ~1|mrn), data = dat_nahs_long_an, method = "REML", na.action = na.omit)    
# summary(mod_nahs1)

mod_nahs_int <- gls(nahs_score ~ group*as.factor(time_m), weights = varIdent(form = ~1|time_m), correlation = corAR1(form = ~1|mrn), data = dat_nahs_long_an, method = "REML", na.action = na.omit)    
summary(mod_nahs_int)

nahs_sum <- data.frame(anova(mod_nahs_int))

names(nahs_sum) <- c("df", "F-value", "p-value")

rownames(nahs_sum) <- c(
  "(Intercept)",
  "Everted Labrum (1) vs Control (0)",
  "Time",
  "Interaction"
)

nahs_sum <- nahs_sum %>% 
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

emm_nahs1 <- emmeans(mod_nahs_int, ~group|time_m, mode = "appx-satterthwaite")

nahs_cont_table1 <- as.data.frame(summary(contrast(emm_nahs1, method = "pairwise", adjust = "Tukey")))

nahs_conf_table1 <- round((confint(contrast(emm_nahs1, method = "pairwise", adjust = "Tukey")))[, 6:7], 2)
nahs_conf_table1 <- paste0("(", nahs_conf_table1[, 1], ", ", nahs_conf_table1[, 2], ")")

nahs_cont_table1 <- cbind(nahs_cont_table1, nahs_conf_table1)

names(nahs_cont_table1) <- c("Contrast", "Months Post-Op", "Diff. Mean NAHS Score", "Std. Err.", "df", "t-ratio", "p-value", "95% CI") 

nahs_cont_table1 <- nahs_cont_table1 %>% 
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

