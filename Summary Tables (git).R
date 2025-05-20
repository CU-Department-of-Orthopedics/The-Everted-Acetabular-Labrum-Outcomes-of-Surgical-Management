### Summary Stats 

# Sum Fun

{
  ## T-test and Chi-square Table Fn 
  
  library(table1)
  
  render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
         c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
  }
  
  render.cat <- function(x) {
    c("", 
      sapply(stats.default(x), 
             function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
  }
  
  
  pvalue <- function(x, ...) {
    y <- unlist(x)
    g <- factor(rep(1:length(x), times = sapply(x, length)))
    if (is.numeric(y)) {
      p <- t.test(y ~ g)$p.value
    } else {
      p <- chisq.test(table(y, g))$p.value
    }
    c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
  }
  

}

label(dat_full_dem$age) <- "Age (yrs)"
label(dat_full_dem$sex) <- "Sex"

tab1 <- table1(
  ~ age + sex | group,
  data = dat_full_dem,
  overall = F,
  extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont,
  render.categorical = render.cat
)

table(dat_full_dem$mrn)

# ## Only EL 
# dat_el <- ...
# 
# dat_el <- janitor::clean_names(dat_el)
# 
# dat_el <- dat_el %>%
#   select(
#     bilateral, mrn, sex, age, race, bmi, pao,
#     beightons, cyst_grafting:medial_joint_space_mm, contains("i_hot"), contains("non_arthritic")
#   )
# dat_el_sum <- dat_el
# 
# label(dat_el_sum$age) <- "Age"
# label(dat_el_sum$bmi) <- "BMI"
# dat_el_sum$beightons <- as.factor(dat_el_sum$beightons)
# label(dat_el_sum$beightons) <- "Beightons Score"
# label(dat_el_sum$torsion) <- "Torsion"
# label(dat_el_sum$eq) <- "EQ"
# label(dat_el_sum$lce) <- "LCE"
# label(dat_el_sum$sourcil) <- "Sourcil"
# label(dat_el_sum$alpha) <- "Alpha Angle"
# label(dat_el_sum$wbz_joint_space_mm) <- "WBZ Joint Space (mm)"
# label(dat_el_sum$medial_joint_space_mm) <- "Medial Joint Space (mm)"
# label(dat_el_sum$sex) <- "Sex"
# 
# tab1 <- table1(
#   ~ age + sex + bmi + beightons + 
#     torsion + eq + lce + sourcil + alpha + 
#     wbz_joint_space_mm + medial_joint_space_mm,
#   data = dat_el_sum,
#   # overall = F,
#   # extra.col=list(`P-value`= pvalue),
#   render.continuous = render.cont,
#   render.categorical = render.cat,
#   render.missing = NULL
# )
