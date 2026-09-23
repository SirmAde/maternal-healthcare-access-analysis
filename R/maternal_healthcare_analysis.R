#  Maternal Healthcare Access and Child Mortality in Nigeria
#  University of Évora
#  Demography - Master's in Statistical Modelling and Data Analysis
#  Student: Samuel Adeosun (m66594)
#  Data Source: Approved by World Demographic Health Survey; For Nigeria DHS Birth Record (BR) File – NGBR8BFL.DTA
#
#######################################
#  1.      HYPOTHESIS
#######################################

#  H0: Adequate maternal ANC has no effect on under-five child mortality in Nigeria.
#  H1: Adequate maternal ANC reduces under-five child mortality in Nigeria.

#  MAIN VARIABLE: m14 (Antenatal Care visits)

#  CONTROL VARIABLES: place of delivery, mother's education,cwealth index, residence type, mother's age


rm(list = ls(all = TRUE))

library(haven)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(Hmisc)

setwd("~/Documents/Nigeria Dataset/Birthcode")

data <- read_dta("NGBR8BFL.DTA")
data <- as.data.frame(data)

# Selected key variables for my project analysis
small_data <- data %>%
  select(
    b5,    # Child survival status (1 = Alive, 0 = Dead)
    b7,    # Age at death in months (imputed)
    m14,   # ANC visits – MAIN HYPOTHESIS VARIABLE
    m15,   # Place of delivery – control variable
    v106,  # Mother's education – control variable
    v190,  # Wealth index – control variable
    v025,  # Residence type – control variable
    v012   # Mother's age – control variable
  )

head(small_data)
tail(small_data)

###########################################
#  2.       MISSING VALUES & CLEANING
###########################################

colSums(is.na(small_data))  #Summary of missing values

clean_data <- small_data %>%
  filter(
    !is.na(b5),
    !is.na(m14),
    !is.na(m15),
    !is.na(v106)
  )

cat("Records after cleaning:", nrow(clean_data), "\n")

head(clean_data)
tail(clean_data)

##################################################
# 3.     Binary Code for categorical variables
##################################################

#__________Child Survival Status (0,1)__________________________________
clean_data$b5 <- factor(
  clean_data$b5,
  levels = c(0, 1),
  labels = c("Dead", "Alive")
)

# Numeric version for logistic regression (1 = Alive, 0 = Dead)
clean_data$survived <- ifelse(clean_data$b5 == "Alive", 1, 0)

# ______Antinatal Care Adequacy ANC variable___________________________

# Adequate = >= 4 visits | Inadequate = < 4 visits
clean_data$anc_adequate <- ifelse(clean_data$m14 >= 4, 1, 0)

clean_data$anc_adequate <- factor(
  clean_data$anc_adequate,
  levels = c(0, 1),
  labels = c("Inadequate ANC (<4 visits)", "Adequate ANC (>=4 visits)")
)

# ________________Place of delivery______________________________________

# DHS codes: < 30 = health facility; >= 30 = home or non-facility
clean_data$facility_delivery <- ifelse(clean_data$m15 < 30, 1, 0)

clean_data$facility_delivery <- factor(
  clean_data$facility_delivery,
  levels = c(0, 1),
  labels = c("Home Delivery", "Facility Delivery")
)

# ______________Mother's Education_______________________________________

clean_data$education <- factor(
  clean_data$v106,
  levels = c(0, 1, 2, 3),
  labels = c("No Education", "Primary", "Secondary", "Higher")
)

# ____________Residence Type__________________________________________
clean_data$residence <- factor(
  clean_data$v025,
  levels = c(1, 2),
  labels = c("Urban", "Rural")
)

##################################
# 4. DESCRIPTIVE STATISTICS
##################################

cat("\n-- Child survival status --\n")

table(clean_data$b5)

prop.table(table(clean_data$b5)) * 100

# _____________Main variable____________________________________________
cat("\n-- ANC adequacy (main variable) --\n")

table(clean_data$anc_adequate)

prop.table(table(clean_data$anc_adequate)) * 100

# ____________Controled variables____________________________________
cat("\n-- Place of delivery (control) --\n")

table(clean_data$facility_delivery)

prop.table(table(clean_data$facility_delivery)) * 100

cat("\n-- Mother's education (control) --\n")

table(clean_data$education)

prop.table(table(clean_data$education)) * 100

summary(clean_data)

#####################################
# 5.  CROSS-TABULATION
#####################################

cat("\n-- Child survival x ANC adequacy (counts) --\n")

surv_anc_table <- table(clean_data$b5, clean_data$anc_adequate)

print(surv_anc_table)

cat("\n-- Column percentages --\n")

print(round(prop.table(surv_anc_table, 2) * 100, 2))

cat("\n-- Death rate by ANC group --\n")

cat(sprintf("  Inadequate ANC (<4 visits):  %.2f%% of children died\n",
            prop.table(surv_anc_table, 2)["Dead", "Inadequate ANC (<4 visits)"] * 100))

cat(sprintf("  Adequate ANC  (>=4 visits):  %.2f%% of children died\n",
            prop.table(surv_anc_table, 2)["Dead", "Adequate ANC (>=4 visits)"] * 100))


####################################
# Chi-square test of independence
####################################
chi_result <- chisq.test(surv_anc_table)

cat("\n══ CHI-SQUARE TEST OF INDEPENDENCE ══\n")

print(chi_result)

cat("\n-- Hypothesis Decision: Chi-Square (alpha = 0.05) --\n")
if (chi_result$p.value < 0.05) {
  cat("DECISION: Reject H0.\n")
  cat("There is a statistically significant association between\n")
  cat("ANC adequacy and under-five child mortality in Nigeria.\n")
  cat("The data supports H1: adequate ANC reduces under-five mortality.\n")
} else {
  cat("DECISION: Fail to reject H0.\n")
  cat("No statistically significant association found.\n")
}

# ─────────────────────────────────────────────────────────────────
# 6. ABRIDGED LIFE TABLE FUNCTION
# Applied here to under-five ages in months, stratified by ANC group
# ─────────────────────────────────────────────────────────────────
# Age interval start points (months): 0, 1, 6, 12, 24, 36, 48
# Interval widths (n):                1, 5, 6, 12, 12, 12, 12

build_child_lt <- function(deaths_by_age, total_births) {
  
  x  <- c(0, 1, 6, 12, 24, 36, 48)
  n  <- c(1, 5,  6, 12, 12, 12, 12)
  m  <- length(x)
  
  # ax: average fraction of interval lived by those who die
  ax    <- n / 2
  ax[1] <- 0.1
  ax[2] <- 0.4
  
  # Central death rate: observed deaths / total births at risk
  # Each interval uses remaining survivors as denominator
  lx_obs <- numeric(m + 1)
  lx_obs[1] <- total_births
  for (i in 1:m) {
    lx_obs[i + 1] <- max(lx_obs[i] - deaths_by_age[i], 0)
  }
  
  # Person-years exposed in each interval
  dx_obs <- lx_obs[1:m] - lx_obs[2:(m + 1)]
  Lx_obs <- n * lx_obs[2:(m + 1)] + ax * dx_obs
  
  # mx: central death rate
  mx <- ifelse(Lx_obs > 0, dx_obs / Lx_obs, 0)
  
  # qx: probability of dying in the interval
  qx <- (n * mx) / (1 + (n - ax) * mx)
  qx <- pmin(pmax(qx, 0), 1)   # Bound between 0 and 1
  
  # px: probability of surviving
  px <- 1 - qx
  
  # Standardised life table from radix of 100,000
  lx_std    <- numeric(m + 1)
  lx_std[1] <- 100000
  for (i in 1:m) {
    lx_std[i + 1] <- lx_std[i] * px[i]
  }
  
  dx_std <- lx_std[1:m] - lx_std[2:(m + 1)]
  Lx_std <- n * lx_std[2:(m + 1)] + ax * dx_std
  
  # Tx: cumulative person-months lived above age x
  Tx <- numeric(m)
  for (i in 1:m) {
    Tx[i] <- sum(Lx_std[i:m])
  }
  
  ex <- Tx / lx_std[1:m]   # Life expectancy
  
  data.frame(
    x  = x,
    n  = n,
    Dx = deaths_by_age,
    mx = round(mx,  6),
    ax = round(ax,  3),
    qx = round(qx,  6),
    px = round(px,  6),
    lx = round(lx_std[1:m], 1),
    dx = round(dx_std, 1),
    Lx = round(Lx_std, 1),
    Tx = round(Tx,  1),
    ex = round(ex,  3)
  )
}

############################################
# 7. BUILD LIFE TABLES BY ANC GROUP
# ##########################################

age_breaks <- c(0, 1, 6, 12, 24, 36, 48, 60)

age_labels <- c("0", "1-5", "6-11", "12-23", "24-35", "36-47", "48-59")

dead_data <- clean_data %>%
  filter(b5 == "Dead", !is.na(b7), b7 < 60) %>%
  mutate(
    age_group = cut(b7,
                    breaks         = age_breaks,
                    labels         = age_labels,
                    right          = FALSE,
                    include.lowest = TRUE)
  )

get_deaths <- function(df, group_val) {
  d <- df %>% filter(anc_adequate == group_val)
  as.integer(table(factor(d$age_group, levels = age_labels)))
}

get_n <- function(df, group_val) {
  nrow(df %>% filter(anc_adequate == group_val))
}

lt_adeq <- build_child_lt(
  get_deaths(dead_data,  "Adequate ANC (>=4 visits)"),
  get_n(clean_data,      "Adequate ANC (>=4 visits)")
)

lt_inad <- build_child_lt(
  get_deaths(dead_data,  "Inadequate ANC (<4 visits)"),
  get_n(clean_data,      "Inadequate ANC (<4 visits)")
)

cat("\n== Life Table: Adequate ANC (>=4 visits) ==\n")

print(lt_adeq)

cat("\n== Life Table: Inadequate ANC (<4 visits) ==\n")

print(lt_inad)

# ─────────────────────────────────────────────────────────────────
# 8. COMBINE TABLES FOR PLOTTING
# ─────────────────────────────────────────────────────────────────
age_labels_plot <- c("0m", "1-5m", "6-11m", "12-23m", "24-35m", "36-47m", "48-59m")

lt_combined <- bind_rows(
  lt_adeq %>% mutate(Group = "Adequate ANC (>=4 visits)"),
  lt_inad %>% mutate(Group = "Inadequate ANC (<4 visits)")
) %>%
  mutate(
    x_label = factor(
      age_labels_plot[match(x, c(0, 1, 6, 12, 24, 36, 48))],
      levels = age_labels_plot
    )
  )

# ─────────────────────────────────────────────────────────────────
# 9. VISUALISATIONS
# ─────────────────────────────────────────────────────────────────

# All plots stratified by ANC adequacy (the hypothesis variable)

# 9a. -----------Survival curve: lx --------------------------------

p_lx <- ggplot(lt_combined,
               aes(x = x_label, y = lx, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(
    name   = expression(l[x] ~ "(survivors per 100,000 births)"),
    limits = c(90000, 100000)
  ) +
  labs(x = "Age (months)", color = "",
       title    = "Survival Curve by ANC Adequacy",
       subtitle = "Nigeria DHS - children under 5") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(size = 10, color = "black"),
        axis.text.y     = element_text(size = 10, color = "black"))

print(p_lx)

# 9b. Probability of death: qx-------------------------------
p_qx <- ggplot(lt_combined,
               aes(x = x_label, y = qx * 1000, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(name = expression(q[x] ~ "(deaths per 1,000)")) +
  labs(x = "Age (months)", color = "",
       title    = "Probability of Death by ANC Adequacy",
       subtitle = "Nigeria DHS - children under 5") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(size = 10, color = "black"),
        axis.text.y     = element_text(size = 10, color = "black"))

print(p_qx)

# 9c. Deaths distribution: dx----------------------------------
p_dx <- ggplot(lt_combined,
               aes(x = x_label, y = dx, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(name = expression(d[x] ~ "(deaths per 100,000 births)")) +
  labs(x = "Age (months)", color = "",
       title    = "Deaths Distribution by ANC Adequacy",
       subtitle = "Nigeria DHS - children under 5") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(size = 10, color = "black"),
        axis.text.y     = element_text(size = 10, color = "black"))

print(p_dx)

# 9d. Life expectancy: ex---------------------------------------
p_ex <- ggplot(lt_combined,
               aes(x = x_label, y = ex, color = Group, group = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(name = expression(e[x] ~ "(months of remaining life)")) +
  labs(x = "Age (months)", color = "",
       title    = "Life Expectancy by ANC Adequacy",
       subtitle = "Nigeria DHS - children under 5") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(size = 10, color = "black"),
        axis.text.y     = element_text(size = 10, color = "black"))

print(p_ex)

# 9e. All four life table plots together---------------------------
grid.arrange(p_lx, p_qx, p_dx, p_ex, ncol = 2)

# 9f. Under-5 Mortality Rate bar chart-----------------------------
u5mr <- data.frame(
  Group = c("Adequate ANC\n(>=4 visits)", "Inadequate ANC\n(<4 visits)"),
  U5MR  = c(
    (1 - lt_adeq$lx[7] / 100000) * 1000,
    (1 - lt_inad$lx[7] / 100000) * 1000
  )
)

ggplot(u5mr, aes(x = Group, y = U5MR, fill = Group)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(U5MR, 1)),
            vjust = 1.6, color = "white", size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(
    name   = "Under-5 Mortality Rate (per 1,000 births)",
    breaks = seq(0, 100, 10)
  ) +
  labs(x = "", fill = "",
       title    = "Under-5 Mortality Rate by ANC Adequacy",
       subtitle = "Nigeria DHS - Abridged Life Table estimates") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x     = element_text(size = 11, color = "black"),
        axis.text.y     = element_text(size = 10, color = "black"))

# ─────────────────────────────────────────────────────────────────
# 10. LOGISTIC REGRESSION
# ─────────────────────────────────────────────────────────────────

# Testing H0: Adequate ANC has no effect on under-five mortality
# anc_adequate is the main predictor; all other variables are controls

model <- glm(
  survived ~ anc_adequate      # Main variable – hypothesis test
  + facility_delivery  # Control: place of delivery
  + education          # Control: mother's education
  + v190               # Control: wealth index
  + residence          # Control: urban/rural
  + v012,              # Control: mother's age
  data   = clean_data,
  family = binomial(link = "logit")
)

summary(model)


summary(survived ~ anc_adequate + facility_delivery + education + v190 + residence + v012,
        data=clean_data,
        fun=table)

# Odds ratios with 95% confidence intervals
OR <- exp(coef(model))
CI <- exp(confint(model))

results <- data.frame(
  Variable = names(OR),
  OR       = round(OR, 3),
  CI_lower = round(CI[, 1], 3),
  CI_upper = round(CI[, 2], 3)
)
print(results)

# -- Odds ratio plot --
results_plot <- results[-1, ]

ggplot(results_plot, aes(x = reorder(Variable, OR), y = OR)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.25, color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "tomato", size = 1) +
  coord_flip() +
  scale_y_continuous(name   = "Odds Ratio (95% CI)",
                     breaks = seq(0, 3, 0.5)) +
  labs(x = "",
       title    = "Logistic Regression - Odds Ratios for Child Survival",
       subtitle = "Main variable: ANC adequacy | Reference: Inadequate ANC (<4 visits)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"))
