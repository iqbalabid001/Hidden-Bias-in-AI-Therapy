library(haven)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(ggcorrplot)

# filter specific variables when loading
# plb0022_h	Employment status [harmonised]
# plh0182	Current Life Satisfaction
# plh0175 satisfaction with household income
# mh_nbs Mental Health - z-transformed ( > 50 good, < 50 not)
# e11102	Employment Status of Individual
# m11125	Satisfaction with Health
# d11104	Marital Status of Individual  


pl <- read_dta("SOEP Teaching v40/pl.dta", col_select = c(pid, syear, plb0022_h, plh0182, plh0175))
health <- read_dta("SOEP Teaching v40/health.dta", col_select = c(pid, syear, mh_nbs))
pequiv <- read_dta("SOEP Teaching v40/pequiv.dta", col_select = c(pid, syear, e11102, m11125, d11104))

# check
table(pequiv$e11102, useNA = "ifany")
attr(pequiv$e11102, "labels")

# merge data

pl2 <- pl %>% mutate(pid = as.numeric(zap_labels(pid)),
                     syear = as.integer(zap_labels(syear)))

health2 <- health %>% mutate(pid = as.numeric(zap_labels(pid)),
                             syear = as.integer(zap_labels(syear)))

pequiv2 <- pequiv %>% mutate(pid = as.numeric(zap_labels(pid)),
                             syear = as.integer(zap_labels(syear)))

SOEP <- pl2 %>%
  inner_join(health2, by = c("pid", "syear")) %>%
  inner_join(pequiv2, by = c("pid", "syear"))


# give the variables nice names

SOEP <- SOEP %>%
  rename(
    emp_status_harmonized = plb0022_h,   # harmonised employment status
    emp_status_raw        = e11102,      # raw employment status
    life_satisfaction     = plh0182,     # current life satisfaction
    hh_income_satisfaction= plh0175,      # # satisfaction with household income
    mental_health_nbs     = mh_nbs,      # mental health (NBS)
    health_satisfaction   = m11125,      # satisfaction with health
    marital_status        = d11104       # marital status
  )


# filter out values that are not useful

SOEP_clean <- SOEP %>%
  filter(
    emp_status_harmonized > 0,      # removes negative missings
    life_satisfaction >= 0,         # 0–10 scale, keep 0
    hh_income_satisfaction >= 0,    # 0–10 scale, keep 0
    mental_health_nbs >= 0,         # scale, keep valid values
    emp_status_raw >= 0,            # keep 0/1
    health_satisfaction >= 0,       # 0–10 scale, keep 0
    marital_status > 0              # marital status categories (start at 1)
  )

# check NA
colSums(is.na(SOEP_clean))


# descriptive statistics

# create a summary table of the continuous variables
cont_table <- SOEP_clean %>%
  select(life_satisfaction, mental_health_nbs, health_satisfaction, hh_income_satisfaction) %>%
  summarise(
    across(everything(), list(
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE)
    ))
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Stat"), 
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = Stat, values_from = value)

kable(cont_table, digits = 2, caption = "Descriptive Statistics (Continuous)")

# create a correlation matrix of continuous variables

cor_subset <- SOEP_clean %>%
  select(life_satisfaction, mental_health_nbs, health_satisfaction, hh_income_satisfaction) %>%
  na.omit()

# calculate the correlation
cor_matrix <- cor(cor_subset)

# create the "Lower Triangular" correlation plot
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",     # This makes it the triangle shape
           lab = TRUE,          # This adds the numbers (0.5, 0.47, etc.)
           lab_size = 5, 
           colors = c("#6D9EC1", "white", "#E46726"), # Blue to Orange scale
           title = "Correlation of Continuous Variables", 
           ggtheme = theme_minimal())

# crate a distribution histogram of key variables

SOEP_clean %>%
  # select variables to visualize
  select(emp_status_harmonized, life_satisfaction, mental_health_nbs, health_satisfaction, hh_income_satisfaction, marital_status) %>%
  # remove rows with NAs so they don't break the histogram
  drop_na() %>%
  # ensure all are numeric
  mutate(across(everything(), as.numeric)) %>%
  # pivot for ggplot
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>% 
  #plot
  ggplot(aes(x = Value)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) + 
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of Key Variables",
    x = "Value",
    y = "Frequency"
  )

# and emp_status_raw is binary (42417 times 0; 80205 times 1)

# analysis
# check mental health before and after being unemployed
# identify people who are employed (based on emp_status_harmonized)

employed_harmonized <- c(
  1,  # Voll erwerbstätig
  2,  # Teilzeitbeschäftigung
  4,  # Geringfügig erwerbstätig
  12  # Kurzarbeit
)
unemployed_raw <- 0   # 0 = not employed / unemployed

# sort data to check history chronologically
SOEP_arranged <- SOEP_clean %>%
  arrange(pid, syear)


# check transition
transitions <- SOEP_arranged %>%
  group_by(pid) %>%
  mutate(
    employed_before = emp_status_harmonized %in% employed_harmonized,
    unemployed_now  = emp_status_raw == unemployed_raw,
    
    # lagged values
    employed_prev   = lag(employed_before),
    mh_prev         = lag(mental_health_nbs),
    year_prev       = lag(syear)
  ) %>%
  ungroup() %>%
  
  # transition: employed to unemployed
  filter(
    employed_prev == TRUE,
    unemployed_now == TRUE
  ) %>%
  
  # Keep only clean mental health observations
  filter(
    !is.na(mh_prev),
    !is.na(mental_health_nbs)
  ) %>%
  
  transmute(
    pid,
    year_before = year_prev,
    year_after  = syear,
    mh_before   = mh_prev,
    mh_after    = mental_health_nbs,
    mh_change   = mh_after - mh_before
  )

summary(transitions$mh_before)
summary(transitions$mh_after)
summary(transitions$mh_change)
mean(transitions$mh_change, na.rm = TRUE)

# simple OLS: Does 'before' predict 'after'?
ols_transition <- lm(mh_after ~ mh_before, data = transitions)

# testing the "Change" directly against a constant 
# this will tell us if the average increase (mh_change) is significantly different from zero
ols_change <- lm(mh_change ~ 1, data = transitions)

# results
summary(ols_transition)
summary(ols_change)
# result shows the average mental health score increased slightly by 0.431228
# after the transition to unemployment

# paired t-test to see statistical significance, p > 0.05
t.test(
  transitions$mh_after,
  transitions$mh_before,
  paired = TRUE
)
# shows it is statistically significant
# however, a change of 0.43 is only 0.04 SDs in SF-12
# a noticeable change would be 3 to 5 maybe
# might be Relief Effect: If the previous employment was high-stress

# track mental health over duration of unemployment
SOEP_arranged <- SOEP_arranged %>%
  group_by(pid) %>%
  arrange(pid, syear) %>%
  mutate(
    # create the counter for consecutive years of unemployment
    is_unemployed = (emp_status_raw == unemployed_raw),
    unemployment_duration = sequence(rle(is_unemployed)$lengths) * is_unemployed,
    
    # keep your original transition variables as well
    employed_before = emp_status_harmonized %in% employed_harmonized,
    employed_prev   = lag(employed_before),
    mh_prev         = lag(mental_health_nbs),
    year_prev       = lag(syear)
  ) %>%
  ungroup()

# summary for the visualization
duration_analysis <- SOEP_arranged %>%
  filter(is_unemployed == TRUE) %>% 
  group_by(unemployment_duration) %>%
  summarise(
    avg_mh = mean(mental_health_nbs, na.rm = TRUE),
    n_obs = n()
  )

# plot the line (to show aggregate increase or decrease)
ggplot(duration_analysis, aes(x = unemployment_duration, y = avg_mh)) +
  geom_line() +
  geom_point(aes(size = n_obs)) +
  labs(title = "Mental Health by Duration of Unemployment",
       x = "Years Spent Unemployed",
       y = "Average Mental Health Score",
       size = "Sample Size")

# shows increase in mental health over time
# but we don't know if these are the some people
# to see if the same person's mental health actually improves over time 
# (rather than just the "unhappy" people leaving the sample), we use a Fixed Effects (FE) Model

# load the library plm to run the FE
library(plm)

# FE model

fe_model <- plm(mental_health_nbs ~ unemployment_duration, 
                data = SOEP_arranged, 
                index = c("pid", "syear"), 
                model = "within")

summary(fe_model)

# now it appears, mental health indeed declines for individual by 0.074/y
# mental health score drops by about 0.075 points per year

# recreate the scenario with the emp_status_raw (pequiv$e11102)

# define 0/1 dummy variables
employed_val   <- 1
not_employed_val <- 0

SOEP_arranged_e11102 <- SOEP_clean %>%
  group_by(pid) %>%
  arrange(pid, syear) %>%
  mutate(
    # basic status
    is_emp = (emp_status_raw == employed_val),
    is_not_emp = (emp_status_raw == not_employed_val),
    
    # counter for Duration
    # to track how long they have stayed at '0'
    unemployment_duration = sequence(rle(is_not_emp)$lengths) * is_not_emp,
    
    # transition lag
    was_emp_prev = lag(is_emp)
  ) %>%
  ungroup()

# FE Model for the 'e11102'
fe_e11102 <- plm(mental_health_nbs ~ unemployment_duration, 
                 data = SOEP_arranged_e11102, 
                 index = c("pid", "syear"), 
                 model = "within")

summary(fe_e11102)

# we are getting pretty much same result

# however, r-squared is very low, meaning it only explains tiny fraction of changes

# let's see if other factors influence decline in mental health 

SOEP_arranged_final <- SOEP_arranged %>%
  filter(marital_status != 7) %>% # filter out the 'Under 18' group
  
  # convert to factor and label them for readable results
  mutate(marital_status = factor(marital_status, 
                                 levels = c(1, 2, 3, 4, 5, 6),
                                 labels = c("Married", "Single", "Widowed", 
                                            "Divorced", "Separated", "NotWPartner")))

# run the model
fe_model_expanded <- plm(
  mental_health_nbs ~ unemployment_duration + 
    life_satisfaction + 
    hh_income_satisfaction + 
    health_satisfaction + 
    marital_status, 
  data = SOEP_arranged_final, 
  index = c("pid", "syear"), 
  model = "within"
)
summary(fe_model_expanded)

# compare the R-squared of the old model and the new one
cat("Old R2:", summary(fe_model)$r.squared[1], "\n")
cat("New R2:", summary(fe_model_expanded)$r.squared[1])

# now, r-squared increased by nearly 800 times (from 0.01% to 12.4%)

# plot the results

# Prepare data for bar chart
plot_data <- data.frame(
  Status = factor(c("Before", "After"), levels = c("Before", "After")),
  Avg_MH = c(50.44, 50.87)
)

# 2. Create the bar graph
ggplot(plot_data, aes(x = Status, y = Avg_MH)) +
  geom_col(fill = "steelblue", width = 0.5) +
  # Add the specific values on top
  geom_text(aes(label = Avg_MH), vjust = -0.5, size = 5) +
  # Zoom the scale to see the difference clearly
  coord_cartesian(ylim = c(50, 51)) +
  theme_minimal() +
  labs(title = "Relief Effect", x = NULL, y = "Mental Health Score")


# plot for the "True" Scarring Effect (-0.075 per year)
intercept <- mean(SOEP_arranged_final$mental_health_nbs, na.rm=TRUE)
coef_duration <- -0.074955

prediction_df <- data.frame(year = 0:10) %>%
  mutate(predicted_mh = intercept + (year * coef_duration))

ggplot(prediction_df, aes(x = year, y = predicted_mh)) +
  geom_line(color = "firebrick", size = 1.5) +
  geom_point() +
  theme_minimal() +
  labs(title = "The True Scarring Effect (Fixed Effects)",
       subtitle = "Mental health consistently declines for the same individual",
       x = "Years of Unemployment", y = "Predicted MH Score")

# shows a straight line
# does it actually decline in a straight line or it curves?
# to find out we can consider every year as unique event

library(broom)

# we can use a Fixed Effects model using Factors
# this way the data show its own shape for each year
fe_model_factor <- plm(
  mental_health_nbs ~ as.factor(unemployment_duration) + 
    life_satisfaction + health_satisfaction + marital_status, 
  data = SOEP_arranged_final, 
  index = c("pid", "syear"), 
  model = "within"
)

summary(fe_model_factor)

# it is not linear
# let's see how it actually goes over time

# extract and clean the coefficients for plotting
coef_data <- tidy(fe_model_factor) %>%
  filter(grepl("unemployment_duration", term)) %>%
  mutate(
    # convert "as.factor(unemployment_duration)1" into just the number 1
    year = as.numeric(gsub("as.factor\\(unemployment_duration\\)", "", term)),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  ) %>%
  filter(year <= 10) %>% # keep up to 10 years to avoid noise
  select(year, estimate, conf.low, conf.high)

# year 0 (the reference baseline where change is 0)
year_zero <- data.frame(year = 0, estimate = 0, conf.low = 0, conf.high = 0)
plot_data <- rbind(year_zero, coef_data)

# create plot
ggplot(plot_data, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "steelblue") + 
  geom_line(color = "red", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.4) +
  scale_x_continuous(breaks = 0:10) +
  theme_minimal() +
  labs(
    title = "Impact of Unemployment Duration on Mental Health",
    subtitle = "Fixed Effects: Comparing an individual to their own employed baseline (Year 0)",
    x = "Years of Unemployment",
    y = "Change in Mental Health (NBS Score)"
  )

# since marriage seem to be a great factor here
# we can do an interaction model of unemployment_duration * marital_status

# as we found earlier the effect is not straight line
# we will take the squared unemployment_duration

# interaction model
# using quadratic model, to avoid having too many coefficients
fe_interaction <- plm(
  mental_health_nbs ~ (unemployment_duration + I(unemployment_duration^2)) * marital_status + 
    life_satisfaction + health_satisfaction, 
  data = SOEP_arranged_final, 
  index = c("pid", "syear"), 
  model = "within"
)

summary(fe_interaction)

# it appears that widowed people get almost zero relief from unemployment

# plot the result of interaction

# define the range of years, 0.5 steps to make the curve look smooth
years <- seq(0, 10, by = 0.5)

# extract Coefficients (using Widowed results)
b1 <- 0.4488768    # Linear (Married/Baseline)
b2 <- -0.0523931   # Quadratic (Married/Baseline)

# widowed Interaction Terms (from summary output)
int_b1_widow <- -0.3856923  # Linear interaction for Widowed
int_b2_widow <- 0.0481283   # Quadratic interaction for Widowed

# calculate Predicted Curves
# married: y = (b1 * t) + (b2 * t^2)
# widowed:  y = ((b1 + int_b1_widow) * t) + ((b2 + int_b2_widow) * t^2)
curve_data_widow <- data.frame(year = years) %>% # create time line
  mutate(
    Married = (b1 * year) + (b2 * year^2), # linear + quadratic
    Widowed = ((b1 + int_b1_widow) * year) + ((b2 + int_b2_widow) * year^2) # b1 + interaction
  ) %>%
  tidyr::pivot_longer(cols = c(Married, Widowed), names_to = "Status", values_to = "Effect")

# plot the comparison
ggplot(curve_data_widow, aes(x = year, y = Effect, color = Status)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  scale_color_manual(values = c("Married" = "steelblue", "Widowed" = "firebrick")) +
  labs(
    title = "The impact of Widowhood in Unemployment",
    subtitle = "Comparing Married (Baseline) vs. Widowed Trajectories",
    x = "Years of Unemployment",
    y = "Predicted Change in Mental Health"
  )
