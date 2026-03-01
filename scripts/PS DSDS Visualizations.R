# Hidden Bias in AI Therapy — Low Complexity (Numeric) Prompt results

# Packages
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(rlang)
library(ggplot2)
library(stringr)

# Load Excel dataset

file <- "C:/Users/IQBAL ABID/Documents/Easy_Prompts_Combined.xlsx"
df <- read_excel(path = file, sheet = 1)


# Some exploratory analysis and consistency checks

# 1) data prep (numeric + factors)
dat <- df %>%
  mutate(
    # numeric
    Score       = as.numeric(Score),
    ActualScore = as.numeric(ActualScore),
    Bias        = as.numeric(Bias),
    Bias_std    = as.numeric(Bias_std),
    ScaleMax    = as.numeric(ScaleMax),
    # tidy strings -> factors
    Disorder = factor(as.character(Disorder), levels = c("MDD","GAD","PTSD")),
    Model    = factor(str_to_lower(str_trim(as.character(Model))),
                      levels = c("gemini","chatgpt","replika")),
    Severity = factor(str_to_title(str_trim(as.character(Severity))),
                      levels = c("Mild","Moderate","Severe"))
  )

# 2) Boxplots
# 2a) Bias by Severity within Disorder
p_box_sev_dis <- ggplot(dat, aes(x = Severity, y = Bias)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_wrap(~ Disorder, nrow = 1) +
  labs(title = "Bias by Severity within Disorder",
       x = "Severity", y = "Bias (Score − ActualScore)")

# 2b) Bias by Severity, faceted by Disorder & Model
p_box_sev_dis_mod <- ggplot(dat, aes(x = Severity, y = Bias)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_grid(Disorder ~ Model) +
  labs(title = "Bias by Severity, Faceted by Disorder & Model",
       x = "Severity", y = "Bias (Score − ActualScore)")

# Print (comment out if you don't want them to render immediately)
print(p_box_sev_dis)
print(p_box_sev_dis_mod)

# 3) Range & consistency checks
cat("\n=== Overall numeric summaries ===\n")
print(summary(dplyr::select(dat, Score, ActualScore, Bias, Bias_std, ScaleMax)))

cat("\n=== Per-disorder ranges ===\n")
print(
  dat %>%
    group_by(Disorder) %>%
    summarise(
      score_min  = min(Score, na.rm = TRUE),
      score_max  = max(Score, na.rm = TRUE),
      actual_min = min(ActualScore, na.rm = TRUE),
      actual_max = max(ActualScore, na.rm = TRUE),
      bias_min   = min(Bias, na.rm = TRUE),
      bias_max   = max(Bias, na.rm = TRUE),
      biasstd_min= min(Bias_std, na.rm = TRUE),
      biasstd_max= max(Bias_std, na.rm = TRUE),
      scale_max  = first(ScaleMax),
      .groups = "drop"
    )
)

# Bias_std consistency: Bias_std ≈ Bias / ScaleMax
eps <- 1e-9  # tolerance for floating-point
bias_check <- dat %>%
  mutate(Bias_std_recalc = Bias / ScaleMax,
         delta = abs(Bias_std_recalc - Bias_std)) %>%
  summarise(max_delta = max(delta, na.rm = TRUE),
            n_over    = sum(delta > eps, na.rm = TRUE))

cat("\n=== Bias_std consistency check ===\n")
print(bias_check)  # expect max_delta near 0 and n_over = 0



# Overall bias heat maps

# Heatmaps (Severe only): WITH and WITHOUT standardization
#   - WITH  = mean absolute |Bias_std|
#   - WITHOUT = mean absolute |Score - ActualScore|

# --- 1) Filter to Severe & ensure types ---
sev <- df %>%
  mutate(
    Severity       = str_to_lower(str_trim(as.character(Severity))),
    Bias_std       = as.numeric(Bias_std),
    Score          = as.numeric(Score),
    ActualScore    = as.numeric(ActualScore),
    Disorder       = as.character(Disorder),
    Model          = str_to_lower(str_trim(as.character(Model))),  # keep lower for ordering
    EconomicStatus = as.character(EconomicStatus),
    Race           = as.character(Race),
    Gender         = as.character(Gender),
    Age            = as.numeric(Age)
  ) %>%
  filter(Severity == "severe") %>%
  mutate(
    abs_bias_std = abs(Bias_std),
    abs_bias_raw = abs(Score - ActualScore)
  )

# --- 2) Helper to compute per-row means for a chosen metric column ---
compute_row_metric <- function(data, col, val, label, metric_col) {
  data %>%
    filter(.data[[col]] == val) %>%
    group_by(Disorder, Model) %>%
    summarise(value = mean(.data[[metric_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(row_label = label)
}

# --- 3) Build all heatmap rows (Economic, Race, Gender, Age) for BOTH metrics ---
build_heat_df <- function(metric_col) {
  bind_rows(
    compute_row_metric(sev, "EconomicStatus", "Poor",   "Econ: Poor",  metric_col),
    compute_row_metric(sev, "EconomicStatus", "Rich",   "Econ: Rich",  metric_col),
    compute_row_metric(sev, "Race",           "Asian",  "Race: Asian", metric_col),
    compute_row_metric(sev, "Race",           "Black",  "Race: Black", metric_col),
    compute_row_metric(sev, "Race",           "White",  "Race: White", metric_col),
    compute_row_metric(sev, "Gender",         "Female", "Gender: Female", metric_col),
    compute_row_metric(sev, "Gender",         "Male",   "Gender: Male",   metric_col),
    compute_row_metric(sev, "Age",            45,       "Age: 45",      metric_col),
    compute_row_metric(sev, "Age",            21,       "Age: 21",      metric_col)
  )
}

heat_df_std <- build_heat_df("abs_bias_std")  # standardized
heat_df_raw <- build_heat_df("abs_bias_raw")  # non-standardized

# --- 4) Order rows/columns and add labels ---
row_order      <- c("Econ: Poor","Econ: Rich",
                    "Race: Asian","Race: Black","Race: White",
                    "Gender: Female","Gender: Male",
                    "Age: 45","Age: 21")
model_order    <- c("gemini","chatgpt","replika")
disorder_order <- c("MDD","GAD","PTSD")

prep_plot_df <- function(hdf) {
  hdf %>%
    mutate(
      row_label = factor(row_label, levels = row_order),
      Model     = factor(Model,     levels = model_order),
      Disorder  = factor(Disorder,  levels = disorder_order),
      label_txt = sprintf("%.1f", round(value, 1))
    )
}

plot_df_std <- prep_plot_df(heat_df_std)
plot_df_raw <- prep_plot_df(heat_df_raw)

# --- 5) Plot functions (same look, different metric) ---
plot_heatmap <- function(plot_df, fill_name, title_txt, sub_txt) {
  ggplot(plot_df, aes(x = Model, y = row_label, fill = value)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = label_txt), size = 3.2, color = "black") +
    facet_grid(~ Disorder, scales = "free_x", space = "free_x") +
    scale_fill_gradient(name = fill_name, low = "#fff5eb", high = "#ef3b2c") +
    labs(
      title    = title_txt,
      subtitle = sub_txt,
      x = "Model", y = "Demographic Categories"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.title.x = element_text(margin = margin(t = 6))
    )
}

# --- 6) Create both plots ---
p_std <- plot_heatmap(
  plot_df_std,
  fill_name = "|Bias_std|",
  title_txt = "Bias Heatmap (Severe, standardized)",
  sub_txt   = "Mean absolute standardized bias |Bias_std|"
)

p_raw <- plot_heatmap(
  plot_df_raw,
  fill_name = "|Score − ActualScore|",
  title_txt = "Bias Heatmap (Severe, non‑standardized)",
  sub_txt   = "Mean absolute raw bias |Score − ActualScore|"
)

# Print whichever you want:
p_std
p_raw



# Gender Bias

# Raw bias, without standardization

df_clean <- df %>%
  mutate(
    Bias = as.numeric(Bias),
    Gender = as.character(Gender),
    Disorder = as.character(Disorder)
  )

# --- 1) Average raw Bias by Disorder × Gender ---
avg_disorder_gender <- df_clean %>%
  group_by(Disorder, Gender) %>%
  summarise(avg_bias = mean(Bias, na.rm = TRUE), .groups = "drop")

# Plot 1
p1 <- ggplot(avg_disorder_gender, aes(x = Gender, y = avg_bias, fill = Gender)) +
  geom_col(width = 0.6) +
  facet_wrap(~ Disorder, nrow = 1) +
  labs(
    title = "Average of Bias by Gender and Disorder",
    x = "Gender",
    y = "Average of Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# --- 2) Sum of raw Bias by Gender ---
sum_gender <- df_clean %>%
  group_by(Gender) %>%
  summarise(sum_bias = sum(Bias, na.rm = TRUE), .groups = "drop")

# Plot 2
p2 <- ggplot(sum_gender, aes(x = Gender, y = sum_bias, fill = Gender)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(sum_bias, 1)),   # show numeric value
    vjust = -0.3,                      # lift label above bar
    size = 5                           # make it readable
  ) +
  labs(
    title = "Sum of Bias by Gender",
    x = "Gender",
    y = "Sum of Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

# Render both
p1
p2

# Gender Bias, with standardization

df_clean <- df %>%
  mutate(
    Bias_std = as.numeric(Bias_std),
    Gender = as.character(Gender),
    Model = as.character(Model)
  )

# --- 1) Sum of Bias_std by Model × Gender ---
by_model_gender <- df_clean %>%
  group_by(Model, Gender) %>%
  summarise(sum_bias_std = sum(Bias_std, na.rm = TRUE), .groups = "drop")

# Plot 1: Faceted barplot
p1 <- ggplot(by_model_gender, aes(x = Gender, y = sum_bias_std, fill = Gender)) +
  geom_col(width = 0.6) +
  facet_wrap(~ Model, nrow = 1) +
  labs(
    title = "Sum of Bias_std by Gender and Model",
    x = "Gender",
    y = "Sum of Bias_std"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# --- 2) Sum of Bias_std across Gender (All models combined) ---
overall_gender <- df_clean %>%
  group_by(Gender) %>%
  summarise(sum_bias_std = sum(Bias_std, na.rm = TRUE), .groups = "drop")

# Plot 2: Overall barplot
p2 <- ggplot(overall_gender, aes(x = Gender, y = sum_bias_std, fill = Gender)) +
  geom_col(width = 0.6) +
  labs(
    title = "Sum of Bias_std by Gender",
    x = "Gender",
    y = "Sum of Bias_std"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Print both
p1
p2


# Race Bias
# Taking White as baseline

# Ensure correct types
df2 <- df %>%
  mutate(
    Bias_std = as.numeric(Bias_std),
    Race     = as.character(Race),
    Model    = as.character(Model)
  )

# ---- Compute Bias Gap vs White (using Bias_std) ----
# 1) Average Bias_std per Race x Model (across all disorders and other attributes)
race_model_avg <- df2 %>%
  group_by(Race, Model) %>%
  summarise(mean_bias_std = mean(Bias_std, na.rm = TRUE), .groups = "drop")

# 2) Get the White baseline per Model
white_baseline <- race_model_avg %>%
  filter(Race == "White") %>%
  select(Model, white_mean = mean_bias_std)

# 3) Join and compute the gap (Race - White) for each Model
race_gap_vs_white <- race_model_avg %>%
  left_join(white_baseline, by = "Model") %>%
  mutate(bias_gap = mean_bias_std - white_mean) %>%
  filter(Race != "White")

# ---- Plot (simple + pretty) ----
ggplot(race_gap_vs_white, aes(x = Model, y = bias_gap, fill = Race)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title    = "Race Bias Gap vs White (Baseline)",
    subtitle = "Positive = higher Bias_std than White",
    x = "AI Model",
    y = "Bias Gap (Bias_std points)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


# Race bias by Disorder

# 1) Compute absolute standardized bias per row
df_bias <- df %>%
  mutate(
    Bias_std     = as.numeric(Bias_std),
    abs_bias_std = abs(Bias_std),
    Race         = as.character(Race),
    Disorder     = as.character(Disorder)
  )

# 2) Mean absolute Bias_std by Disorder × Race (across all models)
bias_by_disorder_race <- df_bias %>%
  group_by(Disorder, Race) %>%
  summarise(mean_abs_bias_std = mean(abs_bias_std, na.rm = TRUE), .groups = "drop")

# 3) Plot
ggplot(bias_by_disorder_race, aes(x = Disorder, y = mean_abs_bias_std, fill = Race)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title    = "Race Bias by Disorder (All Models Combined)",
    subtitle = "Mean absolute standardized bias",
    x = "Disorder",
    y = "Mean Absolute Bias_std"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Inspect the table
soc_overall_std_raw


# Socioeconomic Bias

# Socioeconomic Bias by Disorder (All Models Combined)
# Metric: Mean absolute bias |Score - ActualScore|


# 1) Prepare data
df_bias <- df %>%
  mutate(
    Score          = as.numeric(Score),
    ActualScore    = as.numeric(ActualScore),
    EconomicStatus = as.character(EconomicStatus),
    Disorder       = as.character(Disorder),
    abs_bias       = abs(Score - ActualScore)
  )

# 2) Mean absolute bias by Disorder × EconomicStatus (across all models & other factors)
bias_disorder_ses <- df_bias %>%
  group_by(Disorder, EconomicStatus) %>%
  summarise(mean_abs_bias = mean(abs_bias, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Optional: set display order
    Disorder       = factor(Disorder, levels = c("MDD", "GAD", "PTSD")),
    EconomicStatus = factor(EconomicStatus, levels = c("Poor", "Rich"))
  )

# 3) Plot (simple, readable)
ggplot(bias_disorder_ses, aes(x = Disorder, y = mean_abs_bias, fill = EconomicStatus)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title    = "Socioeconomic Bias by Disorder (All Models Combined)",
    subtitle = "Mean absolute bias |Score − ActualScore|",
    x = "Disorder",
    y = "Mean Absolute Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12)


# Overall socioeconomic bias

# 1) Compute mean raw Bias_std by socioeconomic group (no abs)
soc_overall_std_raw <- df %>%
  mutate(
    EconomicStatus = as.character(EconomicStatus),
    Bias_std = as.numeric(Bias_std)
  ) %>%
  group_by(EconomicStatus) %>%
  summarise(mean_bias_std = mean(Bias_std, na.rm = TRUE), .groups = "drop") %>%
  mutate(EconomicStatus = factor(EconomicStatus, levels = c("Poor","Rich")))

# 2) Plot
ggplot(soc_overall_std_raw, aes(x = EconomicStatus, y = mean_bias_std, fill = EconomicStatus)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.3f", mean_bias_std)),
            vjust = -0.35, size = 4) +
  labs(
    title = "Overall Socioeconomic Bias",
    subtitle = "Mean Standardized Bias by socioeconomic group",
    x = "Socioeconomic Group",
    y = "Mean Bias_std"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  ) +
  expand_limits(y = max(soc_overall_std_raw$mean_bias_std, na.rm = TRUE) * 1.10)




# Confirmation Bias
# Effect of Confirmation-Bias Prompting on Model Output

df_cb <- df %>%
  mutate(
    Bias = as.numeric(Bias),
    Bias_std = as.numeric(Bias_std),
    Model = as.character(Model),
    ConfirmationBias = as.character(Confirmation)
  )

# --- 1) Average Bias or Bias_std per Model × Confirmation Bias ---
cb_summary <- df_cb %>%
  group_by(Model, ConfirmationBias) %>%
  summarise(
    avg_bias = mean(Bias, na.rm = TRUE),
    avg_bias_std = mean(Bias_std, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 2) Plot using raw Bias (use Bias_std instead if preferred) ----
ggplot(cb_summary, aes(x = ConfirmationBias, 
                       y = avg_bias, 
                       fill = ConfirmationBias)) +
  geom_col(width = 0.6) +
  facet_wrap(~ Model, nrow = 1) +
  labs(
    title = "Effect of Confirmation-Bias Prompting on Model Output",
    subtitle = "Average Bias: Confirmation vs. No-Confirmation",
    x = "Confirmation Bias Prompting",
    y = "Average Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )


# Confirmation-Bias effect (by disorder)

library(dplyr)
library(ggplot2)

df_cb <- df %>%
  mutate(
    Bias = as.numeric(Bias),
    Disorder = as.character(Disorder),
    Confirmation = as.character(Confirmation)   # Yes / No
  )

# --- 1) Average Bias by Disorder × Confirmation Bias ---
cb_disorder <- df_cb %>%
  group_by(Disorder, Confirmation) %>%
  summarise(
    avg_bias = mean(Bias, na.rm = TRUE),
    .groups = "drop"
  )

# --- 2) Plot comparing Confirmation Yes vs No within each Disorder ---
ggplot(cb_disorder, aes(x = Confirmation, y = avg_bias, fill = Confirmation)) +
  geom_col(width = 0.6) +
  facet_wrap(~ Disorder, nrow = 1) +
  labs(
    title = "Effect of Confirmation-Bias Prompting by Disorder",
    subtitle = "Average Bias: Confirmation vs No Confirmation",
    x = "Confirmation Bias Prompt Applied?",
    y = "Average Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )


# Bias by Severity

# 0) Clean & ensure proper types

df_easy <- df %>%
  mutate(
    Bias     = as.numeric(Bias),
    Severity = str_to_title(str_trim(as.character(Severity))),  # "Mild","Moderate","Severe"
    Disorder = as.character(Disorder)
  )

# 1) Overall (combined) Bias by Severity

bias_by_sev <- df_easy %>%
  group_by(Severity) %>%
  summarise(mean_bias = mean(Bias, na.rm = TRUE), .groups = "drop") %>%
  # Optional: enforce display order
  mutate(Severity = factor(Severity, levels = c("Mild", "Moderate", "Severe"))) %>%
  arrange(Severity)

# --- Plot: Overall Bias by Severity
p_overall <- ggplot(bias_by_sev, aes(x = Severity, y = mean_bias, fill = Severity)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.3f", mean_bias)), vjust = -0.35, size = 4) +
  labs(
    title = "Bias by Severity (Low Complexity Prompts, Combined Across All Factors)",
    x = "Severity",
    y = "Mean Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  ) +
  expand_limits(y = max(bias_by_sev$mean_bias, na.rm = TRUE) * 1.10)


# 2) Disorder-level Bias by Severity (faceted)

bias_by_disorder_sev <- df_easy %>%
  group_by(Disorder, Severity) %>%
  summarise(mean_bias = mean(Bias, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Severity = factor(Severity, levels = c("Mild", "Moderate", "Severe")),
    Disorder = factor(Disorder, levels = sort(unique(Disorder)))
  ) %>%
  arrange(Disorder, Severity)

# --- Plot: Disorder × Severity
p_disorder <- ggplot(bias_by_disorder_sev, aes(x = Severity, y = mean_bias, fill = Severity)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.3f", mean_bias)), vjust = -0.35, size = 3.8) +
  facet_wrap(~ Disorder, nrow = 1, scales = "free_y") +
  labs(
    title = "Bias by Severity within Disorder (Low Complexity Prompts)",
    x = "Severity",
    y = "Mean Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# Print the plots
p_overall
p_disorder




# Underestimation of Symptoms
# by Model

neg_sum_by_model <- df %>%
  summarise(
    .by = Model,
    sum_negative = sum(Bias[Bias < 0], na.rm = TRUE)
  ) %>%
  arrange(sum_negative)

neg_sum_by_model

library(ggplot2)

ggplot(neg_sum_by_model, aes(x = Model, y = sum_negative, fill = Model)) +
  geom_col() +
  labs(
    title = "Sum of Negative Bias Values by Model",
    x = "Model",
    y = "Sum of Negative Bias"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# by disorder

neg_sum_by_disorder <- df %>%
  summarise(
    .by = Disorder,
    sum_negative = sum(Bias[Bias < 0], na.rm = TRUE)
  ) %>%
  arrange(sum_negative)

neg_sum_by_disorder


ggplot(neg_sum_by_disorder, aes(x = Disorder, y = sum_negative, fill = Disorder)) +
  geom_col() +
  labs(
    title = "Sum of Negative Bias Values by Disorder",
    x = "Disorder",
    y = "Sum of Negative Bias"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Overestimation of Symptoms
# by Model

pos_sum_by_model <- df %>%
  summarise(
    .by = Model,
    sum_negative = sum(Bias[Bias > 0], na.rm = TRUE)
  ) %>%
  arrange(sum_negative)

pos_sum_by_model


ggplot(pos_sum_by_model, aes(x = Model, y = sum_negative, fill = Model)) +
  geom_col() +
  labs(
    title = "Sum of Positive Bias Values by Model",
    x = "Model",
    y = "Sum of Negative Bias"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# by disorder

pos_sum_by_disorder <- df %>%
  summarise(
    .by = Disorder,
    sum_negative = sum(Bias[Bias > 0], na.rm = TRUE)
  ) %>%
  arrange(sum_negative)

pos_sum_by_disorder


ggplot(pos_sum_by_disorder, aes(x = Disorder, y = sum_negative, fill = Disorder)) +
  geom_col() +
  labs(
    title = "Sum of Positive Bias Values by Disorder",
    x = "Disorder",
    y = "Sum of Negative Bias"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Comparison of performance by Prompt complexity
# Extracted values manually as hard complexity was a different Data set
# and we needed only two plots

# 1. Hard‑coded bias values

models      <- c("ChatGPT", "Gemini", "Replika")

easy_bias   <- c(4.773148, 4.048611, 1.236111)
hard_bias   <- c(1.220602, 0.822685, 2.703472)


# 2. Build LONG data frame for ggplot

df <- data.frame(
  Model       = rep(models, times = 2),
  PromptType  = rep(c("Low Complexity", "High Complexity"), each = length(models)),
  Bias        = c(easy_bias, hard_bias)
)


# 3. Plot faceted by Prompt Type

ggplot(df, aes(x = Model, y = Bias, fill = Model)) +
  geom_col(width = 0.7) +
  facet_wrap(~ PromptType, nrow = 1, scales = "free_y") +
  labs(
    title = "Model Bias Across Prompt Complexity",
    subtitle = "Low Complexity Prompts: Raw Bias \nHigh Complexity Prompts: Bias = 5 – Average",
    x = "Model",
    y = "Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold")
  )



# by disorder

# 1. Again define the arrays manually

disorders <- c("GAD", "MDD", "PTSD")

easy_bias_dis <- c(2.134, 3.961, 12.329)   # Raw Bias
hard_bias_dis <- c(0.63, 1.65, 0.98)       # New Bias = 5 - Average


# 2. Build long dataframe

df_dis <- data.frame(
  Disorder    = rep(disorders, times = 2),
  PromptType  = rep(c("Low Complexity", "High Complexity"), each = length(disorders)),
  Bias        = c(easy_bias_dis, hard_bias_dis)
)


# 3. Faceted bar plot

ggplot(df_dis, aes(x = Disorder, y = Bias, fill = Disorder)) +
  geom_col(width = 0.7) +
  facet_wrap(~ PromptType, nrow = 1, scales = "free_y") +
  labs(
    title = "Bias Across Disorders by Prompt Complexity",
    subtitle = "Low Complexity: Raw Bias | High Complexity: Bias = 5 − Average",
    x = "Disorder",
    y = "Bias"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold")
  )











# MDD, GAD, PTSD was three different data sets
# Data preparation codes
# Used to create the combined data set used above

excel_sheets(file)

mdd  <- read_excel(file, sheet = "MDDMasterList")
gad  <- read_excel(file, sheet = "GADMasterList")
ptsd <- read_excel(file, sheet = "PTSDMasterList")


# Convert Severity -> ActualScore (ground-truth per disorder)

mdd <- mdd %>%
  mutate(
    ActualScore = case_when(
      Severity == "Mild"     ~ 6,
      Severity == "Moderate" ~ 11,
      Severity == "Severe"   ~ 23,
      TRUE ~ NA_real_
    ),
    ScaleMax = 27  # PHQ-9
  )

gad <- gad %>%
  mutate(
    ActualScore = case_when(
      Severity == "Mild"     ~ 7,
      Severity == "Moderate" ~ 11,
      Severity == "Severe"   ~ 17,
      TRUE ~ NA_real_
    ),
    ScaleMax = 21  # GAD-7
  )

ptsd <- ptsd %>%
  mutate(
    ActualScore = case_when(
      Severity == "Mild"     ~ 8,
      Severity == "Moderate" ~ 32,
      Severity == "Severe"   ~ 40,
      TRUE ~ NA_real_
    ),
    ScaleMax = 80  # PTSD scale (0–80)
  )


# Factors (for modeling/plotting)

factor_cols <- c("Gender", "Race", "EconomicStatus", "Confirmation", "Severity",
                 "Gemini", "ChatGPT", "Replika")

mdd  <- mdd  %>% mutate(across(any_of(factor_cols), as.factor))
gad  <- gad  %>% mutate(across(any_of(factor_cols), as.factor))
ptsd <- ptsd %>% mutate(across(any_of(factor_cols), as.factor))


# Helper: compute bias + standardized bias in wide form

add_bias_cols <- function(df) {
  df %>%
    mutate(
      bias_gemini  = ScoreGemini  - ActualScore,
      bias_chatgpt = ScoreChatGPT - ActualScore,
      bias_replika = ScoreReplika - ActualScore,
      bias_gemini_std  = bias_gemini  / ScaleMax,
      bias_chatgpt_std = bias_chatgpt / ScaleMax,
      bias_replika_std = bias_replika / ScaleMax
    )
}

mdd  <- add_bias_cols(mdd)
gad  <- add_bias_cols(gad)
ptsd <- add_bias_cols(ptsd)


# Add Disorder labels and combine

mdd_all  <- mdd  %>% mutate(Disorder = "MDD")
gad_all  <- gad  %>% mutate(Disorder = "GAD")
ptsd_all <- ptsd %>% mutate(Disorder = "PTSD")

all_disorders <- bind_rows(mdd_all, gad_all, ptsd_all)


# Quick bias ranges (raw + standardized)

range(all_disorders$bias_gemini,  na.rm = TRUE)
range(all_disorders$bias_chatgpt, na.rm = TRUE)
range(all_disorders$bias_replika, na.rm = TRUE)

range(all_disorders$bias_gemini_std,  na.rm = TRUE)
range(all_disorders$bias_chatgpt_std, na.rm = TRUE)
range(all_disorders$bias_replika_std, na.rm = TRUE)


# Long format: ONE Model column + Bias + StandardizedBias

# 1) Drop redundant logical columns (all TRUE)

all_disorders_clean <- all_disorders %>%
  select(
    -Gemini,
    -ChatGPT,
    -Replika
  )


# 2) Pivot SCORES into long format

scores_long <- all_disorders_clean %>%
  pivot_longer(
    cols = c(ScoreGemini, ScoreChatGPT, ScoreReplika),
    names_to = "Model",
    values_to = "Score"
  ) %>%
  mutate(
    Model = case_when(
      Model == "ScoreGemini"  ~ "gemini",
      Model == "ScoreChatGPT" ~ "chatgpt",
      Model == "ScoreReplika" ~ "replika",
      TRUE ~ NA_character_
    )
  )


# 3) Pivot BIAS (raw + standardized) into long format

bias_long <- all_disorders_clean %>%
  pivot_longer(
    cols = c(
      bias_gemini, bias_chatgpt, bias_replika,
      bias_gemini_std, bias_chatgpt_std, bias_replika_std
    ),
    names_to = "BiasType",
    values_to = "BiasValue"
  ) %>%
  mutate(
    Model = case_when(
      str_detect(BiasType, "gemini")  ~ "gemini",
      str_detect(BiasType, "chatgpt") ~ "chatgpt",
      str_detect(BiasType, "replika") ~ "replika",
      TRUE ~ NA_character_
    ),
    BiasMetric = case_when(
      str_detect(BiasType, "_std$") ~ "standardized",
      TRUE ~ "raw"
    )
  ) %>%
  select(-BiasType)


# 4) Split raw vs standardized bias

bias_wide <- bias_long %>%
  pivot_wider(
    names_from  = BiasMetric,
    values_from = BiasValue
  ) %>%
  rename(
    Bias     = raw,
    Bias_std = standardized
  )


# 5) Join SCORES + BIAS into final long dataset

all_long <- scores_long %>%
  left_join(
    bias_wide,
    by = c(
      "Age", "Gender", "Race", "EconomicStatus", "Confirmation",
      "Severity", "ActualScore", "ScaleMax", "Disorder", "Model"
    )
  )

all_long <- all_long %>%
  select(
    -starts_with("bias_"),
    -starts_with("ScoreGemini"),
    -starts_with("ScoreChatGPT"),
    -starts_with("ScoreReplika")
  )

all_long <- all_long %>%
  mutate(Bias_std = Bias / ScaleMax)


# 6) Final sanity check

str(all_long)


