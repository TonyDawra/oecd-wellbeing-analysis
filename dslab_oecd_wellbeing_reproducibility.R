# ============================================================
# DSLAB PROJECT
# Life Satisfaction and Multidimensional Well-being in OECD Countries
# Period: 2010–2025
# Method: latest available observation for each country and measure
# ============================================================

# -----------------------------
# 1. Install and load packages
# -----------------------------

packages <- c("tidyverse", "janitor", "skimr", "broom")

installed_packages <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed_packages)) {
    install.packages(p)
  }
}

library(tidyverse)
library(janitor)
library(skimr)
library(broom)

# -----------------------------
# 2. Load the OECD data
# -----------------------------

# IMPORTANT:
# Change this filename to the name of your downloaded OECD DATA csv.
# Do NOT use the metadata file.

file_path <- "OECD_WELLBEING_DATA.csv"

df_raw <- read_csv(file_path, show_col_types = FALSE) %>%
  clean_names()

# Check the column names
names(df_raw)
glimpse(df_raw)

# -----------------------------
# 3. Helper functions
# -----------------------------

pick_first_col <- function(data, candidates) {
  hits <- candidates[candidates %in% names(data)]
  if (length(hits) == 0) {
    return(NA_character_)
  } else {
    return(hits[1])
  }
}

find_col_by_content <- function(data, pattern) {
  char_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  
  scores <- sapply(char_cols, function(col) {
    sum(str_detect(
      str_to_lower(as.character(data[[col]])),
      str_to_lower(pattern)
    ), na.rm = TRUE)
  })
  
  if (length(scores) == 0 || max(scores) == 0) {
    return(NA_character_)
  } else {
    return(names(which.max(scores)))
  }
}

# -----------------------------
# 4. Detect important columns
# -----------------------------

country_col <- pick_first_col(
  df_raw,
  c("reference_area", "ref_area_label", "country", "country_name", "location")
)

if (is.na(country_col)) {
  country_col <- find_col_by_content(df_raw, "italy|france|germany|spain|japan")
}

measure_col <- find_col_by_content(
  df_raw,
  "life satisfaction|social support|employment rate|housing|income|health"
)

time_col <- pick_first_col(
  df_raw,
  c("time_period", "time", "year")
)

value_col <- pick_first_col(
  df_raw,
  c("obs_value", "observation_value", "value")
)

if (any(is.na(c(country_col, measure_col, time_col, value_col)))) {
  stop("Some important columns were not detected. Check the column names of your CSV file.")
}

country_col
measure_col
time_col
value_col

# -----------------------------
# 5. Filter to Total groups if available
# -----------------------------

filter_to_total <- function(data) {
  possible_cols <- names(data)[str_detect(names(data), "sex|age|education")]
  
  for (col in possible_cols) {
    values_lower <- str_to_lower(as.character(data[[col]]))
    
    if (any(str_detect(values_lower, "^total$|_t$|all persons|all levels|total population"), na.rm = TRUE)) {
      data <- data %>%
        filter(
          is.na(.data[[col]]) |
            str_detect(
              str_to_lower(as.character(.data[[col]])),
              "^total$|_t$|all persons|all levels|total population"
            )
        )
    }
  }
  
  return(data)
}

df_total <- filter_to_total(df_raw)

# -----------------------------
# 6. Select period and measures
# -----------------------------

df_clean <- df_total %>%
  mutate(
    country = str_squish(as.character(.data[[country_col]])),
    measure_label = str_squish(as.character(.data[[measure_col]])),
    measure_lower = str_to_lower(measure_label),
    year = as.integer(parse_number(as.character(.data[[time_col]]))),
    value = parse_number(as.character(.data[[value_col]]))
  ) %>%
  filter(year >= 2010, year <= 2025)

# Map OECD measures to simple variable names

df_selected <- df_clean %>%
  mutate(
    measure_key = case_when(
      str_detect(measure_lower, "life satisfaction") &
        !str_detect(measure_lower, "less than|top|bottom|quintile") ~ "life_satisfaction",
      
      str_detect(measure_lower, "net adjusted disposable income") ~ "income",
      str_detect(measure_lower, "median net wealth") ~ "wealth",
      str_detect(measure_lower, "poverty line|relative income poverty") ~ "poverty",
      
      str_detect(measure_lower, "employment rate") ~ "employment_rate",
      str_detect(measure_lower, "labour market insecurity|labor market insecurity") ~ "labour_market_insecurity",
      str_detect(measure_lower, "long hours") ~ "long_hours",
      str_detect(measure_lower, "job strain") ~ "job_strain",
      
      str_detect(measure_lower, "housing affordability") ~ "housing_affordability",
      str_detect(measure_lower, "housing cost overburden") ~ "housing_cost_overburden",
      str_detect(measure_lower, "overcrowded") ~ "overcrowding",
      
      str_detect(measure_lower, "life expectancy") ~ "life_expectancy",
      str_detect(measure_lower, "perceived health") ~ "perceived_health_positive",
      
      str_detect(measure_lower, "lack of social support") ~ "lack_social_support",
      str_detect(measure_lower, "social support") ~ "social_support",
      
      str_detect(measure_lower, "having a say") ~ "say_in_government",
      str_detect(measure_lower, "voter turnout") ~ "voter_turnout",
      
      str_detect(measure_lower, "air pollution") ~ "air_pollution",
      str_detect(measure_lower, "extreme temperature") ~ "extreme_temperature",
      
      str_detect(measure_lower, "feeling safe") ~ "feeling_safe",
      str_detect(measure_lower, "road deaths") ~ "road_deaths",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(measure_key), !is.na(value))

# Check which measures were found
df_selected %>%
  distinct(measure_key, measure_label) %>%
  arrange(measure_key) %>%
  print(n = 50)

# -----------------------------
# 7. Keep latest available observation
# -----------------------------

df_latest_long <- df_selected %>%
  group_by(country, measure_key) %>%
  arrange(desc(year), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# Convert from long to wide format

df_wide <- df_latest_long %>%
  select(country, measure_key, value) %>%
  pivot_wider(
    names_from = measure_key,
    values_from = value,
    values_fn = function(x) mean(x, na.rm = TRUE)
  )

# Remove possible aggregate rows if present
df_wide <- df_wide %>%
  filter(!str_detect(str_to_lower(country), "oecd|european union"))

# Check final dataset
glimpse(df_wide)
skim(df_wide)

# Save clean datasets
dir.create("outputs", showWarnings = FALSE)

write_csv(df_latest_long, "outputs/latest_long_data.csv")
write_csv(df_wide, "outputs/latest_wide_data.csv")

# -----------------------------
# 8. Missing value analysis
# -----------------------------

missing_table <- df_wide %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  ) %>%
  arrange(desc(missing_values))

missing_table

write_csv(missing_table, "outputs/missing_values.csv")

# -----------------------------
# 9. Descriptive statistics
# -----------------------------

descriptive_stats <- df_wide %>%
  select(where(is.numeric)) %>%
  summarise(across(
    everything(),
    list(
      n = ~ sum(!is.na(.)),
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      min = ~ min(., na.rm = TRUE),
      max = ~ max(., na.rm = TRUE)
    )
  ))

descriptive_stats

write_csv(descriptive_stats, "outputs/descriptive_statistics.csv")

# -----------------------------
# 10. Define dimensions
# -----------------------------

dimension_map <- tibble(
  variable = c(
    "income",
    "wealth",
    "poverty",
    
    "employment_rate",
    "labour_market_insecurity",
    "long_hours",
    "job_strain",
    
    "housing_affordability",
    "housing_cost_overburden",
    "overcrowding",
    
    "life_expectancy",
    "perceived_health_positive",
    
    "social_support",
    "lack_social_support",
    
    "say_in_government",
    "voter_turnout",
    
    "air_pollution",
    "extreme_temperature",
    "feeling_safe",
    "road_deaths"
  ),
  
  dimension_key = c(
    "economic_security",
    "economic_security",
    "economic_security",
    
    "work_quality",
    "work_quality",
    "work_quality",
    "work_quality",
    
    "housing",
    "housing",
    "housing",
    
    "health",
    "health",
    
    "social_connections",
    "social_connections",
    
    "civic_engagement",
    "civic_engagement",
    
    "environment_safety",
    "environment_safety",
    "environment_safety",
    "environment_safety"
  ),
  
  dimension_label = c(
    "Economic security",
    "Economic security",
    "Economic security",
    
    "Work quality",
    "Work quality",
    "Work quality",
    "Work quality",
    
    "Housing",
    "Housing",
    "Housing",
    
    "Health",
    "Health",
    
    "Social connections",
    "Social connections",
    
    "Civic engagement",
    "Civic engagement",
    
    "Environment and safety",
    "Environment and safety",
    "Environment and safety",
    "Environment and safety"
  ),
  
  # direction = 1 means higher value is better
  # direction = -1 means higher value is worse
  direction = c(
    1, 1, -1,
    1, -1, -1, -1,
    1, -1, -1,
    1, 1,
    1, -1,
    1, 1,
    -1, -1, 1, -1
  )
)

available_vars <- intersect(dimension_map$variable, names(df_wide))

available_vars

# -----------------------------
# 11. Correlation with life satisfaction
# -----------------------------

safe_cor <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 10) {
    return(NA_real_)
  } else {
    return(cor(x[ok], y[ok]))
  }
}

if (!("life_satisfaction" %in% names(df_wide))) {
  stop("Life satisfaction was not found. Check the selected measures in the OECD download.")
}

cor_table <- tibble(variable = available_vars) %>%
  mutate(
    correlation_with_life_satisfaction = map_dbl(
      variable,
      ~ safe_cor(df_wide$life_satisfaction, df_wide[[.x]])
    )
  ) %>%
  left_join(dimension_map, by = "variable") %>%
  arrange(desc(abs(correlation_with_life_satisfaction)))

cor_table

write_csv(cor_table, "outputs/correlation_with_life_satisfaction.csv")

# Plot: strongest measure-level correlations

p1 <- cor_table %>%
  filter(!is.na(correlation_with_life_satisfaction)) %>%
  slice_max(abs(correlation_with_life_satisfaction), n = 12) %>%
  ggplot(aes(
    x = reorder(variable, correlation_with_life_satisfaction),
    y = correlation_with_life_satisfaction
  )) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Strongest correlations with life satisfaction",
    x = "Measure",
    y = "Correlation with life satisfaction"
  ) +
  theme_minimal()

p1

ggsave("outputs/plot_measure_correlations.png", p1, width = 9, height = 6, dpi = 300)

# -----------------------------
# 12. Create dimension scores
# -----------------------------

# Each measure is standardized.
# Bad indicators are reversed, so higher dimension score means better well-being.

long_for_scores <- df_wide %>%
  select(country, all_of(available_vars)) %>%
  pivot_longer(
    cols = -country,
    names_to = "variable",
    values_to = "value"
  ) %>%
  left_join(dimension_map, by = "variable") %>%
  group_by(variable) %>%
  mutate(z_value = as.numeric(scale(value))) %>%
  ungroup() %>%
  mutate(score_component = z_value * direction)

dimension_scores <- long_for_scores %>%
  group_by(country, dimension_key, dimension_label) %>%
  summarise(
    dimension_score = mean(score_component, na.rm = TRUE),
    n_measures_used = sum(!is.na(score_component)),
    .groups = "drop"
  ) %>%
  filter(n_measures_used > 0)

dimension_scores_wide <- dimension_scores %>%
  select(country, dimension_key, dimension_score) %>%
  pivot_wider(
    names_from = dimension_key,
    values_from = dimension_score,
    names_prefix = "dim_"
  )

analysis_scores <- df_wide %>%
  select(country, life_satisfaction) %>%
  left_join(dimension_scores_wide, by = "country")

write_csv(dimension_scores, "outputs/dimension_scores_long.csv")
write_csv(analysis_scores, "outputs/dimension_scores_wide.csv")

# -----------------------------
# 13. Correlation between dimensions and life satisfaction
# -----------------------------

dimension_vars <- names(analysis_scores)[str_detect(names(analysis_scores), "^dim_")]

dimension_labels <- dimension_map %>%
  distinct(dimension_key, dimension_label)

dimension_corr <- tibble(dimension_var = dimension_vars) %>%
  mutate(
    correlation_with_life_satisfaction = map_dbl(
      dimension_var,
      ~ safe_cor(analysis_scores$life_satisfaction, analysis_scores[[.x]])
    ),
    dimension_key = str_remove(dimension_var, "^dim_")
  ) %>%
  left_join(dimension_labels, by = "dimension_key") %>%
  arrange(desc(abs(correlation_with_life_satisfaction)))

dimension_corr

write_csv(dimension_corr, "outputs/dimension_correlations.csv")

# Plot: dimension correlations

p2 <- dimension_corr %>%
  filter(!is.na(correlation_with_life_satisfaction)) %>%
  ggplot(aes(
    x = reorder(dimension_label, correlation_with_life_satisfaction),
    y = correlation_with_life_satisfaction
  )) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Which well-being dimensions are most associated with life satisfaction?",
    x = "Well-being dimension",
    y = "Correlation with life satisfaction"
  ) +
  theme_minimal()

p2

ggsave("outputs/plot_dimension_correlations.png", p2, width = 9, height = 6, dpi = 300)

# -----------------------------
# 14. Scatterplot for strongest individual measure
# -----------------------------

top_variable <- cor_table %>%
  filter(!is.na(correlation_with_life_satisfaction)) %>%
  slice_max(abs(correlation_with_life_satisfaction), n = 1) %>%
  pull(variable)

top_variable

p3 <- ggplot(df_wide, aes(x = .data[[top_variable]], y = life_satisfaction, label = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(check_overlap = TRUE, size = 3) +
  labs(
    title = paste("Life satisfaction and", top_variable),
    x = top_variable,
    y = "Life satisfaction"
  ) +
  theme_minimal()

p3

ggsave("outputs/plot_top_measure_scatter.png", p3, width = 9, height = 6, dpi = 300)

# -----------------------------
# 15. Italy vs OECD average excluding Italy
# -----------------------------

italy_row <- analysis_scores %>%
  filter(str_to_lower(country) == "italy")

oecd_average_row <- analysis_scores %>%
  filter(str_to_lower(country) != "italy") %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
  mutate(country = "OECD average excluding Italy")

italy_vs_oecd <- bind_rows(
  italy_row,
  oecd_average_row
)

write_csv(italy_vs_oecd, "outputs/italy_vs_oecd_average_excluding_italy.csv")

italy_dimension_compare <- italy_vs_oecd %>%
  select(country, all_of(dimension_vars)) %>%
  pivot_longer(
    cols = -country,
    names_to = "dimension_var",
    values_to = "score"
  ) %>%
  mutate(
    dimension_key = str_remove(dimension_var, "^dim_")
  ) %>%
  left_join(dimension_labels, by = "dimension_key")

p4 <- ggplot(
  italy_dimension_compare,
  aes(x = reorder(dimension_label, score), y = score)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ country) +
  labs(
    title = "Italy compared with the OECD average excluding Italy",
    x = "Well-being dimension",
    y = "Standardized dimension score"
  ) +
  theme_minimal()

p4

ggsave(
  "outputs/plot_italy_vs_oecd_dimensions_excluding_italy.png",
  p4,
  width = 10,
  height = 6,
  dpi = 300
)

# -----------------------------
# 16. Simple regression using dimensions
# -----------------------------

# Important:
# We do NOT regress life satisfaction on all 20 measures.
# Instead, we use the strongest 3 dimensions.

top_dimensions <- dimension_corr %>%
  filter(!is.na(correlation_with_life_satisfaction)) %>%
  slice_max(abs(correlation_with_life_satisfaction), n = 3) %>%
  pull(dimension_var)

top_dimensions

model_data <- analysis_scores %>%
  select(country, life_satisfaction, all_of(top_dimensions)) %>%
  drop_na()

if (nrow(model_data) >= 10) {
  
  model_formula <- reformulate(
    termlabels = top_dimensions,
    response = "life_satisfaction"
  )
  
  model <- lm(model_formula, data = model_data)
  
  summary(model)
  
  model_tidy <- tidy(model)
  model_glance <- glance(model)
  
  write_csv(model_tidy, "outputs/regression_coefficients.csv")
  write_csv(model_glance, "outputs/regression_model_summary.csv")
  
} else {
  warning("Not enough complete observations for regression.")
}

# -----------------------------
# 17. Export final analysis dataset
# -----------------------------

write_csv(model_data, "outputs/final_regression_dataset.csv")

# End of code
# ============================================================
# 18. MORE COLORFUL AND READABLE PLOTS
# These plots overwrite the previous grey versions in the outputs folder.
# ============================================================

# Color palette used in the report
positive_col <- "#2A9D8F"   # green/teal for positive relations
negative_col <- "#E76F51"   # orange/red for negative relations
italy_col    <- "#D62828"   # red for Italy
other_col    <- "#457B9D"   # blue for other OECD countries
oecd_col     <- "#1D3557"   # dark blue for OECD average
neutral_col  <- "#6C757D"   # grey for neutral elements

clean_label <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_to_sentence()
}

report_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3),
      plot.subtitle = element_text(size = base_size, color = "grey30"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# -----------------------------
# 18.1 Colorful measure-level correlation plot
# -----------------------------

p1 <- cor_table %>%
  filter(!is.na(correlation_with_life_satisfaction)) %>%
  slice_max(abs(correlation_with_life_satisfaction), n = 12) %>%
  mutate(
    sign = if_else(correlation_with_life_satisfaction >= 0, "Positive association", "Negative association"),
    variable_label = clean_label(variable),
    label_position = if_else(correlation_with_life_satisfaction >= 0, 0.035, -0.035),
    label_hjust = if_else(correlation_with_life_satisfaction >= 0, 0, 1)
  ) %>%
  ggplot(aes(
    x = reorder(variable_label, correlation_with_life_satisfaction),
    y = correlation_with_life_satisfaction,
    fill = sign
  )) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, linewidth = 0.7, color = "grey25") +
  geom_text(
    aes(
      y = correlation_with_life_satisfaction + label_position,
      label = sprintf("%.2f", correlation_with_life_satisfaction),
      hjust = label_hjust
    ),
    size = 3.6
  ) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Positive association" = positive_col,
    "Negative association" = negative_col
  )) +
  labs(
    title = "Strongest correlations with life satisfaction",
    subtitle = "Positive values indicate that higher values of the measure are associated with higher life satisfaction",
    x = "Measure",
    y = "Correlation with life satisfaction",
    fill = "Direction"
  ) +
  report_theme()

p1

ggsave("outputs/plot_measure_correlations.png", p1, width = 10, height = 6.5, dpi = 300)

# -----------------------------
# 18.2 Colorful dimension correlation plot
# -----------------------------

p2 <- dimension_corr %>%
  filter(!is.na(correlation_with_life_satisfaction)) %>%
  mutate(
    sign = if_else(correlation_with_life_satisfaction >= 0, "Positive association", "Negative association"),
    label_position = if_else(correlation_with_life_satisfaction >= 0, 0.03, -0.03),
    label_hjust = if_else(correlation_with_life_satisfaction >= 0, 0, 1)
  ) %>%
  ggplot(aes(
    x = reorder(dimension_label, correlation_with_life_satisfaction),
    y = correlation_with_life_satisfaction,
    fill = sign
  )) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, linewidth = 0.7, color = "grey25") +
  geom_text(
    aes(
      y = correlation_with_life_satisfaction + label_position,
      label = sprintf("%.2f", correlation_with_life_satisfaction),
      hjust = label_hjust
    ),
    size = 3.8
  ) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Positive association" = positive_col,
    "Negative association" = negative_col
  )) +
  labs(
    title = "Which well-being dimensions are most associated with life satisfaction?",
    subtitle = "Social connections show the strongest positive association in the selected OECD data",
    x = "Well-being dimension",
    y = "Correlation with life satisfaction",
    fill = "Direction"
  ) +
  report_theme()

p2

ggsave("outputs/plot_dimension_correlations.png", p2, width = 10, height = 6, dpi = 300)

# -----------------------------
# 18.3 Colorful scatterplot: strongest individual measure
# -----------------------------

p3 <- df_wide %>%
  filter(!is.na(life_satisfaction), !is.na(.data[[top_variable]])) %>%
  mutate(
    highlight = if_else(country == "Italy", "Italy", "Other OECD countries")
  ) %>%
  ggplot(aes(x = .data[[top_variable]], y = life_satisfaction)) +
  geom_point(aes(color = highlight), size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = oecd_col, fill = "#A8DADC", linewidth = 1.1) +
  geom_text(
    aes(label = country, color = highlight),
    check_overlap = TRUE,
    size = 3.2,
    vjust = -0.6
  ) +
  scale_color_manual(values = c(
    "Italy" = italy_col,
    "Other OECD countries" = other_col
  )) +
  labs(
    title = paste("Life satisfaction and", clean_label(top_variable)),
    subtitle = "Italy is highlighted in red; the line shows the cross-country linear trend",
    x = clean_label(top_variable),
    y = "Life satisfaction",
    color = "Country group"
  ) +
  report_theme()

p3

ggsave("outputs/plot_top_measure_scatter.png", p3, width = 10, height = 6.5, dpi = 300)

# -----------------------------
# 18.4 Colorful Italy vs OECD comparison
# -----------------------------

p4 <- italy_dimension_compare %>%
  mutate(
    country = recode(country,
      "OECD average excluding Italy" = "OECD avg. excl. Italy"
    ),
    dimension_label = factor(dimension_label)
  ) %>%
  ggplot(aes(
    x = reorder(dimension_label, score),
    y = score,
    fill = country
  )) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_hline(yintercept = 0, linewidth = 0.7, color = "grey25") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Italy" = italy_col,
    "OECD avg. excl. Italy" = oecd_col
  )) +
  labs(
    title = "Italy compared with the OECD average excluding Italy",
    subtitle = "Scores are standardized; values above zero indicate better-than-average performance",
    x = "Well-being dimension",
    y = "Standardized dimension score",
    fill = "Benchmark"
  ) +
  report_theme()

p4

ggsave(
  "outputs/plot_italy_vs_oecd_dimensions_excluding_italy.png",
  p4,
  width = 10,
  height = 6.5,
  dpi = 300
)

# -----------------------------
# 18.5 Optional: missing values plot for the data section
# -----------------------------

p_missing <- missing_table %>%
  filter(variable != "country", missing_values > 0) %>%
  mutate(variable_label = clean_label(variable)) %>%
  ggplot(aes(
    x = reorder(variable_label, missing_values),
    y = missing_values,
    fill = missing_values
  )) +
  geom_col(width = 0.72) +
  geom_text(aes(label = missing_values), hjust = -0.25, size = 3.6) +
  coord_flip() +
  scale_fill_gradient(low = "#A8DADC", high = "#E76F51") +
  labs(
    title = "Missing values by variable",
    subtitle = "Only variables with at least one missing observation are shown",
    x = "Variable",
    y = "Number of missing values",
    fill = "Missing"
  ) +
  report_theme() +
  theme(legend.position = "none")

p_missing

ggsave("outputs/plot_missing_values.png", p_missing, width = 9, height = 6, dpi = 300)

# End of colorful plotting section
