################################# Descriptive Statistics #######################
# Aim: Create several descriptive statistics tables and figures 
# for the Chile case study

# Packages
library(ggplot2)
library(readr)
library(sf)
library(dplyr)
library(writexl)
library(xtable)
library(tidyr)

# Load data --------------------------------------------------------------------

# Set path
# The folder should contain the downloaded data sets
data_path <- Sys.getenv("DATA_PATH_CHILE")

# Individual-level data
individual_level_preferences_and_result <- read_excel("DataGeneration/Chile/outputs/individual_level_preferences_and_result_province.xlsx")

# Matching outcome by region
matching_outcome_by_region <- read_excel("DataGeneration/Chile/outputs/matching_outcome_by_region.xlsx")

# School data
school_data <- read_excel("DataGeneration/Chile/outputs/school_capacity_with_region_and_province.xlsx")


# Prep data --------------------------------------------------------------------

# Keep data with unique mrun (!Attention only for descriptives about
# socio-demographics)
# Rows: 124.492
individual_level_preferences_and_result_unique_mrun <- individual_level_preferences_and_result %>%
  group_by(mrun) %>%
  slice(1) %>%
  ungroup()
df <- individual_level_preferences_and_result_unique_mrun
N <- nrow(df)

# (1) Demographics -------------------------------------------------------------

# 1.) Create number and share of students by:
# Region
# Province

# Province
province_rows <- df %>%
  count(Region, province, name = "number") %>%
  group_by(Region) %>%
  mutate(
    region_total = sum(number),
    share = number / N,
    variable = "",
    output = paste0("\\hspace{1em}Province ", province),
    row_type = "province"
  ) %>%
  ungroup() %>%
  select(Region, province, variable, output, number, share, row_type)

# Region
region_rows <- df %>%
  count(Region, name = "number") %>%
  mutate(
    Province = -Inf,
    share = number / N,
    variable = paste0("\\textbf{Region ", Region, "}"),
    output = "",
    row_type = "region"
  ) %>%
  select(Region, Province, variable, output, number, share, row_type)

# Combine and sort correctly
region_province_table <- bind_rows(region_rows, province_rows) %>%
  arrange(Region, Province) %>%
  mutate(
    share = round(share, 8)
  ) %>%
  select(variable, output, number, share)

# LaTeX export
print(
  xtable(region_province_table, digits = c(0, 0, 0, 0, 3)),
  include.rownames = FALSE,
  sanitize.text.function = identity
)

# 2.) Create number and share of students by:
# Sex
# Priority Student
# High-Performing Student

# Helper function to create frequency table
create_freq_table <- function(data, var, labels = NULL) {
  data %>%
    count(!!sym(var)) %>%
    mutate(
      variable = var,
      category = if (!is.null(labels)) labels[as.character(!!sym(var))] else as.character(!!sym(var)),
      share = n / N
    ) %>%
    select(variable, category, number = n, share)
}

# Binary variables (map 0/1 to no/yes)
female_tab <- create_freq_table(df, "female", c("0" = "no", "1" = "yes"))
priority_tab <- create_freq_table(df, "priority_student", c("0" = "no", "1" = "yes"))
performance_tab <- create_freq_table(df, "high_performance_student", c("0" = "no", "1" = "yes"))

final_table <- bind_rows(
  female_tab,
  priority_tab,
  performance_tab
)

# LaTeX export
latex_table <- xtable(final_table)
print(latex_table, include.rownames = FALSE)

# (2) Behavior ----------------------------------------------------------------

# 1.) Number and share of stundent applying to school outside their
# Region
# Province

# Region
# For each student, get distinct (student region, school region) pairs,
# collapse all out-of-region schools to "Other"
cross_region_table <- individual_level_preferences_and_result %>%
  left_join(
    school_data %>% distinct(rbd, Region) %>% rename(school_region = Region),
    by = "rbd"
  ) %>%
  distinct(mrun, Region, school_region) %>%
  mutate(school_region = if_else(Region == school_region, school_region, "Other")) %>%
  distinct(mrun, Region, school_region) %>%
  group_by(Region, school_region) %>%
  summarise(n_students = n(), .groups = "drop") %>%
  left_join(
    individual_level_preferences_and_result %>%
      distinct(mrun, Region) %>%
      count(Region, name = "n_total"),
    by = "Region"
  ) %>%
  mutate(pct = n_students / n_total * 100) %>%
  select(
    `Region Student` = Region,
    `Region School`  = school_region,
    Number           = n_students,
    `%`              = pct
  ) %>%
  arrange(`Region Student`, `Region School` == "Other")

# LaTeX export
print(
  xtable(cross_region_table, digits = c(0, 0, 0, 0, 1)),
  include.rownames = FALSE
)

# Province
cross_province_table <- individual_level_preferences_and_result %>%
  left_join(
    school_data %>% distinct(rbd, Provincia) %>% rename(school_province = Provincia),
    by = "rbd"
  ) %>%
  distinct(mrun, province, school_province) %>%
  mutate(school_province = if_else(province == school_province, school_province, "Other")) %>%
  distinct(mrun, province, school_province) %>%
  group_by(province, school_province) %>%
  summarise(n_students = n(), .groups = "drop") %>%
  left_join(
    individual_level_preferences_and_result %>%
      distinct(mrun, province) %>%
      count(province, name = "n_total"),
    by = "province"
  ) %>%
  mutate(pct = n_students / n_total * 100) %>%
  select(
    `Province Student` = province,
    `Province School`  = school_province,
    Number             = n_students,
    `%`                = pct
  ) %>%
  arrange(`Province Student`, `Province School` == "Other")

# LaTeX export
print(
  xtable(cross_province_table, digits = c(0, 0, 0, 0, 1)),
  include.rownames = FALSE
)

# 2.) Ranking list length stats by
# Sex
# Priority Student
# High-Performing Student

list_length_df <- individual_level_preferences_and_result %>%
  group_by(mrun) %>%
  summarise(
    list_length = max(preference_number, na.rm = TRUE),
    female = first(female),
    priority_student = first(priority_student),
    high_performance_student = first(high_performance_student)
  ) %>%
  ungroup()

final_stats <- individual_level_preferences_and_result %>%
  group_by(mrun) %>%
  summarise(
    list_length = max(preference_number, na.rm = TRUE),
    female = first(female),
    priority_student = first(priority_student),
    high_performance_student = first(high_performance_student),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(female, priority_student, high_performance_student),
    names_to = "variable",
    values_to = "category"
  ) %>%
  mutate(
    variable = recode(variable,
                      female = "Female",
                      priority_student = "Priority Student",
                      high_performance_student = "High-Performing Student"
    ),
    category = recode(as.character(category),
                      `0` = "No",
                      `1` = "Yes"
    )
  ) %>%
  group_by(variable, category) %>%
  summarise(
    q1 = quantile(list_length, 0.25, na.rm = TRUE),
    q2 = quantile(list_length, 0.50, na.rm = TRUE),  # median
    q3 = quantile(list_length, 0.75, na.rm = TRUE),
    mean = mean(list_length, na.rm = TRUE),
    sd = sd(list_length, na.rm = TRUE),
    .groups = "drop"
  )

# LaTeX export
print(
  xtable(final_stats, digits = c(0, 0, 0, 2, 2, 2, 2, 2)),
  include.rownames = FALSE
)

# (3) Welfare metrics ----------------------------------------------------------


# 1.) Cumulative % matched by rank for
# Sex
# Priority Student
# High-Performing Student

# Derive assigned rank per student (preference_number where matched_first_round == 1)
assigned_rank_df <- individual_level_preferences_and_result %>%
  mutate(unmatched = !any(matched_first_round == 1), .by = mrun) %>%
  filter(matched_first_round == 1 | unmatched) %>%
  slice(1, .by = mrun) %>%
  transmute(
    mrun,
    female,
    priority_student,
    high_performance_student,
    assigned_rank = if_else(matched_first_round == 1, preference_number, NA_real_)
  )

# Cumulative % matched by rank position by gender
rank_by_gender <- assigned_rank_df %>%
  filter(!is.na(assigned_rank)) %>%
  count(female, assigned_rank) %>%
  left_join(
    assigned_rank_df %>% count(female, name = "n_total"),
    by = "female"
  ) %>%
  arrange(female, assigned_rank) %>%
  mutate(
    cum_pct = cumsum(n) / n_total * 100,
    Gender = if_else(female == 1, "Female", "Male"),
    .by = female
  )

p_gender <- ggplot(rank_by_gender, aes(x = assigned_rank, y = cum_pct, color = Gender)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1, max(rank_by_gender$assigned_rank))) +
  scale_y_continuous(limits = c(min(rank_by_gender$cum_pct), max(rank_by_gender$cum_pct))) +
  scale_color_manual(values = c("Female" = "#FFBE6A", "Male" = "#40B0A6")) +
  labs(
    x = "Rank Position",
    y = "Cumulative % Matched",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave("DataGeneration/Chile/outputs/cum_rank_by_gender.pdf", p_gender, width = 8, height = 5)

# Cumulative % matched by rank position by priority status
rank_by_priority <- assigned_rank_df %>%
  filter(!is.na(assigned_rank)) %>%
  count(priority_student, assigned_rank) %>%
  left_join(
    assigned_rank_df %>% count(priority_student, name = "n_total"),
    by = "priority_student"
  ) %>%
  arrange(priority_student, assigned_rank) %>%
  mutate(
    cum_pct = cumsum(n) / n_total * 100,
    Priority = factor(
      if_else(priority_student == 1, "Priority", "Non-Priority"),
      levels = c("Priority", "Non-Priority")
    ),
    .by = priority_student
  )

p_priority <- ggplot(rank_by_priority, aes(x = assigned_rank, y = cum_pct, color = Priority)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1, max(rank_by_priority$assigned_rank))) +
  scale_y_continuous(limits = c(min(rank_by_priority$cum_pct), max(rank_by_priority$cum_pct))) +
  scale_color_manual(values = c("Priority" = "#FFBE6A", "Non-Priority" = "#40B0A6")) +
  labs(
    x = "Rank Position",
    y = "Cumulative % Matched",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave("DataGeneration/Chile/outputs/cum_rank_by_priority.pdf", p_priority, width = 8, height = 5)

# Cumulative % matched by rank position by high-performing status
rank_by_performance <- assigned_rank_df %>%
  filter(!is.na(assigned_rank)) %>%
  count(high_performance_student, assigned_rank) %>%
  left_join(
    assigned_rank_df %>% count(high_performance_student, name = "n_total"),
    by = "high_performance_student"
  ) %>%
  arrange(high_performance_student, assigned_rank) %>%
  mutate(
    cum_pct = cumsum(n) / n_total * 100,
    Performance = if_else(high_performance_student == 1, "High-Performing", "Other"),
    .by = high_performance_student
  )

p_performance <- ggplot(rank_by_performance, aes(x = assigned_rank, y = cum_pct, color = Performance)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1, max(rank_by_performance$assigned_rank))) +
  scale_y_continuous(limits = c(min(rank_by_performance$cum_pct), max(rank_by_performance$cum_pct))) +
  scale_color_manual(values = c("High-Performing" = "#FFBE6A", "Other" = "#40B0A6")) +
  labs(
    x = "Rank Position",
    y = "Cumulative % Matched",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave("DataGeneration/Chile/outputs/cum_rank_by_performance.pdf", p_performance, width = 8, height = 5)

# 2.) Average Rank, Probability (un)matched by
# Sex
# Priority Student
# High-Performing Student

welfare_stats <- assigned_rank_df %>%
  pivot_longer(
    cols = c(female, priority_student, high_performance_student),
    names_to = "variable",
    values_to = "category"
  ) %>%
  mutate(
    variable = recode(variable,
      female                   = "Female",
      priority_student         = "Priority Student",
      high_performance_student = "High-Performing Student"
    ),
    category = recode(as.character(category), `0` = "No", `1` = "Yes")
  ) %>%
  group_by(variable, category) %>%
  summarise(
    n               = n(),
    avg_rank        = mean(assigned_rank, na.rm = TRUE),
    pct_matched     = mean(!is.na(assigned_rank)) * 100,
    pct_unmatched   = mean(is.na(assigned_rank)) * 100,
    .groups = "drop"
  )

# LaTeX export
print(
  xtable(welfare_stats, digits = c(0, 0, 0, 0, 2, 1, 1)),
  include.rownames = FALSE
)

# 3.) Welfare metrics by region of residence

matching_outcome_by_region_cum <- matching_outcome_by_region %>%
  mutate(
    pct_top_1 = pct_top1,
    pct_top_1_3 = pct_top1 + pct_top2 + pct_top3,
    pct_top_1_5 = rowSums(across(pct_top1:pct_top5), na.rm = TRUE),
    pct_top_1_10 = rowSums(across(pct_top1:pct_top10), na.rm = TRUE),
    pct_top_1_12 = rowSums(across(pct_top1:pct_top12), na.rm = TRUE),
    pct_matched = 100 - pct_unmatched
  ) %>%
  select(
    Region, n_students,
    pct_top_1, pct_top_1_3, pct_top_1_5,
    pct_top_1_10, pct_top_1_12,
    pct_matched, pct_unmatched
  )

# LateX export
latex_table <- xtable(
  matching_outcome_by_region_cum,
  digits = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1)  # 1 decimal for percentages
)

print(
  latex_table,
  include.rownames = FALSE,
)
