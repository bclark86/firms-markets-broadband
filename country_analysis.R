# 0. Setup ----
library(DataExplorer)
library(recipes)
library(tidyverse)
library(fs)


working_dir        <- fs::path(here::here())
processed_data_dir <- fs::path(working_dir, "data", "processed")

# 1. Data ----
countries_raw_tbl <- 
  read_csv(fs::path(processed_data_dir, "countries_comparison.csv"))

# 2. Explore ----
countries_raw_tbl %>% DataExplorer::profile_missing()

countries_raw_tbl %>% DataExplorer::plot_missing()

countries_raw_tbl %>% DataExplorer::plot_correlation()

keep_vars <- countries_raw_tbl %>% 
  DataExplorer::profile_missing() %>%
  filter(num_missing <= 5) %>%
  pull(feature)


countries_tbl <- countries_raw_tbl %>%
  select(keep_vars) %>%
  select(-contains("code"),
         -total_fixed_broadband_suscriptions,
         -total_mobile_broadband_subscriptions,
         -packages_measured) 

# 2. Pre-Processing ----
country_rec <- countries_tbl %>%
  recipes::recipe(~., data = .) %>%
  update_role(country, new_role = "id") %>%
  # min-max scaling [0,1]
  step_range(all_numeric()) %>%
  # replace N/As with the median
  step_medianimpute(all_numeric()) %>%
  prep()

country_processed_tbl <- 
  bake(country_rec, new_data = countries_tbl)

# 3. Scoring ----
country_rank_tbl <- country_processed_tbl %>%
  mutate(
    # reverse the score for cost
    average_cost_of_a_fixed_line_broadband_package_per_month_in_usd = 1 - 
      average_cost_of_a_fixed_line_broadband_package_per_month_in_usd,
    average_cost_of_fixed_line_broadband_per_megabit_per_month_in_usd = 1 - 
      average_cost_of_fixed_line_broadband_per_megabit_per_month_in_usd
  ) %>%
  mutate(
    # broadband quality
    score = rowSums(
      select(., contains("broadband"), contains("speed"))) /
      length(select(., contains("broadband"), contains("speed")))
    ) %>% 
  arrange(desc(score)) %>%
  mutate(rank = row_number()) %>%
  select(country, rank, score, contains("broadband"), contains("speed")) %>%
  left_join(
    countries_tbl,
    by = "country",
    suffix = c("_idx", "")
  ) %>%
  mutate_at(
    select(., contains("idx"), score) %>% names(),
    function(x) round(x * 100, 0)
  ) %>%
  mutate(quartile = 5 - ntile(score, 4)) %>%
  relocate(quartile, .after = score)

country_rank_tbl %>% glimpse()

country_rank_tbl %>%
  group_by(quartile) %>%
  summarize_at(.vars = c("personal_earnings_us_dollar",
                         "long_term_unemployment_rate_percentage",
                         "life_expectancy_years",
                         "educational_attainment_percentage",
                         "life_satisfaction_average_score"),
               .funs = function(x) mean(x, na.rm = T))

write_csv(country_rank_tbl, fs::path(processed_data_dir, "countries_rank.csv"))

