# CREATE DATA FOR FIRMS & MARKETS GROUP PROJECT ----

# 0. Setup ----
library(rvest)
library(tidyverse)

working_dir        <- fs::path(here::here(), "project")
raw_data_dir       <- fs::path(working_dir, "data", "raw")
processed_data_dir <- fs::path(working_dir, "data", "processed")

# 1. Data ----
bli_tbl <- read_csv(fs::path(raw_data_dir, "BLI.csv"))%>%
  janitor::clean_names()

broadband_tbl <- read_csv(fs::path(raw_data_dir, "BROADBAND_DB.csv")) %>%
  janitor::clean_names()

pricing_tbl <- readxl::read_excel(fs::path(raw_data_dir, "global-broadband-pricing-study-2020.xlsx"),
                                  sheet = "2020 (Current)",
                                  skip = 1) %>% janitor::clean_names()

# 2. Better Life Index ----

bli_vars <- c(
  "Long-term unemployment rate",
  "Years in education",
  "Educational attainment",
  "Personal earnings",
  "Life expectancy",
  "Life satisfaction"
)

bli_processed_tbl <- bli_tbl %>%
  filter(inequality_2 == "Total", indicator_2 %in% bli_vars) %>%
  select(location, country, indicator, indicator_2, unit, value) %>%
  mutate(col_label = str_glue("{indicator_2} ({str_to_lower(unit)})")) %>%
  select(location, country, col_label, value) %>%
  pivot_wider(
    names_from = col_label,
    values_from = value
  ) %>%
  janitor::clean_names()

# 3. Broadband DB ----

variables <- c(
  "Total fixed broadband suscriptions",
  "Total fixed broadband suscriptions per 100 inhabitants",
  "Total mobile broadband subscriptions",
  "Total mobile broadband subscriptions per 100 inhabitants",
  "Mobile data usage per mobile broadband subscription, GB per month"
)

broadband_processed_tbl <- broadband_tbl %>%
  # filter(str_length(time) == 4) %>%
  # arrange(location, variable, desc(time)) %>%
  # group_by(location, variable) %>%
  # mutate(rn = row_number()) %>% 
  # ungroup() %>%
  # filter(rn == 1) %>%
  filter(time == "2018", location != "OECD", variable %in% variables) %>%
  select(location, country, variable, time, unit, value) %>%
  mutate(col_label = str_glue("{variable})")) %>%
  select(location, country, col_label, value) %>%
  pivot_wider(
    names_from = col_label,
    values_from = value
  ) %>%
  janitor::clean_names()


# 4. Global Pricing Study ----

pricing_processed_tbl <- pricing_tbl %>%
  select(
    country_code, name, packages_measured,
    average_cost_of_a_fixed_line_broadband_package_per_month_in_usd,
    average_cost_of_fixed_line_broadband_per_megabit_per_month_in_usd
  )

# 5. Median Income ----

wiki_url_income <- "https://en.wikipedia.org/wiki/List_of_countries_by_average_wage"

median_income_tbl <-
  read_html(wiki_url_income) %>%
  html_nodes(xpath = '//*[@id="2018"]') %>%
  html_table() %>%
  .[[1]] %>% 
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(ppp_usd     = parse_number(ppp),
         nominal_usd = parse_number(nominal)) %>%
  select(-ppp, -nominal)

# 6. Speed Comparison ----

wiki_url_speed <- "https://en.wikipedia.org/wiki/List_of_countries_by_Internet_connection_speeds"

speed_tbl <-
  read_html(wiki_url_speed) %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]/tbody/tr[2]/td[1]/table') %>%
  html_table() %>%
  .[[1]] %>% 
  as_tibble() %>%
  janitor::clean_names() %>%
  select(-rank)

# 7. Urbanization ----

wiki_url_urban <- "https://en.wikipedia.org/wiki/Urbanization_by_country"

urban_tbl <-
  read_html(wiki_url_urban) %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table() %>%
  .[[1]] %>% 
  as_tibble() %>%
  janitor::clean_names() %>%
  select(nation, urban_population_percent) %>%
  mutate(urban_population_percent = parse_number(urban_population_percent))

# 8. Full Dataset ----

country_codes <- 
  read_html("https://www.worldatlas.com/aatlas/ctycodes.htm") %>%
  html_nodes(xpath = '//*[@id="container"]/main/div/article/div[1]/table') %>%
  html_table(header = F) %>%
  .[[1]] %>% 
  as_tibble() %>%
  slice(2:nrow(.)) %>%
  select(country = X1, code_2 = X2, code_3 = X3)

# analytics_base_tbl <- broadband_processed_tbl %>%
#   left_join(
#     bli_processed_tbl
#   ) %>%
#   left_join(
#     median_income_tbl
#   ) %>%
#   left_join(
#     pricing_processed_tbl,
#     by = c("country" = "name")
#   )

analytics_base_tbl <- country_codes %>%
  left_join(
    broadband_processed_tbl,
    by = c("code_3" = "location"),
    suffix = c("", "_drop")
  ) %>%
  left_join(
    bli_processed_tbl,
    by = c("code_3" = "location"),
    suffix = c("", "_drop")
  ) %>%
  left_join(
    pricing_processed_tbl,
    by = c("code_2" = "country_code"),
    suffix = c("", "_drop")
  ) %>% 
  left_join(
    median_income_tbl,
    by = c("country_drop" = "country"),
    suffix = c("", "_drop")
  ) %>% 
  left_join(
    speed_tbl,
    by = c("country_drop" = "country_territory"),
    suffix = c("", "_drop")
  ) %>% 
  left_join(
    urban_tbl,
    by = c("country_drop" = "nation"),
    suffix = c("", "_drop")
  ) %>% 
  select(-contains("_drop"), -name, -nominal_usd) %>%
  drop_na(total_fixed_broadband_suscriptions_per_100_inhabitants)


write_csv(analytics_base_tbl, fs::path(processed_data_dir, "countries_comparison.csv"))
