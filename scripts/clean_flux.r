library(tidyverse)

df <- read_csv("data/clean_flux.csv") %>%
  clean_names() %>%
  rename(date = year_month_day)

trt <- read_csv("data/metadata/plot_treatments.csv") %>%
  clean_names() %>%
  # add leading zero to plot numbers
  mutate(plot = str_pad(plot, width = 2, side = "left", pad = "0")) %>%
  select(!growing_season)

# compare columns
compare_df_cols(df, trt)

trt_2023 <- trt %>%
  filter(date == "2023-07-01")

trt_2024 <- trt %>%
  filter(date == "2024-07-01")

trt_year <- bind_rows(
  trt_2023 %>% mutate(year = 2023),
  trt_2024 %>% mutate(year = 2024)
) %>%
  select(plot, year, treatment)

df <- df %>%
  mutate(
    plot = str_pad(plot, width = 2, pad = "0"),
    year = year(date)
  ) %>%
  left_join(trt_year, by = c("plot", "year"))

head(df)
summary(df)
table(df$treatment, useNA = "ifany")
df %>% count(year, plot)
df %>% filter(is.na(treatment))
anti_join(df,
  trt_2023 %>% mutate(year = 2023) %>% select(plot, year),
  by = c("plot", "year")
)
anti_join(df,
  trt_2024 %>% mutate(year = 2024) %>% select(plot, year),
  by = c("plot", "year")
)

# sanity checks
table(df$treatment, useNA = "ifany")
range(df$date, na.rm = TRUE)

df %>%
  filter(!is.na(treatment)) %>%
  group_by(plot, rowvs_interrow, treatment, date) %>%
  summarise(n = sum(!is.na(gnha_day_no_negative)), .groups = "drop") %>%
  count(n) %>%
  arrange(n)
