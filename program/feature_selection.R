source("setup.R")

fi_clean <- readRDS("data/fi_clean.rds")

###cleaning other columns
#contributed by Yuxin Liu
data_all_clean <- data_all_clean %>%
  mutate(
    year_recorded = as.integer(format(date_recorded, "%Y")),
    month_recorded = as.integer(format(date_recorded, "%m")),
    status_group = as.factor(status_group)
  ) %>%
  select(-date_recorded)

# years_in_use (year_recorded - construction_year)
# delete construction_year and day_recorded
# keep month_recorded
data_all_clean <- data_all_clean %>%
  mutate(
    construction_year = na_if(construction_year, 0),
    years_in_use = if_else(
      year_recorded - construction_year >= 0,
      year_recorded - construction_year,
      as.numeric(NA)
    )
  )