###contributed by:Gelan Ye, coeidted: Haoran Ju

train_all_clean <- readRDS("data/train_all_clean.rds")

#find the numeric_vars
numeric_vars <- c("population", "gps_height", "years_in_use")

# installer
train_all_clean$installer_missing <- is.na(train_all_clean$installer)

cat_installer <- chisq_missingness(train_all_clean, "installer_missing", exclude_vars = c("installer"))
print(cat_installer[cat_installer < 0.05])

t_installer <- t_test_missingness(train_all_clean, "installer_missing", numeric_vars)
print(t_installer)

missingness_map(train_all_clean, "installer_missing", "Installer Missingness Map")


#funder
train_all_clean$funder_missing <- is.na(train_all_clean$funder)

cat_funder <- chisq_missingness(train_all_clean, "funder_missing", exclude_vars = c("funder"))
print(cat_funder[cat_funder < 0.05])

t_funder <- t_test_missingness(train_all_clean, "funder_missing", numeric_vars)
print(t_funder)

missingness_map(train_all_clean, "funder_missing", "Funder Missingness Map")


# gps_height
train_all_clean$gps_missing <- train_all_clean$gps_height == 0

cat_gps <- chisq_missingness(train_all_clean, "gps_missing", exclude_vars = c("gps_height"))
print(cat_gps[cat_gps < 0.05])

t_gps <- t_test_missingness(train_all_clean, "gps_missing", numeric_vars)
print(t_gps)

missingness_map(train_all_clean, "gps_missing", "GPS Height Missingness Map")


#years_in_use
train_all_clean$years_in_use_missing <- is.na(train_all_clean$years_in_use)

cat_constr <- chisq_missingness(train_all_clean, "years_in_use_missing", exclude_vars = c("years_in_use"))
print(cat_constr[cat_constr < 0.05])

t_constr <- t_test_missingness(train_all_clean, "years_in_use_missing", numeric_vars)
print(t_constr)

missingness_map(train_all_clean, "years_in_use_missing","Used Year Missingness Map")


#population
train_all_clean$population_missing <- train_all_clean$population == 0

cat_pop <- chisq_missingness(train_all_clean, "population_missing", exclude_vars = c("population"))
print(cat_pop[cat_pop < 0.05])

t_pop <- t_test_missingness(train_all_clean, "population_missing", numeric_vars)
print(t_pop)

missingness_map(train_all_clean, "population_missing", "Population Missingness Map")



