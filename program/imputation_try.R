.libPaths("/media/external/s25_5/Rlibs")
library(mlr3)
library(mlr3pipelines)
library(ranger)
library(dplyr)
library(tidyr)
library(mlr3learners)
set.seed(7832)
lgr::get_logger("mlr3")$set_threshold("warn")

train_all_clean <- readRDS("data/fi_clean.rds") %>%
  filter(dataset == "train")

#select cols for training, use region_code
train_model <- train_all_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage,-district_code, -region,-dataset,-year_recorded, -month_recorded
         ,-region_code) %>%
  mutate(population = ifelse(population == 0, NA, population))
#find missing values
missing_summary <- function(df) {
  miss_pct <- sapply(df, function(x) mean(is.na(x)))
  miss_pct[miss_pct > 0] %>% sort(decreasing = TRUE)
}
missing_summary(train_model)
#
train_model <- train_model %>%
  mutate(
    public_meeting = as.factor(public_meeting),
    permit = as.factor(permit),
    region_district = as.factor(region_district),
    across(where(is.character), as.factor)
  )

#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model,
  target = "status_group"
)
print(task)

###imputation
#select cols
years_features <- c("region_district",
                    "extraction_type",
                    "scheme_management",
                    "waterpoint_type")

population_features <- c(
  "region_district",
  "management",
  "waterpoint_type",
  "extraction_type"
)

gps_features <- c(
  "region_district"
)
#
ranger_learner <- lrn("regr.ranger", num.trees = 50, max.depth =6)


#imputation rules
imp_years <-
  po("select", id = "sel_years", selector = selector_name(years_features)) %>>%
  po("imputelearner", id = "imp_years", learner = ranger_learner$clone(deep = TRUE),
     affect_columns = selector_name("years_in_use"))
imp_pop <-
  po("select", id = "sel_pop", selector = selector_name(population_features)) %>>%
  po("imputelearner", id = "imp_pop", learner = ranger_learner$clone(deep = TRUE),
     affect_columns = selector_name("population"))
imp_gps <-
  po("select", id = "sel_gps", selector = selector_name(gps_features)) %>>%
  po("imputelearner", id = "imp_gps", learner = ranger_learner$clone(deep = TRUE),
     affect_columns = selector_name("gps_height"))
imp_scheme <- po("imputemode", id = "imp_scheme", affect_columns = selector_name("scheme_management"))
imp_public <- po("imputemode", id = "imp_public", affect_columns = selector_name("public_meeting"))
imp_permit <- po("imputemode", id = "imp_permit", affect_columns = selector_name("permit"))
imp_installer <- po("imputemode", id = "imp_installer", affect_columns = selector_name("installer"))
imp_funder <- po("imputemode", id = "imp_funder", affect_columns = selector_name("funder"))
#
impute_graph <-
  imp_years %>>%
  imp_pop %>>%
  imp_gps %>>%
  imp_scheme %>>%
  imp_public %>>%
  imp_permit %>>%
  imp_installer %>>%
  imp_funder

#
base_learner <- lrn("classif.ranger", predict_type = "prob")
graph <- impute_graph %>>% base_learner
graph_learner = as_learner(graph)
#run 3fold cv
rr <- resample(task, graph_learner, rsmp("cv", folds = 3))
acc_learner <- rr$aggregate()
saveRDS(acc_learner, file = "/media/external/s25_5/result/imp_learner.rds")


###median+mode
train_all_clean <- readRDS("data/train_all_clean.rds")
#select cols for training, use region_code
train_model <- train_all_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage,-district_code, -region,-dataset,-year_recorded, -month_recorded
         ,-region_code)
train_model_copy <- train_model

#write a function for getting mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#here we should edit: get mode for scheme_management, public_meeting, permit,installer and funder,
#and mutate,also factorize
train_model_copy <- train_model_copy %>%
  mutate(
    population = replace_na(population, median(population, na.rm = TRUE)),
    years_in_use = replace_na(years_in_use, median(years_in_use, na.rm = TRUE)),
    gps_height = replace_na(gps_height, median(gps_height, na.rm = TRUE)),
    scheme_management = replace_na(scheme_management, get_mode(scheme_management)),
    public_meeting = as.factor(public_meeting),
    public_meeting = replace_na(public_meeting, get_mode(public_meeting)),
    permit = as.factor(permit),
    permit = replace_na(permit, get_mode(permit)),
    installer = replace_na(installer, get_mode(installer)),
    funder = replace_na(funder, get_mode(funder)),
    gps_height = replace_na(gps_height, median(gps_height, na.rm = TRUE)),
    across(where(is.character), as.factor))

#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model_copy,
  target = "status_group"
)
#
learner <- lrn("classif.ranger", predict_type = "prob")
rr <- resample(task, learner, rsmp("cv", folds = 3))
result2 <- rr$aggregate()
saveRDS(result2, file = "/media/external/s25_5/result/imp_median_mode.rds")


###mean+mode
train_model_copy <- train_model
train_model_copy <-train_model_copy %>%
  mutate(
    population = replace_na(population, mean(population, na.rm = TRUE)),
    years_in_use = replace_na(years_in_use, mean(years_in_use, na.rm = TRUE)),
    gps_height = replace_na(gps_height, mean(gps_height, na.rm = TRUE)),
    scheme_management = replace_na(scheme_management, as.factor("unknown")),
    public_meeting = as.factor(public_meeting),
    public_meeting = replace_na(public_meeting, "FALSE"),
    permit = as.factor(permit),
    permit = replace_na(permit, "FALSE"),
    installer = replace_na(installer, get_mode(installer)),
    funder = replace_na(funder, get_mode(funder)),
    across(where(is.character), as.factor))

#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model,
  target = "status_group"
)
#
learner <- lrn("classif.ranger", predict_type = "prob")
rr <- resample(task, learner, rsmp("cv", folds = 3))
result3 <- rr$aggregate()
saveRDS(result3, file = "/media/external/s25_5/result/imp_mean_mode.rds")


###delete all columns with missing values
train_model_copy <- train_model
train_model_copy <- train_model_copy %>%
  select(-c(population, years_in_use, scheme_management, public_meeting, permit,gps_height)) %>%
  mutate(across(where(is.character), as.factor))
#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model_copy,
  target = "status_group"
)
#
learner <- lrn("classif.ranger", predict_type = "prob")
rr <- resample(task, learner, rsmp("cv", folds = 3))
result4 <- rr$aggregate()
saveRDS(result4, file = "/media/external/s25_5/result/imp_delete.rds")
