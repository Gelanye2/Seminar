########any ideas can be saved here.

###1 Idea of preprocessing and variable selection

#step 1:quick preselection emperical knowledge?
sapply(train_all, function(x) length(unique(x)))
sapply(train_clean, function(x) sum(is.na(x)))

table_funder <- train_clean %>%
  count(funder, sort = TRUE)

#step2: statistical test (but need to deal with NA values first)
sapply(train_clean, class)

#step3:simple models to test feature importance

###2 choose rd or lga or ward
data_model <- data_all_clean %>%
  filter(!is.na(status_group)) %>%
  mutate(
    rd_id = as.integer(as.factor(region_district)),
    lga_id = as.integer(as.factor(lga)),
    ward_id = as.integer(as.factor(ward)),
    y = as.integer(as.factor(status_group)) - 1
  )

X <- data_model[, c("rd_id", "lga_id", "ward_id")]
y <- data_model$y

bst <- xgboost(data = as.matrix(X), label = y, nrounds = 20, objective = "multi:softprob", num_class = 3, verbose = 0)
xgb.importance(model = bst)





#####
set.seed(7832)
lgr::get_logger("mlr3")$set_threshold("warn")

train_all_clean <- readRDS("data/train_all_clean.rds")
#

#select cols for training
train_model <- train_all_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage,-installer,-funder,-district_code, -region,-dataset,-year_recorded, -month_recorded
         ,-extraction_type_group, -waterpoint_type_group,-region_district,-region_code)
train_model$gps_height_missing <- is.na(train_model$gps_height)
train_model$population_missing <- is.na(train_model$population)
train_model$years_in_use_missing <- is.na(train_model$years_in_use)
#convert character to factor
train_model <- train_model %>%
  mutate(across(where(is.character), as.factor))
#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model,
  target = "status_group"
)
print(task)

#find missing values
missing_summary <- function(df) {
  miss_pct <- sapply(df, function(x) mean(is.na(x)))
  miss_pct[miss_pct > 0] %>% sort(decreasing = TRUE)
}
missing_summary(train_model)

#autoplot
autoplot(task$clone()$select(c("gps_height", "population","years_in_use")), type = "pairs")

###imputation
#select cols
years_features <- c(
                    "extraction_type",
                    "scheme_management",
                    "waterpoint_type_group",
                    "gps_height_missing",
                    "population_missing")


population_features <- c(
  "management",
  "waterpoint_type",
  "extraction_type",
  "source_class"
)

ranger_learner <- lrn("regr.ranger", num.trees = 5, max.depth =2)

#imputation rules
imp_years <-
  po("select", id = "sel_years", selector = selector_name(years_features)) %>>%
  po("imputelearner", id = "imp_years", learner = ranger_learner$clone(deep = TRUE),
     affect_columns = selector_name("years_in_use"))

imp_pop <-
  po("select", id = "sel_pop", selector = selector_name(population_features)) %>>%
  po("imputelearner", id = "imp_pop", learner = ranger_learner$clone(deep = TRUE),
     affect_columns = selector_name("population"))

imp_scheme <- po("imputemode", id = "imp_scheme", affect_columns = selector_name("scheme_management"))
imp_public <- po("imputemode", id = "imp_public", affect_columns = selector_name("public_meeting"))
imp_permit <- po("imputemode", id = "imp_permit", affect_columns = selector_name("permit"))

impute_graph <-
  imp_years %>>%
  imp_pop %>>%
  imp_scheme %>>%
  imp_public %>>%
  imp_permit

#
base_learner <- lrn("classif.rpart", predict_type = "prob")
graph <- impute_graph %>>% base_learner
graph_learner = as_learner(graph)
rr = resample(task, graph_learner, rsmp("holdout"))
rr$aggregate()

na_cols <- names(train_model)[sapply(train_model, function(x) any(is.na(x)))]
train_model_dropped <- train_model %>% select(-all_of(na_cols))
task_dropped <- TaskClassif$new(
  id = "waterpoints_dropna",
  backend = train_model_dropped,
  target = "status_group"
)
rr_drop <- resample(task_dropped, learner, rsmp("holdout"))
rr_drop$aggregate()


