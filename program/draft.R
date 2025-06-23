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

# modify extraction_type, delete extraction_type_class, extraction_type_group
# data_all_clean <- data_all_clean %>%
#   mutate(
#     extraction_type = if_else(
#       extraction_type == "other - mkulima/shinyanga",
#       "other",
#       extraction_type)
#   ) %>%
#   mutate(
#     extraction_type = case_when(
#       str_detect(extraction_type, regex("^india mark (ii|iii)$", ignore_case = TRUE)) 
#       ~ "india mark",
#       str_detect(extraction_type, regex("^other - ", ignore_case = TRUE)) 
#       ~ str_remove(extraction_type, regex("^other - ", ignore_case = TRUE)),
#       TRUE ~ extraction_type
#     )
#   )

mapping_extraction <- unique(data_all_clean[, c("extraction_type", 
                                                "extraction_type_group", 
                                                "extraction_type_class")])

data_all_clean <- data_all_clean %>%
  mutate(
    extraction_type_other = case_when(
      extraction_type %in% c("india mark ii", "india mark iii") ~ "india mark",
      extraction_type == "ksb" ~ "ksb",
      extraction_type == "other - rope pump" ~ "other rope pump",
      TRUE ~ extraction_type_group
    )
  )

train3 <<- data_all_clean
train3 <- train3 %>% filter(dataset == "train") %>% select(-id, -quantity_group, 
                                                           -recorded_by, -amount_tsh,
                                                           -wpt_name, num_private, -dataset,
                                                           -subvillage, -ward)
train3[] <- lapply(train3, function(x) if (is.character(x)) as.factor(x) else x)
train3_raw <- train3 %>% select(-extraction_type_class, -extraction_type_group, -extraction_type_other)
train3_grouped <- train3 %>% select(-extraction_type, -extraction_type_group, -extraction_type_other)
train3_other <- train3 %>% select(-extraction_type, -extraction_type_class, -extraction_type_group)
train3_classed <- train3 %>% select(-extraction_type, -extraction_type_class, -extraction_type_other)

task_raw     <- TaskClassif$new(id = "raw", backend = train3_raw, target = "status_group")
task_grouped <- TaskClassif$new(id = "grouped", backend = train3_grouped, target = "status_group")
task_other <- TaskClassif$new(id = "other", backend = train3_other, target = "status_group")
task_classed <- TaskClassif$new(id = "classed", backend = train3_classed, target = "status_group")

library(mlr3pipelines)
graph <- po("encode", method = "treatment") %>>% lrn("classif.xgboost")
learner <- GraphLearner$new(graph)
resampling <- rsmp("cv", folds = 5)

design <- benchmark_grid(
  tasks = list(task_raw, task_grouped, task_classed, task_other),
  learners = list(learner),
  resamplings = list(resampling)
)

bmr <- benchmark(design)

bmr$aggregate(msr("classif.acc"))

# keep management, delete management_group
mapping_management <- unique(data_all_clean[, c("management", "management_group")])

train3 <<- data_all_clean

train3 <- train3 %>%
  mutate(
    management_other = case_when(
      str_detect(management, regex("^other - ", ignore_case = TRUE)) ~
        str_remove(management, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ management
    )
  )

unique(train3[, c("management", "management_group", "management_other")])

train3 <- train3 %>% filter(dataset == "train") %>% select(-id, -quantity_group, 
                                                           -recorded_by, -amount_tsh,
                                                           -wpt_name, num_private, -dataset,
                                                           -subvillage, -ward, -extraction_type, 
                                                           -extraction_type_group, -extraction_type_other)
train3[] <- lapply(train3, function(x) if (is.character(x)) as.factor(x) else x)
train3_raw <- train3 %>% select(-management_group, -management_other)
train3_grouped <- train3 %>% select(-management, -management_other)
train3_other <- train3 %>% select(-management_other, -management_group)

task_raw     <- TaskClassif$new(id = "raw", backend = train3_raw, target = "status_group")
task_grouped <- TaskClassif$new(id = "grouped", backend = train3_grouped, target = "status_group")
task_other <- TaskClassif$new(id = "other", backend = train3_other, target = "status_group")

learner <- lrn("classif.lightgbm")
resampling <- rsmp("cv", folds = 5)
design <- benchmark_grid(
  tasks = list(task_raw, task_grouped, task_other),
  learners = list(learner),
  resamplings = list(resampling)
)
bmr <- benchmark(design)
bmr$aggregate(msr("classif.acc"))

unique(train3[, c("scheme_management", "scheme_name")])
n_distinct(train3$scheme_management)

train3 <- train3 %>% select(-management_group, -management_other)
colnames(train3)
run_feature_set_benchmark <- function(data, target_col, feature_sets, learner_id = "classif.lightgbm", folds = 5) {
  # Step 1: Ensure all character columns are converted to factors
  data[] <- lapply(data, function(x) if (is.character(x)) as.factor(x) else x)
  
  # Step 2: Construct classification tasks
  tasks <- lapply(seq_along(feature_sets), function(i) {
    features <- feature_sets[[i]]
    task_data <- data[, c(features, target_col)]
    TaskClassif$new(id = paste0("task_", i), backend = task_data, target = target_col)
  })
  
  # Step 3: Setup learner and resampling
  learner <- lrn(learner_id, predict_type = "response")
  resampling <- rsmp("cv", folds = folds)
  
  # Step 4: Create benchmark grid and run
  design <- benchmark_grid(
    tasks = tasks,
    learners = list(learner),
    resamplings = list(resampling)
  )
  
  bmr <- benchmark(design)
  accs <- bmr$aggregate(msr("classif.acc"))
  return(accs)
}

scheme_raw <- setdiff(names(train3), c("scheme_management", "status_group"))
scheme_grouped <- setdiff(names(train3), c("scheme_name", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(scheme_raw, scheme_grouped)
)

train3 <- train3 %>% select(-scheme_name)
colnames(train3)
n_distinct(train3$payment)
n_distinct(train3$payment_type)
unique(train3[, c("payment", "payment_type")])


train3 <- train3 %>% select(-payment)
colnames(train3)
n_distinct(train3$water_quality)
n_distinct(train3$quality_group)
unique(train3[, c("water_quality", "quality_group")])
quality_raw <- setdiff(names(train3), c("quality_group", "status_group"))
quality_grouped <- setdiff(names(train3), c("water_quality", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(quality_raw, quality_grouped)
)

train3 <- train3 %>% select(-water_quality)
colnames(train3)
n_distinct(train3$source)
n_distinct(train3$source_type)
n_distinct(train3$source_class)
unique(train3[, c("source", "source_type", "source_class")])
source_raw <- setdiff(names(train3), c("source_type", "source_class", "status_group"))
source_type <- setdiff(names(train3), c("source", "source_class", "status_group"))
source_class <- setdiff(names(train3), c("source_type", "source", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(source_raw, source_type, source_class)
)

train3 <- train3 %>% select(-source, -source_class)
colnames(train3)
n_distinct(train3$waterpoint_type)
n_distinct(train3$waterpoint_type_group)
unique(train3[, c("waterpoint_type", "waterpoint_type_group")])
waterpoint_raw <- setdiff(names(train3), c("waterpoint_type_group", "status_group"))
waterpoint_group <- setdiff(names(train3), c("waterpoint_type", "status_group"))

run_feature_set_benchmark(
  data = train3,
  target_col = "status_group",
  feature_sets = list(waterpoint_raw, waterpoint_group)
)

train3 <- train3 %>% select(-waterpoint_type)
colnames(train3)

data_all_clean <- data_all_clean %>%
  mutate(
    management_other = case_when(
      str_detect(management, regex("^other - ", ignore_case = TRUE)) ~
        str_remove(management, regex("^other - ", ignore_case = TRUE)),
      TRUE ~ management
    )
  )
