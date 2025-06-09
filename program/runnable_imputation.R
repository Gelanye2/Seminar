###contributed by: Haoran Ju, Gelan YeAdd commentMore actions
set.seed(7832)
lgr::get_logger("mlr3")$set_threshold("warn")

train_all_clean <- readRDS("data/train_all_clean.rds")

#select cols for training, use region_code
train_model <- train_all_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage,-installer,-funder,-district_code, -region,-dataset,-year_recorded, -month_recorded
         ,-waterpoint_type_group,-region_district)
train_model$population_missing <- is.na(train_model$population)
train_model$years_in_use_missing <- is.na(train_model$years_in_use)
#
train_model <- train_model %>%
  mutate(
    public_meeting = as.factor(public_meeting),
    permit = as.factor(permit),
    region_code = as.factor(region_code),
    across(where(is.character), as.factor)
  )

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

# autoplot
autoplot(task$clone()$select(c("years_in_use","population")),
         type = "pairs")

###imputation
#select cols
years_features <- c("region_code",
                    "extraction_type",
                    "scheme_management",
                    "waterpoint_type")

population_features <- c(
  "region_code",
  "management",
  "waterpoint_type",
  "extraction_type",
  "source_class"
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
imp_scheme <- po("imputemode", id = "imp_scheme", affect_columns = selector_name("scheme_management"))
imp_public <- po("imputemode", id = "imp_public", affect_columns = selector_name("public_meeting"))
imp_permit <- po("imputemode", id = "imp_permit", affect_columns = selector_name("permit"))
#
impute_graph <-
  imp_years %>>%
  imp_pop %>>%
  imp_scheme %>>%
  imp_public %>>%
  imp_permit

#
base_learner <- lrn("classif.rpart")
graph <- impute_graph %>>% base_learner
graph_learner = as_learner(graph)
#run 3fold cv
rr <- resample(task, graph_learner, rsmp("cv", folds = 3))
rr$aggregate()


###median+mode
train_all_clean <- readRDS("data/train_all_clean.rds")
#select cols for training, use region_code
train_model <- train_all_clean %>%
  select(-longitude, -latitude, -lga, -ward, -subvillage,,-district_code, -region,-dataset,-year_recorded, -month_recorded
         , -waterpoint_type_group,-region_district,-installer,-funder)
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
    scheme_management = replace_na(scheme_management, get_mode(scheme_management)),
    public_meeting = as.factor(public_meeting),
    public_meeting = replace_na(public_meeting, get_mode(public_meeting)),
    permit = as.factor(permit),
    permit = replace_na(permit, get_mode(permit)),
    across(where(is.character), as.factor))

#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model_copy,
  target = "status_group"
)
#
learner <- lrn("classif.rpart")
rr <- resample(task, learner, rsmp("cv", folds = 3))
rr$aggregate()


###mean+mode
train_model_copy <- train_model
train_model_copy <- train_model_copy %>%
  mutate(
    population = replace_na(population, mean(population, na.rm = TRUE)),
    years_in_use = replace_na(years_in_use, mean(years_in_use, na.rm = TRUE)),
    scheme_management = replace_na(scheme_management, get_mode(scheme_management)),
    public_meeting = as.factor(public_meeting),
    public_meeting = replace_na(public_meeting, get_mode(public_meeting)),
    permit = as.factor(permit),
    permit = replace_na(permit, get_mode(permit)),
    across(where(is.character), as.factor))

#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model_copy,
  target = "status_group"
)
#
learner <- lrn("classif.rpart")
rr <- resample(task, learner, rsmp("cv", folds = 3))
rr$aggregate()


###delete all columns with missing values
train_model_copy <- train_model
train_model_copy <- train_model_copy %>%
  select(-c(population, years_in_use, scheme_management, public_meeting, permit)) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.logical), as.factor))
#create task
task <- TaskClassif$new(
  id = "waterpoints",
  backend = train_model_copy,
  target = "status_group"
)
#
learner <- lrn("classif.rpart")
rr <- resample(task, learner, rsmp("cv", folds = 3))
rr$aggregate()


#######choose median+mode as imputing method(temporary)
train_all_clean <- readRDS("data/fi_clean.rds") %>%
  filter(dataset == "train")
train_model <- train_all_clean %>%
  mutate(
    population = replace_na(population, median(population, na.rm = TRUE)),
    years_in_use = replace_na(years_in_use, median(years_in_use, na.rm = TRUE)),
    scheme_management = replace_na(scheme_management, get_mode(scheme_management)),
    public_meeting = as.factor(public_meeting),
    public_meeting = replace_na(public_meeting, get_mode(public_meeting)),
    permit = as.factor(permit),
    permit = replace_na(permit, get_mode(permit)),
    installer = replace_na(installer, get_mode(installer)),
    funder = replace_na(funder, get_mode(funder)),
    across(where(is.character), as.factor))
saveRDS(train_model, "data/train_imputed.rds")

test_model <- readRDS("data/fi_clean.rds") %>%
  filter(dataset == "test") %>%
  mutate(
    population = replace_na(population, median(population, na.rm = TRUE)),
    years_in_use = replace_na(years_in_use, median(years_in_use, na.rm = TRUE)),
    scheme_management = replace_na(scheme_management, get_mode(scheme_management)),
    public_meeting = as.factor(public_meeting),
    public_meeting = replace_na(public_meeting, get_mode(public_meeting)),
    permit = as.factor(permit),
    permit = replace_na(permit, get_mode(permit)),
    installer = replace_na(installer, get_mode(installer)),
    funder = replace_na(funder, get_mode(funder)),
    across(where(is.character), as.factor))
saveRDS(test_model, "data/test_imputed.rds")
