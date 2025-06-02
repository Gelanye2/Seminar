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
