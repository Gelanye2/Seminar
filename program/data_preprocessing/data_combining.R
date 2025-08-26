##########combine the dataset
train_x <- read_csv("data/Training set values.csv")
train_y <- read_csv("data/Training set labels.csv")
test <- read_csv("data/Test set values.csv")

# combine training values and labels
train <- left_join(train_x, train_y, by = "id")

#create status_group for test
test$status_group <- NA

#mark if the dataset is train or test
train$dataset <- "train"
test$dataset <- "test"
data_all <- bind_rows(train, test)
#save data_all as rds in the data folder
saveRDS(data_all, "data/data_all.rds")

#save train_all
train_all <- data_all %>% filter(dataset == "train")
saveRDS(train_all, "data/train_all.rds")

#save test_all
test_all <- data_all %>% filter(dataset == "test")
saveRDS(test_all, "data/test_all.rds")
