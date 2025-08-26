###contributed by:Yuxin Liu, Haoran Ju

.libPaths("/dss/dsshome1/01/ra59qow2/R/x86_64-pc-linux-gnu-library/4.3")
library(data.table); library(dplyr)
library(mlr3); library(mlr3pipelines)
library(future); plan(multisession, workers = 16)

set.seed(7832)
#initializing
path_data   <- "data/data_imputed_enhanced.rds"
path_model1 <- "result/at_lvl1.rds"
path_model2 <- "result/at_lvl2.rds"
path_sub    <- "result/cassub.csv"
df_full  <- readRDS(path_data)
train_df <- df_full %>% filter(!is.na(status_group))
test_df  <- df_full %>% filter( is.na(status_group))

#load the best learner
glrn_l1 <- readRDS(path_model1)$learner
glrn_l2 <- readRDS(path_model2)$learner

#retrain level-1
train_df$lvl1_target <- factor(ifelse(train_df$status_group=="functional",
                                      "functional","abnormal"))
task_l1_full <- TaskClassif$new("l1full",
                                backend = train_df %>% select(-status_group),
                                target  = "lvl1_target")
glrn_l1$train(task_l1_full)

#retrain level-2
train_abn <- train_df %>% filter(status_group!="functional") %>%
  mutate(lvl2_target = droplevels(status_group)) %>%
  select(-status_group)
task_l2_full <- TaskClassif$new("l2full",
                                backend = train_abn,
                                target  = "lvl2_target")
glrn_l2$train(task_l2_full)

#
lvl1_levels <- levels(train_df$lvl1_target)          # c("functional","abnormal")
lvl2_levels <- levels(train_abn$lvl2_target)         # c("needs_repair","non_functional")

#pred level-1
backend_test <- cbind(
  test_df %>% select(-status_group),
  lvl1_target = factor(NA, levels = lvl1_levels)
)
task_test_l1 <- TaskClassif$new("test_l1", backend_test, target="lvl1_target")
p1       <- glrn_l1$predict(task_test_l1)
prob_fun <- p1$prob[,"functional"]
idx_abn  <- which(prob_fun < 0.48) #######this affects a lot!!!

#pred level-2
prob_rep <- prob_non <- numeric(nrow(test_df))
if(length(idx_abn)){
  backend_test[idx_abn, "lvl2_target"] <- NA
  backend_test <- backend_test %>%
    mutate(lvl2_target = factor(lvl2_target, levels = lvl2_levels))

  task_test_l2 <- TaskClassif$new("test_l2",
                                  backend = backend_test[idx_abn, ],
                                  target  = "lvl2_target")
  p2 <- glrn_l2$predict(task_test_l2)
  prob_rep[idx_abn] <- p2$prob[,"functional needs repair"]
  prob_non[idx_abn] <- p2$prob[ , "non functional"]
}

#
P_fun <- prob_fun
P_abn <- 1 - P_fun
P_rep <- P_abn * prob_rep
P_non <- P_abn * prob_non

prob_mat <- cbind(P_fun, P_rep, P_non)
colnames(prob_mat) <- c("functional",
                        "functional needs repair",
                        "non functional")

saveRDS(prob_mat, file = "result/cas_prob_mat.rds")
prob_mat <- readRDS("result/cas_prob_mat.rds")

repair_cut <- 0.23   ####this affects a lot!
pred <- character(nrow(prob_mat))
for (i in seq_len(nrow(prob_mat))) {
  if (prob_mat[i, "functional needs repair"] >= repair_cut) {
    pred[i] <- "functional needs repair"
  } else {
    sub_probs <- prob_mat[i, c("functional", "non functional")]
    pred[i]  <- names(sub_probs)[which.max(sub_probs)]
  }
}

test_df <- test_df %>%
  mutate(status_group = pred)

test_all <- readRDS("data/test_all.rds")

result <- data.frame(
  id           = test_all$id,
  status_group = test_df$status_group,
  stringsAsFactors = FALSE
)

write.csv(result, file = path_sub, row.names = FALSE)
cat("✅ submission saved to", path_sub, "\n")
