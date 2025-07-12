data_all_spatial <- readRDS("data/data_spatial_ml.rds")

test_all_spatial<- data_all_spatial %>%
  filter(set == "test")


test_all <- readRDS("data/test_all.rds")

test_all_spatial$id <- NA

dups <- test_all_spatial %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(longitude, latitude)

dup1 <- which(test_all_spatial$longitude == dups$longitude[1] &
                 test_all_spatial$latitude  == dups$latitude[1])

test_all_spatial$id[dup1] <- c(8015, 3000)

dup2 <- which(test_all_spatial$longitude == dups$longitude[3] &
                test_all_spatial$latitude  == dups$latitude[3])

test_all_spatial$id[dup2] <- c(25609, 31110)

dup3 <- which(test_all_spatial$longitude == dups$longitude[5] &
                test_all_spatial$latitude  == dups$latitude[5])

test_all_spatial$id[dup3] <- c(17640,9490)

dup4 <- which(test_all_spatial$longitude == dups$longitude[7] &
                test_all_spatial$latitude  == dups$latitude[7])
test_all_spatial$id[dup4] <- c(23564,30889)

test_id_ref <- test_all %>%
  filter(longitude > 25) %>%
  select(id, longitude, latitude) %>%
  distinct(longitude, latitude, .keep_all = TRUE)

#find unmatched rows
unmatched <- test_all_spatial %>%
  filter(is.na(id))

# left join id in unmateched
unmatched<- unmatched %>%
  left_join(test_id_ref, by = c("longitude", "latitude")) %>%
  mutate(id = id.y) %>%
  select(-id.x, -id.y)

matched <- test_all_spatial %>%
  filter(!is.na(id))

spatial_submission <- bind_rows(matched, unmatched) %>%
  select(id, status_group)

#read csv
na <- read_csv("predictions_NA.csv")
sub_all <- rbind(na, spatial_submission)
sub_form <- read_csv("submission/submissionFormat.csv")

na_rf <- read_csv("predictions_ranger_imp_sub.csv")
sub_all2 <- rbind(spatial_submission, na_rf) %>%
  arrange(match(id, sub_form$id))
#make the order of sub_all the same as sub_form
sub_all <- sub_all %>%
  arrange(match(id, sub_form$id))

#write csv
write_csv(sub_all, "submission/spatial_submission1.csv")
write_csv(sub_all2, "submission/spatial_submission2.csv")

identical(sub_all$id, sub_form$id) # should be TRUE
