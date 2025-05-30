install.packages("readxl")
library("readxl")
library(dplyr)
library(ggplot2)
SubmissionFormat <- read_excel("~/Downloads/Seminar/Data/SubmissionFormat.xlsx")
test_set_values <- read_excel("~/Downloads/Seminar/Data/Test set values.xlsx")
training_set_labels <- read_excel("~/Downloads/Seminar/Data/Training set labels.xlsx")
training_set_values <- read_excel("~/Downloads/Seminar/Data/training set values.xlsx", skip = 1)

combined_data <- merge(training_set_values, training_set_labels, by = "id")

############## installer:
############## check if the NAs are random, example:
combined_data$installer_missing <- is.na(combined_data$installer)
table_installer_region <- table(combined_data$installer_missing, combined_data$region) #cross-tabulate
chisq.test(table_installer_region)

############# 1. Function to run chi-squared test for all categorical variables
combined_data$installer_missing <- is.na(combined_data$installer)

results <- list()

for (var in names(combined_data)) {
  if (var != "installer" && var != "installer_missing") {
    # Skip non-factor or numeric vars
    if (is.factor(combined_data[[var]]) || is.character(combined_data[[var]])) {
      tbl <- table(combined_data$installer_missing, combined_data[[var]])
      
      if (all(dim(tbl) > 1)) {
        test <- tryCatch(
          chisq.test(tbl),
          error = function(e) return(NULL)
        )
        
        if (!is.null(test)) {
          results[[var]] <- test$p.value
        }
      }
    }
  }
}

# Sort results by p-value
sorted_results <- sort(unlist(results))
sorted_results[sorted_results < 0.05]
sorted_results[sorted_results > 0.05]

###### t.test for numeric variables
t.test(combined_data$population ~ combined_data$installer_missing)
t.test(combined_data$gps_height ~ combined_data$installer_missing)
t.test(combined_data$construction_year ~ combined_data$installer_missing)

######### 2.relationship with longitude and latitude
df_clean <- combined_data %>%
  filter(!is.na(longitude), !is.na(latitude))

ggplot(df_clean, aes(x = longitude, y = latitude, color = installer_missing)) +
  geom_point(alpha = 0.5) +
  theme_minimal()


####################################funder:

############# 1. Function to run chi-squared test for all categorical variables
combined_data$funder_missing <- is.na(combined_data$funder)

results_2 <- list()

for (var in names(combined_data)) {
  if (var != "funder" && var != "funder_missing") {
    # Skip non-factor or numeric vars
    if (is.factor(combined_data[[var]]) || is.character(combined_data[[var]])) {
      tbl <- table(combined_data$funder_missing, combined_data[[var]])
      
      if (all(dim(tbl) > 1)) {
        test <- tryCatch(
          chisq.test(tbl),
          error = function(e) return(NULL)
        )
        
        if (!is.null(test)) {
          results_2[[var]] <- test$p.value
        }
      }
    }
  }
}

# Sort results by p-value
sorted_results_2 <- sort(unlist(results_2))
sorted_results_2[sorted_results_2 < 0.05]
sorted_results_2[sorted_results_2 > 0.05]

###### t.test for numeric variables
t.test(combined_data$population ~ combined_data$funder_missing)
t.test(combined_data$gps_height ~ combined_data$funder_missing)
t.test(combined_data$construction_year ~ combined_data$funder_missing)

######### 2.relationship with longitude and latitude
ggplot(combined_data, aes(x = longitude, y = latitude, color = funder_missing)) +
  geom_point(alpha = 0.5) +
  theme_minimal()



####################### gps-heigt:
############# 1. Function to run chi-squared test for all categorical variables
combined_data$gps_missing <- combined_data$gps_height == 0

results_3 <- list()

for (var in names(combined_data)) {
  if (var != "gps_height" && var != "gps_missing") {
    # Skip non-factor or numeric vars
    if (is.factor(combined_data[[var]]) || is.character(combined_data[[var]])) {
      tbl <- table(combined_data$gps_missing, combined_data[[var]])
      
      if (all(dim(tbl) > 1)) {
        test <- tryCatch(
          chisq.test(tbl),
          error = function(e) return(NULL)
        )
        
        if (!is.null(test)) {
          results_3[[var]] <- test$p.value
        }
      }
    }
  }
}

# Sort results by p-value
sorted_results_3 <- sort(unlist(results_3))
sorted_results_3[sorted_results_3 < 0.05]
sorted_results_3[sorted_results_3 > 0.05]

###### t.test for numeric variables
t.test(combined_data$population ~ combined_data$gps_missing)
t.test(combined_data$construction_year ~ combined_data$gps_missing)

######### 2.relationship with longitude and latitude
ggplot(combined_data, aes(x = longitude, y = latitude, color = gps_missing)) +
  geom_point(alpha = 0.5) +
  theme_minimal()




##########################construction_year:
############# 1. Function to run chi-squared test for all categorical variables
combined_data$construction_year_missing <- combined_data$construction_year == 0

results_4 <- list()

for (var in names(combined_data)) {
  if (var != "construction_year" && var != "construction_year_missing") {
    # Skip non-factor or numeric vars
    if (is.factor(combined_data[[var]]) || is.character(combined_data[[var]])) {
      tbl <- table(combined_data$construction_year_missing, combined_data[[var]])
      
      if (all(dim(tbl) > 1)) {
        test <- tryCatch(
          chisq.test(tbl),
          error = function(e) return(NULL)
        )
        
        if (!is.null(test)) {
          results_4[[var]] <- test$p.value
        }
      }
    }
  }
}

# Sort results by p-value
sorted_results_4 <- sort(unlist(results_4))
sorted_results_4[sorted_results_4 < 0.05]
sorted_results_4[sorted_results_4 > 0.05]

###### t.test for numeric variables
t.test(combined_data$population ~ combined_data$construction_year_missing)
t.test(combined_data$gps_height ~ combined_data$construction_year_missing)

######### 2.relationship with longitude and latitude
ggplot(combined_data, aes(x = longitude, y = latitude, color = construction_year_missing)) +
  geom_point(alpha = 0.5) +
  theme_minimal()


########################## population:
############# 1. Function to run chi-squared test for all categorical variables
combined_data$population_missing <- combined_data$population == 0

results_5 <- list()

for (var in names(combined_data)) {
  if (var != "population" && var != "population_missing") {
    # Skip non-factor or numeric vars
    if (is.factor(combined_data[[var]]) || is.character(combined_data[[var]])) {
      tbl <- table(combined_data$population_missing, combined_data[[var]])
      
      if (all(dim(tbl) > 1)) {
        test <- tryCatch(
          chisq.test(tbl),
          error = function(e) return(NULL)
        )
        
        if (!is.null(test)) {
          results_5[[var]] <- test$p.value
        }
      }
    }
  }
}

# Sort results by p-value
sorted_results_5 <- sort(unlist(results_5))
sorted_results_5[sorted_results_5 < 0.05]
sorted_results_5[sorted_results_5 > 0.05]

###### t.test for numeric variables
t.test(combined_data$gps_height ~ combined_data$population_missing)
t.test(combined_data$construction_year ~ combined_data$population_missing)


######### 2.relationship with longitude and latitude
ggplot(combined_data, aes(x = longitude, y = latitude, color = population_missing)) +
  geom_point(alpha = 0.5) +
  theme_minimal()


