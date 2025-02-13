# -
Κώδικας R για τη πτυχιακή εργασία ΑΝΙΧΝΕΥΣΗ ΑΠΑΤΗΣ ΣΕ ΧΡΗΜΑΤΟΟΙΚΟΝΟΜΙΚΑ ΔΕΔΟΜΕΝΑ 
#---------------------------------------------------------------
# Πρώτη Ανάγνωση αρχείου train
#---------------------------------------------------------------
# Φόρτωση βιβλιοθηκών
library(dplyr)
library(knitr)
library(kableExtra)
library(writexl)
library(lubridate)

# Απενεργοποίηση της επιστημονικής σημειογραφίας
options(scipen = 999)

# Φόρτωση δεδομένων
file_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraudTrain.csv"
data <- read.csv(file_path)

# Μετατροπές σύμφωνα με τις απαιτήσεις
data$cc_num <- as.integer(data$cc_num)
data$amt <- round(data$amt, 2)  # Μέγιστο δύο δεκαδικά
data$zip <- as.integer(data$zip)
data$lat <- sprintf("%.6f°", data$lat)  # Συντεταγμένες
data$long <- sprintf("%.6f°", data$long)  # Συντεταγμένες
data$merch_lat <- sprintf("%.6f°", data$merch_lat)  # Συντεταγμένες
data$merch_long <- sprintf("%.6f°", data$merch_long)  # Συντεταγμένες
data$dob <- as.Date(data$dob, format="%Y-%m-%d")  # Ημερομηνία γέννησης
data$unix_time <- as.POSIXct(data$unix_time, origin="1970-01-01", tz="UTC")  # Ημερομηνία & ώρα
data$trans_num <- as.integer(data$trans_num)

# Συνάρτηση για την περίληψη των μεταβλητών
summarize_variable <- function(var, name) {
  if (is.numeric(var)) {
    summary <- data.frame(
      Type = "Αριθμητική",
      Count = length(var),
      Missing = sum(is.na(var)),
      Min = if (name == "zip") NA else min(var, na.rm = TRUE),
      Max = if (name == "zip") NA else max(var, na.rm = TRUE),
      Mean = if (name %in% c("zip", "trans_num")) NA else mean(var, na.rm = TRUE),
      Variance = if (name %in% c("zip", "trans_num")) NA else var(var, na.rm = TRUE),
      Sample_Values = NA
    )
  } else if (is.character(var) || is.factor(var)) {
    summary <- data.frame(
      Type = "Κατηγορική",
      Count = length(var),
      Missing = sum(is.na(var)),
      Min = NA,
      Max = NA,
      Mean = NA,
      Variance = NA,
      Sample_Values = paste(unique(var)[1:min(5, length(unique(var)))], collapse = ", ")
    )
  } else if (inherits(var, "Date") || inherits(var, "POSIXct")) {
    summary <- data.frame(
      Type = "Ημερομηνία/Ώρα",
      Count = length(var),
      Missing = sum(is.na(var)),
      Min = min(var, na.rm = TRUE),
      Max = max(var, na.rm = TRUE),
      Mean = NA,
      Variance = NA,
      Sample_Values = NA
    )
  } else {
    summary <- data.frame(
      Type = "Άλλο",
      Count = length(var),
      Missing = sum(is.na(var)),
      Min = NA,
      Max = NA,
      Mean = NA,
      Variance = NA,
      Sample_Values = NA
    )
  }
  return(summary)
}

# Εφαρμογή της συνάρτησης σε κάθε μεταβλητή
variable_summaries <- lapply(names(data), function(name) summarize_variable(data[[name]], name))

# Δημιουργία του πίνακα
summary_table <- do.call(rbind, variable_summaries)
summary_table <- cbind(Variable = names(data), summary_table)

# Αποθήκευση του πίνακα σε αρχείο Excel
output_file_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\summary_table.xlsx"
write_xlsx(summary_table, output_file_path)

# Δημιουργία και μορφοποίηση του πίνακα
kable(summary_table, caption = "Συνοπτική Περιγραφή των Μεταβλητών") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  column_spec(1, bold = TRUE, color = "white", background = "dodgerblue") %>%
  column_spec(2:8, background = "lightgrey") %>%
  column_spec(9, width = "30em") %>%
  scroll_box(height = "500px")

#------------------------------------------------------------------
# Αξιολόγηση με λογιστική παλινδρόμηση
#------------------------------------------------------------------
#Προετοιμασία δεδομένων
#------------------------------------------------------------------
# Εγκατάσταση και φόρτωση απαραίτητων πακέτων
install.packages(c("dplyr", "caret", "pROC", "lubridate", "geosphere", "scales"))
library(dplyr)
library(caret)
library(pROC)
library(lubridate)
library(geosphere)
library(scales)

# A. Καθαρισμός του dataset

# 1. Φόρτωση δεδομένων
file_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraudTrain.csv"
data <- read.csv(file_path)

# 2. Δειγματοληψία για μείωση μεγέθους δεδομένων με αναλογία 1:10 (Απάτη:Μη Απάτη)
set.seed(123)
fraud_cases <- data %>% filter(is_fraud == 1)
non_fraud_cases <- data %>% filter(is_fraud == 0) %>% sample_n(nrow(fraud_cases) * 10)
data <- bind_rows(fraud_cases, non_fraud_cases) %>% sample_frac(1)

# 3. Αφαίρεση μη χρήσιμων δεδομένων
cols_to_remove <- c("merchant", "first", "last", "street", "city", "zip", "trans_num")
data <- data %>% select(-all_of(cols_to_remove))

# 4. Μετατροπή σε factors όπου χρειάζεται
data <- data %>% mutate(
  is_fraud = as.factor(is_fraud),
  category = as.factor(category),
  gender = as.factor(gender),
  state = as.factor(state)
)

# 5. Δημιουργία νέων μεταβλητών και κανονικοποίηση
data <- data %>%
  mutate(trans_date_time = parse_date_time(trans_date_trans_time, orders = "Ymd HMS"),
         hour_of_day = hour(trans_date_time),
         day_of_week = wday(trans_date_time, label = TRUE),
         is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
         distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                            matrix(c(long, lat), ncol = 2)),
         customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d"))/365.25),
         amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                                   labels = c("Low", "Medium", "High", "Very High", "Extreme"))))

# 6. Αφαίρεση ελλιπών δεδομένων
data <- na.omit(data)

# B. Δημιουργία μοντέλου

# 7. Εκπαίδευση μοντέλου λογιστικής παλινδρόμησης
fraud_model <- glm(is_fraud ~ amt + category + gender + state + hour_of_day + day_of_week + 
                     is_weekend + distance_from_home + customer_age + amt_category,
                   data = data, family = binomial)

# 8. Αξιολόγηση μοντέλου
summary(fraud_model)

# 9. Πρόβλεψη και υπολογισμός AUC
predictions <- predict(fraud_model, type = "response")
auc_score <- auc(data$is_fraud, predictions)
cat("AUC Score:", auc_score, "\n")

# 10. Οπτικοποίηση καμπύλης ROC
roc_curve <- roc(data$is_fraud, predictions)
plot(roc_curve, col = "blue", main = "ROC Curve")

# 11. Υπολογισμός μετρικών απόδοσης
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
conf_matrix <- confusionMatrix(as.factor(predicted_classes), data$is_fraud)
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]
balanced_accuracy <- conf_matrix$byClass["Balanced Accuracy"]

# Δημιουργία πίνακα αποτελεσμάτων
performance_metrics <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Balanced Accuracy", "AUC"),
  Value = c(precision, recall, f1_score, balanced_accuracy, auc_score)
)

# Εμφάνιση του πίνακα αποτελεσμάτων
print(performance_metrics)

# 12. Αποθήκευση καθαρισμένων δεδομένων
output_path <- "C:/Users/nitse/OneDrive/Documents/fraud_oversampled_enhanced.csv"
write.csv(data, output_path, row.names = FALSE)
cat("Το νέο oversampled dataset αποθηκεύτηκε ως fraud_oversampled_enhanced.csv")

# 13. Εμφάνιση πλήθους περιπτώσεων απάτης και μη απάτης
fraud_counts <- table(data$is_fraud)
cat("Αριθμός μη απατηλών συναλλαγών (0):", fraud_counts["0"], "\n")
cat("Αριθμός απατηλών συναλλαγών (1):", fraud_counts["1"], "\n")

# 14. Φόρτωση του test set
test_file_path <- "C:/Users/nitse/OneDrive/Documents/fraudTest.csv"
test_data <- read.csv(test_file_path)

# 15. Προεπεξεργασία του test set
test_data <- test_data %>%
  select(-all_of(cols_to_remove)) %>%
  mutate(is_fraud = as.factor(is_fraud),
         category = as.factor(category),
         gender = as.factor(gender),
         state = as.factor(state),
         trans_date_time = parse_date_time(trans_date_trans_time, orders = "Ymd HMS"),
         hour_of_day = hour(trans_date_time),
         day_of_week = wday(trans_date_time, label = TRUE),
         is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
         distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                            matrix(c(long, lat), ncol = 2)),
         customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d"))/365.25),
         amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                                   labels = c("Low", "Medium", "High", "Very High", "Extreme"))))
test_data <- na.omit(test_data)

# 16. Πρόβλεψη στο test set
test_predictions <- predict(fraud_model, newdata = test_data, type = "response")
test_predicted_classes <- ifelse(test_predictions > 0.5, 1, 0)

# Αποθήκευση προβλέψεων
write.csv(test_predicted_classes, "C:/Users/nitse/OneDrive/Documents/fraudTest_predictions.csv", row.names = FALSE)
cat("Οι προβλέψεις αποθηκεύτηκαν επιτυχώς στο αρχείο fraudTest_predictions.csv\n")

# 17. Αξιολόγηση απόδοσης στο test set
test_conf_matrix <- confusionMatrix(as.factor(test_predicted_classes), test_data$is_fraud)

# Υπολογισμός βασικών μετρικών απόδοσης
test_precision <- test_conf_matrix$byClass["Precision"]
test_recall <- test_conf_matrix$byClass["Recall"]
test_f1_score <- test_conf_matrix$byClass["F1"]
test_balanced_accuracy <- test_conf_matrix$byClass["Balanced Accuracy"]
test_auc_score <- auc(test_data$is_fraud, test_predictions)

# Δημιουργία πίνακα αποτελεσμάτων για το test set
test_performance_metrics <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Balanced Accuracy", "AUC"),
  Value = c(test_precision, test_recall, test_f1_score, test_balanced_accuracy, test_auc_score)
)

# Εμφάνιση αποτελεσμάτων
print(test_performance_metrics)

# Κατανομή των προβλεφθέντων κλάσεων (απάτες vs μη απάτες)
test_fraud_counts <- table(test_predicted_classes)
cat("Αριθμός προβλεφθέντων μη απατών (0):", test_fraud_counts["0"], "\n")
cat("Αριθμός προβλεφθέντων απατών (1):", test_fraud_counts["1"], "\n")

# 18. Οπτικοποίηση καμπύλης ROC για το test set
test_roc_curve <- roc(test_data$is_fraud, test_predictions)
plot(test_roc_curve, col = "red", main = "ROC Curve - Test Set")


#----------------------------------------------------------------------------
# Αξιολόγηση με Decision Tree Approach
#----------------------------------------------------------------------------

# 1. Φόρτωση των Βιβλιοθηκών --------------------------------------
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(lubridate)
library(geosphere)
library(pROC)

# 2. εγκατάσταση της R plot
install.packages("rpart.plot")

# 3. Φόρτωση του test set
test_file_path <- "C:/Users/nitse/OneDrive/Documents/fraudTest.csv"
test_data <- read.csv(test_file_path)

# 4. Προεπεξεργασία του test set -----------------------------------
columns_to_remove <- c("cc_num", "first", "last", "street", "job", "trans_num", "unix_time")
test_data_clean <- test_data %>%
  select(-any_of(columns_to_remove)) %>%
  mutate(
    is_fraud = as.factor(is_fraud),
    category = as.factor(category),
    gender = ifelse(gender == "F", 1, 0),
    state = as.factor(state),
    trans_date_time = parse_date_time(trans_date_trans_time, orders = "Ymd HMS"),
    hour_of_day = hour(trans_date_time),
    day_of_week = wday(trans_date_time, label = TRUE),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d"))/365.25),
    amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                              labels = c("Low", "Medium", "High", "Very High", "Extreme"))),
    transaction_hour_range = case_when(
      hour_of_day >= 0 & hour_of_day < 6 ~ "Midnight",
      hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    ),
    high_risk_state = ifelse(state %in% c("NY", "CA", "TX", "FL"), 1, 0),
    transaction_velocity = ave(amt, test_data$category, FUN = length)
  )
test_data_clean <- na.omit(test_data_clean)

# 5. Αποθήκευση του enhanced test set
write.csv(test_data_clean, "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv", row.names = FALSE)


# 6. Φόρτωση Δεδομένων ---------------------------------------------
# Ορισμός paths για τα datasets
train_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraud_oversampled_enhanced.csv"
test_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv"

# Φόρτωση των δεδομένων
train_data <- read.csv(train_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
test_data <- read.csv(test_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# 7. Καθαρισμός Δεδομένων -------------------------------------------
columns_to_remove <- c("cc_num", "first", "last", "street", "job", "trans_num", "unix_time")
train_data_clean <- train_data %>%
  select(-any_of(columns_to_remove)) %>%
  mutate(
    amt = as.numeric(amt),
    category = as.factor(category),
    gender = ifelse(gender == "F", 1, 0),
    city_pop = as.numeric(city_pop),
    hour_of_day = hour(parse_date_time(trans_date_trans_time, orders = "Ymd HMS")),
    day_of_week = wday(parse_date_time(trans_date_trans_time, orders = "Ymd HMS"), label = TRUE),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d"))/365.25),
    amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                              labels = c("Low", "Medium", "High", "Very High", "Extreme"))),
    transaction_hour_range = case_when(
      hour_of_day >= 0 & hour_of_day < 6 ~ "Midnight",
      hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    ),
    high_risk_state = ifelse(state %in% c("NY", "CA", "TX", "FL"), 1, 0),
    transaction_velocity = ave(amt, category, FUN = length)
  )


# 8. Εκπαίδευση Decision Tree ---------------------------------------
cat("Εκπαίδευση του Decision Tree Model...\n")
tree_model <- rpart(is_fraud ~ amt + category + gender + city_pop + hour_of_day + day_of_week + is_weekend + distance_from_home + customer_age + amt_category + transaction_hour_range + high_risk_state + transaction_velocity, 
                    data = train_data_clean, 
                    method = "class",
                    control = rpart.control(maxdepth = 10))

# Σύνοψη και σχεδίαση του δέντρου
print(tree_model)
rpart.plot(tree_model, main = "Decision Tree", cex = 0.6)

# 9. Πρόβλεψη στο Test Dataset ---------------------------------------
cat("Πρόβλεψη με το Decision Tree Model...\n")
tree_predictions <- predict(tree_model, newdata = test_data_clean, type = "class")

# 10. Αξιολόγηση Μοντέλου --------------------------------------------
cat("Αξιολόγηση του Decision Tree Model...\n")
tree_conf_matrix <- confusionMatrix(factor(tree_predictions), factor(test_data_clean$is_fraud))
print(tree_conf_matrix)

# Υπολογισμός Precision, Recall, F1-Score
precision <- tree_conf_matrix$byClass["Pos Pred Value"]
recall <- tree_conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1-Score: ", f1_score, "\n")

# Καμπύλη ROC
roc_curve <- roc(test_data_clean$is_fraud, as.numeric(tree_predictions))
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_value <- auc(roc_curve)
cat("AUC-ROC: ", auc_value, "\n")

#------------------------------------------------------------------
# Αξιολόγηση με GBM
#------------------------------------------------------------------

# 1. Φόρτωση των Βιβλιοθηκών --------------------------------------
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(lubridate)
library(geosphere)
library(pROC)

#install του GBM πακέτου 

install.packages("gbm")
library(gbm)

# 2. Φόρτωση Δεδομένων ---------------------------------------------

# Ορισμός των μονοπατιών για τα αρχεία
train_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraud_oversampled_enhanced.csv"
test_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv"

# Φόρτωση των δεδομένων
train_data <- read.csv(train_path)
test_data <- read.csv(test_path)

# Σύγκριση των στηλών των δύο dataset
train_columns <- colnames(train_data)
test_columns <- colnames(test_data)

# Σύγκριση τύπων δεδομένων
train_types <- sapply(train_data, class)
test_types <- sapply(test_data, class)

# Σύγκριση στηλών και τύπων δεδομένων
column_comparison <- all(train_columns == test_columns)
type_comparison <- all(train_types == test_types)

# Έλεγχος για missing τιμές
train_missing <- colSums(is.na(train_data))
test_missing <- colSums(is.na(test_data))

# Εκτύπωση των αποτελεσμάτων
cat("Column Comparison: ", column_comparison, "\n")
cat("Data Type Comparison: ", type_comparison, "\n")
cat("Missing Values in Train Data: \n")
print(train_missing)
cat("Missing Values in Test Data: \n")
print(test_missing)


# Φόρτωση των δεδομένων
train_data <- read.csv(train_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
test_data <- read.csv(test_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# 3. Καθαρισμός Δεδομένων -------------------------------------------
columns_to_remove <- c("cc_num", "first", "last", "street", "job", "trans_num", "unix_time")
train_data_clean <- train_data %>%
  select(-any_of(columns_to_remove)) %>%
  mutate(
    amt = as.numeric(amt),
    category = as.factor(category),
    gender = ifelse(gender == "F", 1, 0),
    city_pop = as.numeric(city_pop),
    hour_of_day = hour(parse_date_time(trans_date_trans_time, orders = "Ymd HMS")),
    day_of_week = wday(parse_date_time(trans_date_trans_time, orders = "Ymd HMS"), label = TRUE),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d"))/365.25),
    amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                              labels = c("Low", "Medium", "High", "Very High", "Extreme"))),
    transaction_hour_range = factor(case_when(
      hour_of_day >= 0 & hour_of_day < 6 ~ "Midnight",
      hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    ), levels = c("Midnight", "Morning", "Afternoon", "Evening")),
    high_risk_state = ifelse(state %in% c("NY", "CA", "TX", "FL"), 1, 0),
    transaction_velocity = ave(amt, category, FUN = length)
  )

# 4. Προεπεξεργασία του test set -----------------------------------
test_data_clean <- test_data %>%
  select(-any_of(columns_to_remove)) %>%
  mutate(
    is_fraud = as.factor(is_fraud),
    category = as.factor(category),
    gender = ifelse(gender == "F", 1, 0),
    state = as.factor(state),
    trans_date_time = parse_date_time(trans_date_trans_time, orders = "Ymd HMS"),
    hour_of_day = hour(trans_date_time),
    day_of_week = wday(trans_date_time, label = TRUE),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d"))/365.25),
    amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                              labels = c("Low", "Medium", "High", "Very High", "Extreme"))),
    transaction_hour_range = factor(case_when(
      hour_of_day >= 0 & hour_of_day < 6 ~ "Midnight",
      hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    ), levels = c("Midnight", "Morning", "Afternoon", "Evening")),
    high_risk_state = ifelse(state %in% c("NY", "CA", "TX", "FL"), 1, 0)
  )

# 5. Εκπαίδευση GBM Model ---------------------------------------
cat("Εκπαίδευση του GBM Model...\n")
gbm_model <- gbm(is_fraud ~ amt + category + gender + city_pop + hour_of_day + day_of_week + is_weekend + customer_age + transaction_hour_range, 
                 data = train_data_clean, 
                 distribution = "bernoulli", 
                 n.trees = 500, 
                 interaction.depth = 10, 
                 shrinkage = 0.005, 
                 cv.folds = 10, 
                 n.cores = NULL, 
                 verbose = FALSE)

print(summary(gbm_model))

# 6. Πρόβλεψη στο Test Dataset ---------------------------------------
cat("Πρόβλεψη με το GBM Model...\n")
gbm_predictions <- predict(gbm_model, newdata = test_data_clean, n.trees = 500, type = "response")
gbm_predicted_classes <- ifelse(gbm_predictions > 0.5, 1, 0)

# 7. Αξιολόγηση Μοντέλου --------------------------------------------
gbm_conf_matrix <- confusionMatrix(factor(gbm_predicted_classes), factor(test_data_clean$is_fraud))
print(gbm_conf_matrix)

# Υπολογισμός Precision, Recall, F1-Score
precision <- gbm_conf_matrix$byClass["Pos Pred Value"]
recall <- gbm_conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1-Score: ", f1_score, "\n")

# Καμπύλη ROC
roc_curve_gbm <- roc(test_data_clean$is_fraud, gbm_predictions)
plot(roc_curve_gbm, col = "blue", main = "ROC Curve - GBM")
auc_value_gbm <- auc(roc_curve_gbm)
cat("AUC-ROC: ", auc_value_gbm, "\n")



#----------------------------------------------------------------------------------
#Αξιολόγηση με K-means
#----------------------------------------------------------------------------------

# 1. Φόρτωση των βιβλιοθηκών
library(dplyr)
library(caret)
library(e1071)
install.packages("knitr", dependencies = TRUE)
library(knitr)
library(lubridate)  # Βιβλιοθήκη για ημερομηνίες και ώρες
library(ggplot2)    # Για τη δημιουργία γραφημάτων

# 2. Ορισμός των μονοπατιών για τα αρχεία
train_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraud_oversampled_enhanced.csv"
test_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv"

# 3. Φόρτωση των δεδομένων
train_data <- read.csv(train_path)
test_data <- read.csv(test_path)

# 4. Καθαρισμός Δεδομένων -------------------------------------------
columns_to_remove <- c("cc_num", "first", "last", "street", "job", "trans_num", "unix_time")
train_data_clean <- train_data %>%
  select(-any_of(columns_to_remove)) %>%
  mutate(
    amt = as.numeric(amt),
    category = as.factor(category),
    gender = ifelse(gender == "F", 1, 0),
    city_pop = as.numeric(city_pop),
    hour_of_day = hour(parse_date_time(trans_date_trans_time, orders = "Ymd HMS")),
    day_of_week = wday(parse_date_time(trans_date_trans_time, orders = "Ymd HMS"), label = TRUE),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d"))/365.25),
    amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                              labels = c("Low", "Medium", "High", "Very High", "Extreme"))),
    transaction_hour_range = factor(case_when(
      hour_of_day >= 0 & hour_of_day < 6 ~ "Midnight",
      hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    ), levels = c("Midnight", "Morning", "Afternoon", "Evening")),
    high_risk_state = ifelse(state %in% c("NY", "CA", "TX", "FL"), 1, 0),
    transaction_velocity = ave(amt, category, FUN = length)
  )

# 5. Εφαρμογή του K-means Clustering ----------------------------------------
# Ορίζουμε τον αριθμό των clusters που θέλουμε (π.χ. 3 clusters)
k <- 3

# Εφαρμογή του K-means clustering στο train set
set.seed(123)  # Διασφάλιση επαναληψιμότητας
kmeans_result <- kmeans(train_data_clean[, c("amt", "city_pop", "customer_age")], centers = k)

# 6. Ανάλυση των αποτελεσμάτων -------------------------------------------
# Εκτύπωση των κεντρικών σημείων των clusters
cat("Κεντρικά σημεία των clusters:\n")
print(kmeans_result$centers)

# Εκτύπωση του αριθμού των παρατηρήσεων σε κάθε cluster
cat("\nΑριθμός παρατηρήσεων σε κάθε cluster:\n")
print(table(kmeans_result$cluster))

# 7. Δημιουργία Διαγράμματος Διασποράς -----------------------------------
# Δημιουργούμε ένα scatter plot που απεικονίζει τα clusters
train_data_clean$cluster <- as.factor(kmeans_result$cluster)

# Δημιουργούμε ένα νέο data frame για τα κεντρικά σημεία με κατάλληλα ονόματα για τις στήλες
centers_df <- data.frame(
  amt = kmeans_result$centers[, 1],
  city_pop = kmeans_result$centers[, 2],
  customer_age = kmeans_result$centers[, 3],
  cluster = factor(1:k)
)

# Δημιουργία του διαγράμματος με τα κεντρικά σημεία
ggplot(train_data_clean, aes(x = amt, y = city_pop, color = cluster)) +
  geom_point() +
  geom_point(data = centers_df, aes(x = amt, y = city_pop, color = cluster), size = 5, shape = 3) +  # Κεντρικά σημεία των clusters
  labs(title = "K-means Clustering",
       x = "Amount",
       y = "City Population") +
  theme_minimal()

# 8. Ανάθεση των παρατηρήσεων του test set στα clusters --------------------
# Υπολογίζουμε την απόσταση από τα κέντρα των clusters για το test set
test_data_clean_matrix <- as.matrix(test_data_clean[, c("amt", "city_pop", "customer_age")])
cluster_assignments <- apply(test_data_clean_matrix, 1, function(x) {
  # Υπολογισμός της απόστασης μεταξύ της παρατήρησης και των κεντρικών σημείων των clusters
  distances <- sqrt(rowSums((t(kmeans_result$centers) - x)^2))  # Ευκλείδεια απόσταση
  return(which.min(distances))  # Ανάθεση στην κοντινότερη ομάδα
})

# Εκτύπωση των αποτελεσμάτων για το test set
cat("Προβλέψεις για το test set:\n")
print(cluster_assignments)

#-------------------------------------------------------------------------------
# Αξιολόγηση με Isolation Tree
#-------------------------------------------------------------------------------

# 1. Εγκατάσταση και Φόρτωση των Απαραίτητων Βιβλιοθηκών για Isolation Tree

# install.packages("isotree")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("geosphere")
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("pROC")
# install.packages("knitr")
# install.packages("kableExtra")

install.packages("isotree", dependencies = TRUE)
library(isotree)     # Για την Isolation Forest
library(dplyr)       # Για την επεξεργασία δεδομένων
library(lubridate)   # Για την εργασία με ημερομηνίες και ώρες
library(geosphere)   # Για την υπολογισμό αποστάσεων γεωγραφικών συντεταγμένων
library(ggplot2)     # Για την οπτικοποίηση δεδομένων
library(caret)       # Για την αξιολόγηση μοντέλων
library(pROC)        # Για την κατασκευή ROC curves
library(knitr)       # Για τη δημιουργία πινάκων
library(kableExtra)  # Για την ομορφότερη παρουσίαση πινάκων


# 2. Ορισμός των Μονοπατιών και Φόρτωση των Δεδομένων
train_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraud_oversampled_enhanced.csv"
test_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv"

# Φόρτωση των δεδομένων
train_data <- read.csv(train_path, stringsAsFactors = FALSE)
test_data <- read.csv(test_path, stringsAsFactors = FALSE)

# 3. Προεπεξεργασία Δεδομένων

# Συνάρτηση για την μετατροπή ordered σε unordered factor
make_unordered <- function(x) {
  if (is.ordered(x)) {
    return(as.factor(x))
  } else {
    return(x)
  }
}

# Καθαρισμός των δεδομένων του training set
columns_to_remove <- c("cc_num", "first", "last", "street", "job", "trans_num", "unix_time")
train_data_clean <- train_data %>%
  select(-any_of(columns_to_remove)) %>%
  mutate(
    amt = as.numeric(amt),
    category = as.factor(category),
    gender = ifelse(gender == "F", 1, 0),
    city_pop = as.numeric(city_pop),
    hour_of_day = hour(parse_date_time(trans_date_trans_time, orders = "Ymd HMS")),
    day_of_week = make_unordered(wday(parse_date_time(trans_date_trans_time, orders = "Ymd HMS"), label = TRUE)),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d")) / 365.25),
    amt_category = as.factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                                 labels = c("Low", "Medium", "High", "Very High", "Extreme"))),
    transaction_hour_range = as.factor(case_when(
      hour_of_day >= 0 & hour_of_day < 6 ~ "Midnight",
      hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    )),
    high_risk_state = ifelse(state %in% c("NY", "CA", "TX", "FL"), 1, 0)
  ) %>%
  group_by(category) %>%
  mutate(transaction_velocity = n()) %>%
  ungroup()

# Καθαρισμός των δεδομένων του test set
test_data_clean <- test_data %>%
  select(-any_of(columns_to_remove)) %>%
  mutate(
    is_fraud = as.factor(is_fraud),
    category = as.factor(category),
    gender = ifelse(gender == "F", 1, 0),
    state = as.factor(state),
    trans_date_time = parse_date_time(trans_date_trans_time, orders = "Ymd HMS"),
    hour_of_day = hour(trans_date_time),
    day_of_week = make_unordered(wday(trans_date_time, label = TRUE)),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format="%Y-%m-%d")) / 365.25),
    amt_category = as.factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                                 labels = c("Low", "Medium", "High", "Very High", "Extreme"))),
    transaction_hour_range = as.factor(case_when(
      hour_of_day >= 0 & hour_of_day < 6 ~ "Midnight",
      hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    )),
    high_risk_state = ifelse(state %in% c("NY", "CA", "TX", "FL"), 1, 0)
  ) %>%
  group_by(category) %>%
  mutate(transaction_velocity = n()) %>%
  ungroup()

# Επιβεβαίωση ότι δεν υπάρχουν ordered factors και ότι οι στήλες έχουν δημιουργηθεί σωστά
str(train_data_clean)
str(test_data_clean)

# 4. Εκπαίδευση του Isolation Forest Μοντέλου

# Επιλογή χαρακτηριστικών για το μοντέλο (χωρίς την ετικέτα is_fraud)
features <- c("amt", "city_pop", "customer_age", "gender", "hour_of_day", "day_of_week",
              "is_weekend", "distance_from_home", "transaction_velocity", "high_risk_state")

# Έλεγχος αν όλες οι στήλες υπάρχουν
missing_features_train <- setdiff(features, names(train_data_clean))
if (length(missing_features_train) > 0) {
  stop(paste("Missing columns in train_data_clean:", paste(missing_features_train, collapse = ", ")))
}

missing_features_test <- setdiff(features, names(test_data_clean))
if (length(missing_features_test) > 0) {
  stop(paste("Missing columns in test_data_clean:", paste(missing_features_test, collapse = ", ")))
}

# Μετατροπή κατηγορικών μεταβλητών σε αριθμητικές μέσω one-hot encoding (simplified)
train_features <- train_data_clean %>%
  select(all_of(features)) %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) # Μετατροπή factor σε numeric

test_features <- test_data_clean %>%
  select(all_of(features)) %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) # Μετατροπή factor σε numeric

# Δημιουργία και εκπαίδευση του μοντέλου Isolation Forest
iforest_model <- isolation.forest(train_features, ntrees = 100, sample_size = 256)

# 5. Πρόβλεψη στο Test Set

# Πρόβλεψη των anomaly scores στο test set
test_data_clean$anomaly_score <- predict(iforest_model, test_features)

# Ορισμός ενός ορίου για την κατηγοριοποίηση (π.χ., 0.5)
threshold <- 0.5
test_data_clean$predicted_fraud <- ifelse(test_data_clean$anomaly_score > threshold, 1, 0)

# Μετατροπή σε factor για αξιολόγηση
test_data_clean$predicted_fraud <- as.factor(test_data_clean$predicted_fraud)
test_data_clean$is_fraud <- as.factor(test_data_clean$is_fraud)

# 6. Αξιολόγηση του Μοντέλου

# Δημιουργία του Confusion Matrix
conf_matrix <- confusionMatrix(test_data_clean$predicted_fraud, test_data_clean$is_fraud)
print(conf_matrix)

# Υπολογισμός Precision, Recall, F1-Score
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision: ", round(precision, 4), "\n")
cat("Recall: ", round(recall, 4), "\n")
cat("F1-Score: ", round(f1_score, 4), "\n")

# Καμπύλη ROC και Υπολογισμός AUC
roc_curve <- roc(as.numeric(as.character(test_data_clean$is_fraud)), as.numeric(test_data_clean$anomaly_score))
plot(roc_curve, col = "blue", main = "ROC Curve - Isolation Forest")
auc_value <- auc(roc_curve)
cat("AUC-ROC: ", round(auc_value, 4), "\n")

# 7. Οπτικοποίηση των Αποτελεσμάτων
#a.scatter plot
test_data_filtered <- test_data_clean %>%
  filter(amt < quantile(amt, 0.99), city_pop < quantile(city_pop, 0.99))

ggplot(test_data_filtered, aes(x = amt, y = city_pop, color = predicted_fraud)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("blue", "red"), labels = c("Not Fraud", "Fraud")) +
  labs(title = "Anomaly Detection with Isolation Forest",
       x = "Transaction Amount",
       y = "City Population",
       color = "Fraud Prediction") +
  theme_minimal()

# b. Histogram των Anomaly Scores
ggplot(test_data_clean, aes(x = anomaly_score, fill = is_fraud)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Anomaly Scores",
       x = "Anomaly Score",
       y = "Count",
       fill = "Actual Fraud") +
  theme_minimal()

# 8. Δημιουργία Όμορφου Πίνακα Αποτελεσμάτων

# Δημιουργία ενός summary πίνακα με τα αποτελέσματα της αξιολόγησης
evaluation_summary <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "AUC-ROC"),
  Value = c(round(precision, 4), round(recall, 4), round(f1_score, 4), round(auc_value, 4))
)

# Δημιουργία του πίνακα με τη χρήση των knitr και kableExtra
evaluation_summary %>%
  kable("html", caption = "Αξιολόγηση Μοντέλου Isolation Forest") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "left")

# 9. Επιπλέον Βήματα (Προαιρετικά)

# Προσαρμογή του Threshold βάσει του ROC Curve
# Μπορείς να επιλέξεις ένα όριο που βελτιστοποιεί κάποιο metric, όπως το F1-Score

# Υπολογισμός των best threshold βάσει F1-Score
thresholds <- seq(0, 1, by = 0.01)
f1_scores <- sapply(thresholds, function(t) {
  preds <- ifelse(test_data_clean$anomaly_score > t, 1, 0)
  preds <- as.factor(preds)
  cm <- confusionMatrix(preds, test_data_clean$is_fraud)
  p <- cm$byClass["Pos Pred Value"]
  r <- cm$byClass["Sensitivity"]
  if (is.na(p) | is.na(r) | (p + r) == 0) {
    return(0)
  } else {
    return(2 * (p * r) / (p + r))
  }
})

# Δημιουργία πίνακα με thresholds και F1-scores
threshold_f1 <- data.frame(
  Threshold = thresholds,
  F1_Score = f1_scores
)

# Βρες το threshold με το μέγιστο F1-Score
best_threshold <- threshold_f1$Threshold[which.max(threshold_f1$F1_Score)]
best_f1 <- max(threshold_f1$F1_Score)

cat("Καλύτερο όριο βάσει F1-Score:", best_threshold, "\n")
cat("Μέγιστο F1-Score:", round(best_f1, 4), "\n")

#----------------------------------------------------------------------------------------------------------------------------------------------
# Αξιολόγηση με Random forest
# ---------------------------------------------

#--------------------------
# 1. Φόρτωση των Δεδομένων
#--------------------------

# Ορισμός των μονοπατιών των αρχείων δεδομένων
train_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraud_oversampled_enhanced.csv"
test_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv"

# Φόρτωση των δεδομένων
train_data <- read.csv(train_path, stringsAsFactors = FALSE)
test_data <- read.csv(test_path, stringsAsFactors = FALSE)

# ---------------------------------------------
# 2. Έλεγχος Τύπων Δεδομένων
# ---------------------------------------------

cat("Επισκόπηση Τύπων Δεδομένων στο Training Set:\n")
str(train_data)  # Τύποι δεδομένων για το training set

cat("\nΕπισκόπηση Τύπων Δεδομένων στο Test Set:\n")
str(test_data)  # Τύποι δεδομένων για το test set

# ---------------------------------------------
# 3. Έλεγχος για Ελλείπουσες Τιμές
# ---------------------------------------------

cat("\nΕλλείπουσες τιμές στο Training Set:\n")
print(colSums(is.na(train_data)))

cat("\nΕλλείπουσες τιμές στο Test Set:\n")
print(colSums(is.na(test_data)))

# ---------------------------------------------
# 4. Γρήγορη Σύνοψη των Δεδομένων
# ---------------------------------------------

cat("\nΣύνοψη του Training Set:\n")
summary(train_data)

cat("\nΣύνοψη του Test Set:\n")
summary(test_data)

# ---------------------------------------------
# 5. Έλεγχος Κατανομής για τη Μεταβλητή Στόχος
# ---------------------------------------------

if("is_fraud" %in% colnames(train_data)) {
  cat("\nΚατανομή της Μεταβλητής Στόχος (is_fraud) στο Training Set:\n")
  print(table(train_data$is_fraud))
} else {
  cat("\nΗ μεταβλητή 'is_fraud' δεν βρέθηκε στο Training Set.\n")
}

if("is_fraud" %in% colnames(test_data)) {
  cat("\nΚατανομή της Μεταβλητής Στόχος (is_fraud) στο Test Set:\n")
  print(table(test_data$is_fraud))
} else {
  cat("\nΗ μεταβλητή 'is_fraud' δεν βρέθηκε στο Test Set.\n")
}
# 1. Μετατροπή της μεταβλητής στόχος σε factor
train_data$is_fraud <- factor(train_data$is_fraud, levels = c(0, 1), labels = c("normal", "fraud"))
test_data$is_fraud <- factor(test_data$is_fraud, levels = c(0, 1), labels = c("normal", "fraud"))

# 2. Μετατροπή character μεταβλητών σε factor
train_data <- train_data %>% mutate_if(is.character, as.factor)
test_data <- test_data %>% mutate_if(is.character, as.factor)

# 3. Αφαίρεση περιττών στηλών
columns_to_remove <- c("X", "cc_num", "dob", "trans_date_trans_time", "unix_time")
train_data <- train_data %>% select(-any_of(columns_to_remove))
test_data <- test_data %>% select(-any_of(columns_to_remove))

# 4. Έλεγχος και αφαίρεση μεταβλητών με μηδενική διακύμανση
nzv <- nearZeroVar(train_data)
if(length(nzv) > 0){
  train_data <- train_data[, -nzv]
  test_data <- test_data[, -nzv]
}

# 5. Έλεγχος κατανομής της μεταβλητής στόχος
cat("\nΚατανομή της Μεταβλητής Στόχος στο Training Set:\n")
print(table(train_data$is_fraud))

cat("\nΚατανομή της Μεταβλητής Στόχος στο Test Set:\n")
print(table(test_data$is_fraud))
# ---------------------------------------------
# 1. Φόρτωση των Δεδομένων
# ---------------------------------------------

# Ορισμός των μονοπατιών των αρχείων δεδομένων
train_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraud_oversampled_enhanced.csv"
test_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv"

# Φόρτωση των δεδομένων
train_data <- read.csv(train_path)
test_data <- read.csv(test_path)

# ---------------------------------------------
# 2. Προετοιμασία των Δεδομένων
# ---------------------------------------------

# Μετατροπή της μεταβλητής στόχος σε factor
train_data$is_fraud <- factor(train_data$is_fraud, levels = c(0, 1), labels = c("normal", "fraud"))
test_data$is_fraud <- factor(test_data$is_fraud, levels = c(0, 1), labels = c("normal", "fraud"))

# ---------------------------------------------
# 3. Εξίσωση Στηλών Μεταξύ Training και Test Sets
# ---------------------------------------------

# Βρίσκουμε τις κοινές στήλες μεταξύ των δύο datasets
common_columns <- intersect(colnames(train_data), colnames(test_data))

# Διατήρηση μόνο των κοινών στηλών στα δύο datasets
train_data <- train_data[, common_columns]
test_data <- test_data[, common_columns]

# ---------------------------------------------
# 4. Έλεγχος Συμβατότητας
# ---------------------------------------------

# Επιβεβαίωση ότι οι στήλες είναι ίδιες
cat("Στήλες στο Training Set:\n")
print(colnames(train_data))

cat("\nΣτήλες στο Test Set:\n")
print(colnames(test_data))

# Έλεγχος για ελλείπουσες τιμές στο Test Set
cat("\nΕλλείπουσες τιμές στο Test Set μετά την ευθυγράμμιση:\n")
print(colSums(is.na(test_data)))

# ---------------------------------------------
# 5. Εκπαίδευση του Random Forest Μοντέλου
# ---------------------------------------------

# Εγκατάσταση και φόρτωση των απαραίτητων βιβλιοθηκών
if (!"randomForest" %in% installed.packages()) {
  install.packages("randomForest")
}
library(randomForest)

# Εκπαίδευση του Random Forest μοντέλου
set.seed(123)
rf_model <- randomForest(
  is_fraud ~ .,  # Χρήση όλων των χαρακτηριστικών
  data = train_data,
  ntree = 500,         # Αριθμός δέντρων
  mtry = floor(sqrt(ncol(train_data) - 1)),  # Αριθμός χαρακτηριστικών ανά split
  importance = TRUE    # Υπολογισμός σημασίας χαρακτηριστικών
)

# Εμφάνιση της σημασίας χαρακτηριστικών
print(rf_model)
importance(rf_model)
varImpPlot(rf_model)

# ---------------------------------------------
# 6. Πρόβλεψη στο Test Set
# ---------------------------------------------

# Πρόβλεψη των κλάσεων
rf_predictions <- predict(rf_model, newdata = test_data)

# Πρόβλεψη πιθανοτήτων
rf_probabilities <- predict(rf_model, newdata = test_data, type = "prob")[, "fraud"]

# ---------------------------------------------
# 7. Αξιολόγηση του Μοντέλου
# ---------------------------------------------

# Εγκατάσταση και φόρτωση των απαραίτητων βιβλιοθηκών για αξιολόγηση
if (!"caret" %in% installed.packages()) {
  install.packages("caret")
}
if (!"pROC" %in% installed.packages()) {
  install.packages("pROC")
}
library(caret)
library(pROC)

# Confusion Matrix
conf_matrix <- confusionMatrix(rf_predictions, test_data$is_fraud, positive = "fraud")
print(conf_matrix)

# Υπολογισμός Precision, Recall, F1-Score
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision: ", round(precision, 4), "\n")
cat("Recall: ", round(recall, 4), "\n")
cat("F1-Score: ", round(f1_score, 4), "\n")

# Υπολογισμός και οπτικοποίηση της ROC Curve
roc_curve <- roc(response = test_data$is_fraud, predictor = rf_probabilities, levels = c("normal", "fraud"))
plot(roc_curve, col = "blue", main = "ROC Curve - Random Forest")
auc_value <- auc(roc_curve)
cat("AUC-ROC: ", round(auc_value, 4), "\n")

#--------------------------------------------------------------------------------------------------------------------------------------------
# Αξιολόγηση με SVM
#  -----------------------------------
# ΒΗΜΑ 1: ΦΟΡΤΩΣΗ ΚΑΙ ΕΞΕΤΑΣΗ ΔΕΔΟΜΕΝΩΝ
# -----------------------------------
library(e1071)
library(pROC)

# Επισκόπηση δεδομένων
str(train_data)
summary(train_data)

# -----------------------------------
# ΒΗΜΑ 2: ΚΑΘΑΡΙΣΜΟΣ ΔΕΔΟΜΕΝΩΝ
# -----------------------------------
# Μετατροπή της 'is_fraud' σε factor
train_data$is_fraud <- as.factor(train_data$is_fraud)
test_data$is_fraud <- as.factor(test_data$is_fraud)

# Καθορισμός των στηλών που θα αφαιρεθούν
columns_to_remove <- c("X", "trans_date_trans_time", "dob", "trans_date_time", 
                       "cc_num", "first", "last", "street", "job", "trans_num", "unix_time")

# Αφαίρεση των επιλεγμένων στηλών
train_data <- train_data[, !(names(train_data) %in% columns_to_remove)]
test_data <- test_data[, !(names(test_data) %in% columns_to_remove)]

# Βεβαιώνουμε ότι τα δύο dataset έχουν τις ίδιες στήλες
common_columns <- intersect(names(train_data), names(test_data))
train_data <- train_data[, common_columns]
test_data <- test_data[, common_columns]

# Βρίσκουμε τις αριθμητικές στήλες
numeric_features_train <- names(train_data)[sapply(train_data, is.numeric)]
numeric_features_test <- names(test_data)[sapply(test_data, is.numeric)]

# Βεβαιώνουμε ότι και στα δύο dataset υπάρχουν οι ίδιες αριθμητικές στήλες
numeric_features <- intersect(numeric_features_train, numeric_features_test)

# Κανονικοποίηση των αριθμητικών χαρακτηριστικών
normalize <- function(x) { return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)) }
train_data[numeric_features] <- lapply(train_data[numeric_features], normalize)
test_data[numeric_features] <- lapply(test_data[numeric_features], normalize)

# Έλεγχος ότι τα datasets έχουν τις ίδιες στήλες
if (!identical(names(train_data), names(test_data))) {
  stop("Τα δύο dataset δεν έχουν τις ίδιες στήλες! Ελέγξτε τα δεδομένα σας.")
}

# Επιβεβαίωση
cat("Καθαρισμός δεδομένων ολοκληρώθηκε! Τα train και test έχουν τις ίδιες στήλες.\n")

# -----------------------------------
# ΒΗΜΑ 3: ΔΗΜΙΟΥΡΓΙΑ ΥΠΟΣΥΝΟΛΟΥ ΔΕΔΟΜΕΝΩΝ
# -----------------------------------
set.seed(123)
sample_indices <- sample(1:nrow(train_data), size = 50000)
train_data_subset <- train_data[sample_indices, ]

# -----------------------------------
# ΒΗΜΑ 4: ΕΚΠΑΙΔΕΥΣΗ SVM ΜΕ LINEAR KERNEL
# -----------------------------------
svm_model <- svm(
  is_fraud ~ .,
  data = train_data_subset,  # Χρησιμοποιούμε το υποσύνολο δεδομένων
  kernel = "linear",
  cost = 1,
  probability = TRUE
)
#-------------------------------------------------------------------
# ΒΗΜΑ 5: ΕΚΠΑΙΔΕΥΣΗ SVM ΜΕ POLYNOMIAL KERNEL
#-------------------------------------------------------------------
  
svm_model <- svm(is_fraud ~ ., data = train_data_subset, kernel = "polynomial", 
                 degree = 3, cost = 10, probability = TRUE)


# -----------------------------------
# 📌 ΒΗΜΑ 6: ΑΞΙΟΛΟΓΗΣΗ ΜΟΝΤΕΛΟΥ
# -----------------------------------

# Πρόβλεψη στο test set
predictions <- predict(svm_model, test_data[, -which(names(test_data) == "is_fraud")])

# Δημιουργία Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_data$is_fraud)
print(confusion_matrix)

# Υπολογισμός Ακρίβειας (Accuracy)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("📌 Accuracy:", round(accuracy, 4), "\n")

# Υπολογισμός Precision, Recall και F1-Score για την κατηγορία "fraud"
precision <- confusion_matrix["fraud", "fraud"] / sum(confusion_matrix["fraud", ])
recall <- confusion_matrix["fraud", "fraud"] / sum(confusion_matrix[, "fraud"])
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Εκτύπωση αποτελεσμάτων
cat("📌 Precision:", round(precision, 4), "\n")
cat("📌 Recall:", round(recall, 4), "\n")
cat("📌 F1-Score:", round(f1_score, 4), "\n")

# -----------------------------------
# 📌 ΒΗΜΑ 7: ΥΠΟΛΟΓΙΣΜΟΣ ROC ΚΑΙ AUC
# -----------------------------------

# Έλεγχος αν το SVM έχει εκπαιδευτεί με probability = TRUE
if (!is.null(svm_model$probability) && svm_model$probability) {
  
  # Πρόβλεψη με πιθανότητες
  predictions_prob <- predict(svm_model, test_data[, -which(names(test_data) == "is_fraud")], probability = TRUE)
  
  # Εξαγωγή πιθανοτήτων για την κατηγορία "fraud"
  probabilities <- attr(predictions_prob, "probabilities")[, "fraud"]
  
  # Δημιουργία ROC curve
  library(pROC)
  roc_curve <- roc(test_data$is_fraud, probabilities, levels = c("normal", "fraud"), direction = "<")
  
  # Υπολογισμός AUC
  auc_value <- auc(roc_curve)
  
  # Εκτύπωση αποτελέσματος
  cat("📌 AUC-ROC:", round(auc_value, 4), "\n")
  
  # Οπτικοποίηση της καμπύλης ROC
  plot(roc_curve, main = "ROC Curve - SVM Model", col = "blue", lwd = 2)
} else {
  cat("❌ Σφάλμα: Το SVM μοντέλο δεν έχει εκπαιδευτεί με probability = TRUE. Παρακαλώ επανεκπαιδεύστε το με svm(..., probability = TRUE)\n")
}


#---------------------------------------------------------------
# Αξιολόγηση με DBSCAN
#---------------------------------------------------------------

# 1 Εγκατάσταση και φόρτωση βιβλιοθηκών
install.packages("dbscan", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)
install.packages("FactoMineR", dependencies=TRUE)  # Για PCA ανάλυση

library(dbscan)
library(ggplot2)
library(dplyr)
library(FactoMineR)

# 2 Ορισμός των μονοπατιών για τα αρχεία
train_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraud_oversampled_enhanced.csv"
test_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\FraudTest_enhanced.csv"

# 3 Φόρτωση των δεδομένων
train_data <- read.csv(train_path)
test_data <- read.csv(test_path)

# 4 Καθαρισμός δεδομένων
train_data[is.na(train_data)] <- sapply(train_data, function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else x)
test_data[is.na(test_data)] <- sapply(test_data, function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else x)

# 5 Αφαίρεση μη αριθμητικών στηλών
train_data <- train_data %>% select_if(is.numeric)
test_data <- test_data %>% select_if(is.numeric)

# 6 Κανονικοποίηση δεδομένων (Min-Max Scaling)
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
train_data <- as.data.frame(lapply(train_data, normalize))
test_data <- as.data.frame(lapply(test_data, normalize))

# 7 Εφαρμογή DBSCAN
set.seed(123)  # Για αναπαραγωγή αποτελεσμάτων
dbscan_result <- dbscan(train_data, eps = 0.2, minPts = 5)

# 8 Προσθήκη των labels στο dataset
train_data$cluster <- as.factor(dbscan_result$cluster)

# 9 Ερμηνεία των αποτελεσμάτων

## (α) Μέγεθος κάθε cluster
cat("📊 Μέγεθος των clusters:\n")
print(table(dbscan_result$cluster))

## (β) Μέσος όρος κάθε χαρακτηριστικού ανά cluster
cat("\n📌 Μέσος όρος χαρακτηριστικών ανά cluster:\n")
print(aggregate(train_data[, -ncol(train_data)], by = list(cluster = train_data$cluster), mean))

## (γ) Ανάλυση outliers (cluster 0)
cat("\n🚨 Στατιστικά των outliers (Cluster 0):\n")
outliers <- train_data[dbscan_result$cluster == 0, ]
print(summary(outliers))

# 10 Οπτικοποίηση των clusters (αν έχει 2 ή 3 διαστάσεις)
if (ncol(train_data) >= 2) {
  ggplot(train_data, aes(x = train_data[,1], y = train_data[,2], color = cluster)) +
    geom_point() +
    theme_minimal() +
    labs(title = "DBSCAN Clustering", x = "Feature 1", y = "Feature 2")
}

# 11 PCA Visualization για πολλά χαρακτηριστικά
pca <- PCA(train_data[, -ncol(train_data)], graph = FALSE)
pca_data <- data.frame(pca$ind$coord, cluster = train_data$cluster)

ggplot(pca_data, aes(x = Dim.1, y = Dim.2, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "DBSCAN Clustering with PCA", x = "Principal Component 1", y = "Principal Component 2")

# 12 Ανάλυση πιθανής απάτης (αν υπάρχει η στήλη `is_fraud`)
if ("is_fraud" %in% colnames(train_data)) {
  cat("\n⚠️ Ανάλυση απάτης ανά cluster:\n")
  print(table(train_data$cluster, train_data$is_fraud))
  
#------------------------------------------------------------------------
# Ανάλυση fraud profile στα clusters που προέκυψαν από dbscan
#------------------------------------------------------------------------
  # 1 Εγκατάσταση και φόρτωση βιβλιοθηκών
  install.packages("ggplot2", dependencies=TRUE)
  install.packages("dplyr", dependencies=TRUE)
  
  library(ggplot2)
  library(dplyr)
  
  # 2 Ορισμός των fraud clusters με τις περισσότερες απάτες
  top_fraud_clusters <- c(0, 3, 5, 1, 26, 41, 46, 20, 139)
  
  # 3 Φιλτράρισμα των συναλλαγών που ανήκουν σε αυτά τα clusters
  fraud_clusters_data <- train_data[train_data$cluster %in% top_fraud_clusters, ]
  
  # 4 Ορισμός των min/max τιμών για απο-κανονικοποίηση
  min_values <- list(amt = 1, distance_from_home = 0, hour_of_day = 0, customer_age = 18)
  max_values <- list(amt = 10000, distance_from_home = 500, hour_of_day = 24, customer_age = 90)
  
  # 5 Συνάρτηση για αναστροφή της κανονικοποίησης
  inverse_normalization <- function(value, feature) {
    min_val <- min_values[[feature]]
    max_val <- max_values[[feature]]
    return(value * (max_val - min_val) + min_val)
  }
  
  # 6 Εφαρμογή αποκλιμάκωσης στις βασικές μεταβλητές
  fraud_clusters_data$amt <- sapply(fraud_clusters_data$amt, inverse_normalization, "amt")
  fraud_clusters_data$distance_from_home <- sapply(fraud_clusters_data$distance_from_home, inverse_normalization, "distance_from_home")
  fraud_clusters_data$hour_of_day <- sapply(fraud_clusters_data$hour_of_day, inverse_normalization, "hour_of_day")
  fraud_clusters_data$customer_age <- sapply(fraud_clusters_data$customer_age, inverse_normalization, "customer_age")
  
  # 7 Υπολογισμός μέσων τιμών για αυτά τα clusters
  fraud_profiles <- fraud_clusters_data %>%
    summarise(
      avg_amount = mean(amt),
      avg_distance = mean(distance_from_home),
      avg_hour = mean(hour_of_day),
      avg_age = mean(customer_age)
    )
  
  # 8 Προβολή του προφίλ των απατεώνων στα συγκεκριμένα clusters
  print(fraud_profiles)
  
  # 9 Ανάλυση της κατανομής των χαρακτηριστικών
  fraud_clusters_data %>%
    summarise(
      min_amount = min(amt), max_amount = max(amt),
      min_distance = min(distance_from_home), max_distance = max(distance_from_home),
      min_hour = min(hour_of_day), max_hour = max(hour_of_day),
      min_age = min(customer_age), max_age = max(customer_age)
    )
  
  # 10 Οπτικοποιήσεις για patterns απάτης στα συγκεκριμένα clusters
  
  ## (γ) Ανάλυση ηλικιών πελατών στα fraud clusters
  ggplot(fraud_clusters_data, aes(x = customer_age, fill = as.factor(cluster))) +
    geom_histogram(binwidth = 5, position = "dodge") +
    theme_minimal() +
    labs(title = "Ηλικία Πελάτη και Πιθανότητα Απάτης στα Fraud Clusters", 
         x = "Ηλικία Πελάτη", y = "Συχνότητα", fill = "Cluster")
  

}
# 1 Εγκατάσταση και φόρτωση βιβλιοθηκών
install.packages("ggplot2", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)

library(ggplot2)
library(dplyr)

# 2 Ορισμός των fraud clusters με τις περισσότερες απάτες
top_fraud_clusters <- c(0, 3, 5, 1, 26, 41, 46, 20, 139)

# 3 Φιλτράρισμα των συναλλαγών που ανήκουν σε αυτά τα clusters
fraud_clusters_data <- train_data[train_data$cluster %in% top_fraud_clusters, ]

# 4 Ορισμός των min/max τιμών για απο-κανονικοποίηση
min_values <- list(amt = 1, distance_from_home = 0, hour_of_day = 0, customer_age = 18)
max_values <- list(amt = 10000, distance_from_home = 500, hour_of_day = 24, customer_age = 90)

# 5 Συνάρτηση για αναστροφή της κανονικοποίησης
inverse_normalization <- function(value, feature) {
  min_val <- min_values[[feature]]
  max_val <- max_values[[feature]]
  return(value * (max_val - min_val) + min_val)
}

# 6 Εφαρμογή αποκλιμάκωσης στις βασικές μεταβλητές
fraud_clusters_data$amt <- sapply(fraud_clusters_data$amt, inverse_normalization, "amt")
fraud_clusters_data$distance_from_home <- sapply(fraud_clusters_data$distance_from_home, inverse_normalization, "distance_from_home")
fraud_clusters_data$hour_of_day <- sapply(fraud_clusters_data$hour_of_day, inverse_normalization, "hour_of_day")
fraud_clusters_data$customer_age <- sapply(fraud_clusters_data$customer_age, inverse_normalization, "customer_age")

# 7 Υπολογισμός μέσων τιμών για αυτά τα clusters
fraud_profiles <- fraud_clusters_data %>%
  summarise(
    avg_amount = mean(amt),
    avg_distance = mean(distance_from_home),
    avg_hour = mean(hour_of_day),
    avg_age = mean(customer_age)
  )

# 8 Προβολή του προφίλ των απατεώνων στα συγκεκριμένα clusters
print(fraud_profiles)

# 9 Ανάλυση της κατανομής των χαρακτηριστικών
fraud_clusters_data %>%
  summarise(
    min_amount = min(amt), max_amount = max(amt),
    min_distance = min(distance_from_home), max_distance = max(distance_from_home),
    min_hour = min(hour_of_day), max_hour = max(hour_of_day),
    min_age = min(customer_age), max_age = max(customer_age)
  )

# 10 Οπτικοποιήσεις για patterns απάτης στα συγκεκριμένα clusters

## (γ) Ανάλυση ηλικιών πελατών στα fraud clusters
ggplot(fraud_clusters_data, aes(x = customer_age, fill = as.factor(cluster))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  theme_minimal() +
  labs(title = "Ηλικία Πελάτη και Πιθανότητα Απάτης στα Fraud Clusters", 
       x = "Ηλικία Πελάτη", y = "Συχνότητα", fill = "Cluster")

#------------------------------------------------------------------
# Αξιολόγηση με λογιστική παλινδρόμηση με την προσθήκη της μεβλητής συναλλαγές 
#με την ίδια κάρτα σε μικρό χρονικό διάστημα
#------------------------------------------------------------------
# Προετοιμασία δεδομένων
#------------------------------------------------------------------
# Εγκατάσταση και φόρτωση απαραίτητων πακέτων
install.packages(c("dplyr", "caret", "pROC", "lubridate", "geosphere", "scales"))
library(dplyr)
library(caret)
library(pROC)
library(lubridate)
library(geosphere)
library(scales)

# A. Καθαρισμός του dataset

# 1. Φόρτωση δεδομένων
file_path <- "C:\\Users\\nitse\\OneDrive\\Documents\\fraudTrain.csv"
data <- read.csv(file_path)

# 2. Δειγματοληψία για μείωση μεγέθους δεδομένων με αναλογία 1:10 (Απάτη:Μη Απάτη)
set.seed(123)
fraud_cases <- data %>% filter(is_fraud == 1)
non_fraud_cases <- data %>% filter(is_fraud == 0) %>% sample_n(nrow(fraud_cases) * 10)
data <- bind_rows(fraud_cases, non_fraud_cases) %>% sample_frac(1)

# 3. Αφαίρεση μη χρήσιμων δεδομένων
cols_to_remove <- c("merchant", "first", "last", "street", "city", "zip", "trans_num")
data <- data %>% select(-all_of(cols_to_remove))

# 4. Μετατροπή σε factors όπου χρειάζεται
data <- data %>% mutate(
  is_fraud = as.factor(is_fraud),
  category = as.factor(category),
  gender = as.factor(gender),
  state = as.factor(state)
)

# 5. Δημιουργία νέων μεταβλητών και κανονικοποίηση
data <- data %>%
  mutate(
    trans_date_time = parse_date_time(trans_date_trans_time, orders = "Ymd HMS"),
    hour_of_day = hour(trans_date_time),
    day_of_week = wday(trans_date_time, label = TRUE),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format = "%Y-%m-%d")) / 365.25),
    amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                              labels = c("Low", "Medium", "High", "Very High", "Extreme")))
  )

# 5.1. Υπολογισμός του χρόνου μέχρι να ξαναχρησιμοποιηθεί η ίδια κάρτα (cc_num)
data <- data %>%
  arrange(cc_num, trans_date_time) %>%    # Ταξινόμηση κατά cc_num και χρόνο συναλλαγής
  group_by(cc_num) %>%
  mutate(time_since_last = as.numeric(difftime(trans_date_time, lag(trans_date_time), units = "mins"))) %>%
  ungroup()

# 6. Αφαίρεση ελλιπών δεδομένων (π.χ. για τις πρώτες συναλλαγές κάθε κάρτας, όπου time_since_last είναι NA)
data <- na.omit(data)

# B. Δημιουργία μοντέλου

# 7. Εκπαίδευση μοντέλου λογιστικής παλινδρόμησης
# Συμπεριλαμβάνουμε και την νέα μεταβλητή time_since_last
fraud_model <- glm(is_fraud ~ amt + category + gender + state + hour_of_day + day_of_week + 
                     is_weekend + distance_from_home + customer_age + amt_category + time_since_last,
                   data = data, family = binomial)

# 8. Αξιολόγηση μοντέλου
summary(fraud_model)
# Στο summary θα δείτε το coefficient για τη μεταβλητή time_since_last και το p-value του.
# Εάν το p-value είναι μικρότερο από 0.05, θεωρείται στατιστικά σημαντική.

# Επιπλέον, μπορούμε να συγκρίνουμε το διάστημα (time_since_last) μεταξύ απατηλών και μη απατηλών συναλλαγών.
time_diff_test <- t.test(time_since_last ~ is_fraud, data = data)
cat("Αποτελέσματα t-test για το time_since_last μεταξύ απατηλών και μη απατηλών συναλλαγών:\n")
print(time_diff_test)

# 9. Πρόβλεψη και υπολογισμός AUC
predictions <- predict(fraud_model, type = "response")
auc_score <- auc(data$is_fraud, predictions)
cat("AUC Score:", auc_score, "\n")

# 10. Οπτικοποίηση καμπύλης ROC
roc_curve <- roc(data$is_fraud, predictions)
plot(roc_curve, col = "blue", main = "ROC Curve")

# 11. Υπολογισμός μετρικών απόδοσης
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
conf_matrix <- confusionMatrix(as.factor(predicted_classes), data$is_fraud)
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]
balanced_accuracy <- conf_matrix$byClass["Balanced Accuracy"]

# Δημιουργία πίνακα αποτελεσμάτων
performance_metrics <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Balanced Accuracy", "AUC"),
  Value = c(precision, recall, f1_score, balanced_accuracy, auc_score)
)

# Εμφάνιση του πίνακα αποτελεσμάτων
print(performance_metrics)

# 12. Αποθήκευση καθαρισμένων δεδομένων
output_path <- "C:/Users/nitse/OneDrive/Documents/fraud_oversampled_enhanced.csv"
write.csv(data, output_path, row.names = FALSE)
cat("Το νέο oversampled dataset αποθηκεύτηκε ως fraud_oversampled_enhanced.csv\n")

# 13. Εμφάνιση πλήθους περιπτώσεων απάτης και μη απάτης
fraud_counts <- table(data$is_fraud)
cat("Αριθμός μη απατηλών συναλλαγών (0):", fraud_counts["0"], "\n")
cat("Αριθμός απατηλών συναλλαγών (1):", fraud_counts["1"], "\n")

# 14. Φόρτωση του test set
test_file_path <- "C:/Users/nitse/OneDrive/Documents/fraudTest.csv"
test_data <- read.csv(test_file_path)

# 15. Προεπεξεργασία του test set
test_data <- test_data %>%
  select(-all_of(cols_to_remove)) %>%
  mutate(
    is_fraud = as.factor(is_fraud),
    category = as.factor(category),
    gender = as.factor(gender),
    state = as.factor(state),
    trans_date_time = parse_date_time(trans_date_trans_time, orders = "Ymd HMS"),
    hour_of_day = hour(trans_date_time),
    day_of_week = wday(trans_date_time, label = TRUE),
    is_weekend = ifelse(day_of_week %in% c("Sat", "Sun"), 1, 0),
    distance_from_home = distHaversine(matrix(c(merch_long, merch_lat), ncol = 2),
                                       matrix(c(long, lat), ncol = 2)),
    customer_age = as.numeric(difftime(Sys.Date(), as.Date(dob, format = "%Y-%m-%d")) / 365.25),
    amt_category = factor(cut(amt, breaks = c(0, 50, 100, 500, 1000, Inf),
                              labels = c("Low", "Medium", "High", "Very High", "Extreme")))
  )

# 15.1. Υπολογισμός του time_since_last και στο test set με βάση το cc_num
test_data <- test_data %>%
  arrange(cc_num, trans_date_time) %>%
  group_by(cc_num) %>%
  mutate(time_since_last = as.numeric(difftime(trans_date_time, lag(trans_date_time), units = "mins"))) %>%
  ungroup()

test_data <- na.omit(test_data)

# 16. Πρόβλεψη στο test set
test_predictions <- predict(fraud_model, newdata = test_data, type = "response")
test_predicted_classes <- ifelse(test_predictions > 0.5, 1, 0)

# Αποθήκευση προβλέψεων
write.csv(test_predicted_classes, "C:/Users/nitse/OneDrive/Documents/fraudTest_predictions.csv", row.names = FALSE)
cat("Οι προβλέψεις αποθηκεύτηκαν επιτυχώς στο αρχείο fraudTest_predictions.csv\n")

# 17. Αξιολόγηση απόδοσης στο test set
test_conf_matrix <- confusionMatrix(as.factor(test_predicted_classes), test_data$is_fraud)

# Υπολογισμός βασικών μετρικών απόδοσης
test_precision <- test_conf_matrix$byClass["Precision"]
test_recall <- test_conf_matrix$byClass["Recall"]
test_f1_score <- test_conf_matrix$byClass["F1"]
test_balanced_accuracy <- test_conf_matrix$byClass["Balanced Accuracy"]
test_auc_score <- auc(test_data$is_fraud, test_predictions)

# Δημιουργία πίνακα αποτελεσμάτων για το test set
test_performance_metrics <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Balanced Accuracy", "AUC"),
  Value = c(test_precision, test_recall, test_f1_score, test_balanced_accuracy, test_auc_score)
)

# Εμφάνιση αποτελεσμάτων
print(test_performance_metrics)

# Κατανομή των προβλεφθέντων κλάσεων (απάτες vs μη απάτες)
test_fraud_counts <- table(test_predicted_classes)
cat("Αριθμός προβλεφθέντων μη απατών (0):", test_fraud_counts["0"], "\n")
cat("Αριθμός προβλεφθέντων απατών (1):", test_fraud_counts["1"], "\n")

# 18. Οπτικοποίηση καμπύλης ROC για το test set
test_roc_curve <- roc(test_data$is_fraud, test_predictions)
plot(test_roc_curve, col = "red", main = "ROC Curve - Test Set")

