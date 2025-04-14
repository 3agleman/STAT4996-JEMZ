# Load required libraries
library(dplyr)

# Select and rename variables
povertyDepData <- puf2023_102124[, c(
  "AMDELT", "POVERTY3", "NEWRACE2", "IRHHSIZ2", "MOVSINPYR2", 
  "MEDICARE", "CAIDCHIP", "CHAMPUS", "PRVHLTIN", "GRPHLTIN", 
  "IREDUHIGHST2", "WRKSTATWK2", 
  "CIGREC", "HLTINALC", "HLTINDRG", "CIGREC"
)]

names(povertyDepData) <- c(
  "lifetimeDepression", "povertyThreshold", "race", "householdSize", "timesMovedPastYear",
  "medicare", "medicaid_chip", "govtHealthcare", "privateHealthcare", "employer_unionHealthcare",
  "highestEducationCompleted", "workStatusPastWeek",
  "LastSmoke", "AlcHealthCare", "DrgHealthCare", "timeSinceLastCigarette"
)

povertyDepData <- povertyDepData %>%
  filter(!is.na(lifetimeDepression)) %>%
  mutate(
    class = factor(ifelse(lifetimeDepression == 1, "Yes", "No")),
    highestEducationCompleted = as.numeric(highestEducationCompleted),
    timesMovedPastYear = as.numeric(timesMovedPastYear),
    race = as.factor(race),
    medicare = as.factor(medicare),
    timeSinceLastCigarette = factor(
      timeSinceLastCigarette,
      levels = c(1, 2, 3, 4, 91),
      labels = c("Past 30 Days", "Past Year", "Past 3 Years", "More than 3 Years", "Never Used")
    )
  ) %>%
  mutate(
    `timeSinceLastCigarettePast 30 Days` = ifelse(timeSinceLastCigarette == "Past 30 Days", 1, 0),
    `timeSinceLastCigarettePast 3 Years` = ifelse(timeSinceLastCigarette == "Past 3 Years", 1, 0),
    `timeSinceLastCigaretteMore than 3 Years` = ifelse(timeSinceLastCigarette == "More than 3 Years", 1, 0),
    LastSmoke = ifelse(timeSinceLastCigarette != "Never Used", 1, 0)
  )

# Remove rows with NA in predictors (optional but helps model run smoothly)
povertyDepData <- povertyDepData %>%
  filter(
    !is.na(timesMovedPastYear),
    !is.na(highestEducationCompleted),
    !is.na(medicare),
    !is.na(AlcHealthCare),
    !is.na(DrgHealthCare)
  )

# Train-test split
set.seed(123)
sample_size <- floor(0.7 * nrow(povertyDepData))
train_indices <- sample(seq_len(nrow(povertyDepData)), sample_size)
train_data <- povertyDepData[train_indices, ]
test_data <- povertyDepData[-train_indices, ]

# Fit logistic regression
logistic_model <- glm(
  class ~ `timeSinceLastCigarettePast 3 Years` + AlcHealthCare + medicare + race + DrgHealthCare +
    `timeSinceLastCigarettePast 30 Days` + highestEducationCompleted +
    `timeSinceLastCigaretteMore than 3 Years` + LastSmoke + timesMovedPastYear,
  data = train_data,
  family = binomial(link = "logit")
)

# Predict with custom threshold (e.g., 0.2)
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")
threshold <- 0.21
predicted_class <- ifelse(predicted_probs > threshold, "Yes", "No")

# Evaluate performance
confusion_matrix <- table(Actual = test_data$class, Predicted = predicted_class)
print("Confusion Matrix:")
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

FP <- confusion_matrix["No", "Yes"]
TN <- confusion_matrix["No", "No"]
FN <- confusion_matrix["Yes", "No"]
TP <- confusion_matrix["Yes", "Yes"]

FPR <- FP / (FP + TN)
FNR <- FN / (FN + TP)

print(paste("False Positive Rate:", round(FPR, 4)))
print(paste("False Negative Rate:", round(FNR, 4)))