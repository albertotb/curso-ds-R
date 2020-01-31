library(caret)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(randomForest)

params <- list(data = "../data/af_clean4.csv",
               levels = c("1-4.1", "1-4.1", "1-4.1", "1-4.1", "4.2", "4.3", "5"))

data <- read_csv(params$data, 
                 col_types = cols(
                   Diabetes = col_logical(),
                   Hypertension = col_logical(),
                   Medication = col_logical(),
                   Hypercholesterolemia = col_logical(),
                   Depression = col_logical(),
                   Anxiety = col_logical(),
                   Prediabetes  = col_logical(),
                   Season_of_birth = col_factor(NULL)))

sleep    <- c("Sleep_quality", "Sleep_duration", "Sleep_start", "Sleep_satisfaction")
control  <- c("Sex", "Age", "BMI", "Smoker", "Year", "Month", "Province")
response <- c("Medication", "Hypercholesterolemia", "Diabetes", "Hypertension", "Depression", "Anxiety")

data$PA                 <- as.factor(data$PA)
levels(data$PA)         <- params$levels
data[c(control, sleep)] <- lapply(data[c(control, sleep)], factor)
data$BMI                <- relevel(data$BMI, "normal")
data$Smoker             <- relevel(data$Smoker, "non-smoker")
data$Sleep_duration     <- relevel(data$Sleep_duration, "6-9h")

df <- data %>%
  mutate(Diabetes1 = as.factor(as.logical(Diabetes1)),
         Diabetes0 = as.factor(as.logical(Diabetes0)))

print(confusionMatrix(df$Diabetes1, df$Diabetes0, positive="TRUE"))

vars  <- c("Sex", "Age", "BMI", "Smoker", "Year", "Month", "Province", 
           "Month_of_birth", "Season_of_birth", "Colesterol", "Glucose", 
           "SAT", "DAT", "PA", "Prediabetes")

df <- filter(df, Diabetes == FALSE) %>%
  select(vars, Diabetes1)

formula <- as.formula(paste("Diabetes1", paste(vars, collapse="+"), sep="~"))

idx <- createDataPartition(df$Diabetes1, list=FALSE)

train <- df[idx, ]
test <- df[-idx, ]

X <- as.data.frame(select(train, -Diabetes1))
y <- train$Diabetes1