library(readxl)
library(nnet)      
library(caret)
install.packages("tidyverse")
library(tidyverse)

date <- read_excel("ICE.xlsx")

# Selectăm coloanele utile
date_filtrat <- date %>%
  select(`Ani asteptati de scolarizare`,
         `Gradul de inscriere in invatamantul primar`,
         `Gradul de inscriere in invatamantul secundar`,
         `Rata de finalizare a invatamantului primar`,
         `Nivel educațional general al țării`)

date_filtrat <- na.omit(date_filtrat)

# Transformăm variabila țintă într-un factor (etichetăm)
date_filtrat$`Nivel educațional general al țării` <- as.factor(date_filtrat$`Nivel educațional general al țării`)

# Set antrenare/testare
set.seed(123)
ind <- createDataPartition(date_filtrat$`Nivel educațional general al țării`, p = 0.8, list = FALSE)
train_data <- date_filtrat[ind, ]
test_data <- date_filtrat[-ind, ]

# Antrenăm modelul de regresie logistică multinomială
model_multinom <- multinom(`Nivel educațional general al țării` ~ ., data = train_data)
summary(model_multinom)

# Predicții pe setul de testare
predictii <- predict(model_multinom, newdata = test_data)

# Matricea de confuzie
conf_matrix <- confusionMatrix(predictii, test_data$`Nivel educațional general al țării`)
print(conf_matrix)

# Probabilități pentru fiecare predicție
predictii_prob <- predict(model_multinom, newdata = test_data, type = "probs")
head(predictii_prob)

library(pROC)
library(ggplot2)

roc_list <- list()

# Curbe ROC One-vs-Rest pentru fiecare clasă
for (clasa in colnames(predictii_prob)) {
  roc_obj <- roc(
    response = as.numeric(test_data$`Nivel educațional general al țării` == clasa),
    predictor = predictii_prob[, clasa]
  )
  roc_list[[clasa]] <- roc_obj
}

plot(roc_list[[1]], col = "red", main = "Curbe ROC One-vs-Rest")
for (i in 2:length(roc_list)) {
  lines(roc_list[[i]], col = i + 1)  # Culori diferite
}
legend("bottomright", legend = names(roc_list), col = 2:(length(roc_list)+1), lty = 1)

# Afișare AUC pentru fiecare clasă
sapply(roc_list, auc)

