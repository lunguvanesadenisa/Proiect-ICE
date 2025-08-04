library(readxl)
library(nnet)      
library(caret)
install.packages("tidyverse")
library(tidyverse)

# 2. Încarcă datele
date <- read_excel("ICE.xlsx")

# 3. Selectează coloanele utile
date_filtrat <- date %>%
  select(`Ani asteptati de scolarizare`,
         `Gradul de inscriere in invatamantul primar`,
         `Gradul de inscriere in invatamantul secundar`,
         `Rata de finalizare a invatamantului primar`,
         `Nivel educațional general al țării`)

# 4. Elimină rândurile incomplete
date_filtrat <- na.omit(date_filtrat)

# 5. Transformă variabila țintă într-un factor
date_filtrat$`Nivel educațional general al țării` <- as.factor(date_filtrat$`Nivel educațional general al țării`)

# 6. Împarte setul în antrenare/test
set.seed(123)
ind <- createDataPartition(date_filtrat$`Nivel educațional general al țării`, p = 0.8, list = FALSE)
train_data <- date_filtrat[ind, ]
test_data <- date_filtrat[-ind, ]

# 7. Antrenează modelul de regresie logistică multinomială
model_multinom <- multinom(`Nivel educațional general al țării` ~ ., data = train_data)

# 8. Rezumă modelul
summary(model_multinom)

# 9. Preziceri pe setul de testare
predictii <- predict(model_multinom, newdata = test_data)

# 10. Matrice de confuzie
conf_matrix <- confusionMatrix(predictii, test_data$`Nivel educațional general al țării`)
print(conf_matrix)

# 11. Probabilități pentru fiecare clasă 
predictii_prob <- predict(model_multinom, newdata = test_data, type = "probs")
head(predictii_prob)

library(pROC)
library(ggplot2)

# 1. Obține probabilitățile prezise pentru fiecare clasă
predictii_prob <- predict(model_multinom, newdata = test_data, type = "probs")

# 2. Listă pentru a stoca curbele ROC
roc_list <- list()

# 3. Generare curbe ROC One-vs-Rest pentru fiecare clasă
for (clasa in colnames(predictii_prob)) {
  roc_obj <- roc(
    response = as.numeric(test_data$`Nivel educațional general al țării` == clasa),
    predictor = predictii_prob[, clasa]
  )
  roc_list[[clasa]] <- roc_obj
}

# 4. Plotare toate curbele ROC într-un singur grafic
plot(roc_list[[1]], col = "red", main = "Curbe ROC One-vs-Rest")
for (i in 2:length(roc_list)) {
  lines(roc_list[[i]], col = i + 1)  # Culori diferite
}
legend("bottomright", legend = names(roc_list), col = 2:(length(roc_list)+1), lty = 1)

# 5. Afișare AUC pentru fiecare clasă
sapply(roc_list, auc)

