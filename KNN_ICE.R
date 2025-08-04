library(readxl)
library(caret)
library(class)
library(tidyverse)

# 2. Încarcă datele
date <- read_excel("ICE.xlsx")

# 3. Selectează doar coloanele relevante
date_filtrat <- date %>%
  select(`Ani asteptati de scolarizare`,
         `Gradul de inscriere in invatamantul primar`,
         `Gradul de inscriere in invatamantul secundar`,
         `Rata de finalizare a invatamantului primar`,
         `Nivel educațional general al țării`)

# 4. Elimină rânduri cu valori lipsă
date_filtrat <- na.omit(date_filtrat)

# 5. Transformă coloana țintă într-un factor (multi-clasă)
date_filtrat$`Nivel educațional general al țării` <- as.factor(date_filtrat$`Nivel educațional general al țării`)

# 6. Împarte datele în seturi de antrenare/test
set.seed(123)
ind <- createDataPartition(date_filtrat$`Nivel educațional general al țării`, p = 0.8, list = FALSE)
train_data <- date_filtrat[ind, ]
test_data <- date_filtrat[-ind, ]

# 7. Creează modelul KNN
control <- trainControl(method = "cv", number = 10)
model_knn <- train(`Nivel educațional general al țării` ~ ., 
                   data = train_data,
                   method = "knn",
                   trControl = control,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)

# 8. Rezultate model
print(model_knn)
plot(model_knn)

# 9. Preziceri și evaluare
predictii <- predict(model_knn, newdata = test_data)
conf_matrix <- confusionMatrix(predictii, test_data$`Nivel educațional general al țării`)
print(conf_matrix)