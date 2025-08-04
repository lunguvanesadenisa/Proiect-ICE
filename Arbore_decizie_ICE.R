library(readxl)
library(rpart)
library(rpart.plot)
library(pROC)
library(caret)

# Citirea fișierului Excel
educatie <- read_excel("ICE.xlsx")

# Preprocesare
educatie$`Nivel educațional general al țării` <- as.factor(educatie$`Nivel educațional general al țării`)
educatie <- na.omit(educatie)
educatie <- educatie[ , !names(educatie) %in% c("Tara")]

# Împărțirea în set de antrenare și testare
set.seed(123)
split <- createDataPartition(y = educatie$`Nivel educațional general al țării`, p = 0.5, list = FALSE)
train <- educatie[split, ]
test <- educatie[-split, ]

# Construirea arborelui de decizie
arbore <- rpart(`Nivel educațional general al țării` ~ ., data = train, method = "class")

# Vizualizarea arborelui
rpart.plot(arbore, extra = 104)

# Predictii și matrice de confuzie
predictii <- predict(arbore, test, type = "class")
print(paste("Eroare clasificare:", mean(predictii != test$`Nivel educațional general al țării`)))
#matricea de confuzie
confuzie<-table(test$`Nivel educațional general al țării`, predictii)
confuzie
predictie1<-predict(arbore, test, type="prob")
predictie1

# Curățarea arborelui (Pruning)
plotcp(arbore)
cp_min <- arbore$cptable[which.min(arbore$cptable[,"xerror"]),"CP"]
cp_min
arbore_curatat <- prune(arbore, cp = cp_min)

# Vizualizarea arborelui curatat
rpart.plot(arbore_curatat, extra = 104)

# Evaluare pe arborele curățat
predictii2 <- predict(arbore_curatat, test, type = "class")
confuzie2<-(table(test$`Nivel educațional general al țării`, predictii2))
print(paste("Eroare clasificare arbore curățat:", mean(predictii2 != test$`Nivel educațional general al țării`)))
library(pROC)
library(ggplot2)

# Obținem probabilitățile pentru toate clasele
predictii_prob <- predict(arbore, test, type = "prob")

# Listă pentru a stoca curbele ROC
roc_list <- list()

# Generăm curbe ROC pentru fiecare clasă (One-vs-Rest)
for (clasa in colnames(predictii_prob)) {
  roc_obj <- roc(
    response = as.numeric(test$`Nivel educațional general al țării` == clasa),
    predictor = predictii_prob[, clasa]
  )
  roc_list[[clasa]] <- roc_obj
}

# Plotăm toate curbele ROC
plot(roc_list[[1]], col = "red", main = "Curbe ROC One-vs-Rest")
for (i in 2:length(roc_list)) {
  lines(roc_list[[i]], col = i + 1)  # Culori diferite pentru fiecare clasă
}
legend("bottomright", legend = names(roc_list), col = 2:(length(roc_list)+1), lty = 1)

# Afișăm AUC pentru fiecare clasă
sapply(roc_list, auc)
