install.packages("neuralnet")
install.packages("readxl")
install.packages("caTools")

library(neuralnet)
library(readxl)
library(caTools)

# 2. Citirea și curățarea datelor
data <- read_excel("ICE.xlsx")
data <- na.omit(data)

# 3. Verificare existență coloană
if (!"Nivel educațional general al țării" %in% colnames(data)) stop("Coloana 'Nivel educațional general al țării' nu există în fișier!")

# 4. Transformare clasă multiclasa în one-hot encoding
data$C1 <- ifelse(data$`Nivel educațional general al țării` == 1, 1, 0)
data$C2 <- ifelse(data$`Nivel educațional general al țării` == 2, 1, 0)
data$C3 <- ifelse(data$`Nivel educațional general al țării` == 3, 1, 0)

# 5. Selectăm doar coloane numerice utile (excludem Tara și nivelul educational original)
data_num <- data[sapply(data, is.numeric)]
data_num <- subset(data_num, select = -c(`Nivel educațional general al țării`))

# 6. Standardizare
minim <- apply(data_num, 2, min)
maxim <- apply(data_num, 2, max)
data_scaled <- as.data.frame(scale(data_num, center = minim, scale = maxim - minim))
names(data_scaled) <- make.names(names(data_scaled))
data_ready <- cbind(data_scaled, C1 = data$C1, C2 = data$C2, C3 = data$C3)

# 8. Împărțim în train/test
set.seed(123)
split <- sample.split(data_ready$C1, SplitRatio = 0.75)
train_data <- subset(data_ready, split == TRUE)
test_data <- subset(data_ready, split == FALSE)

# 9. Construim formula rețelei neuronale
names(data_scaled) <- make.names(names(data_scaled))
input_vars <- names(data_scaled)
formula <- as.formula(paste("C1 + C2 + C3 ~", paste(input_vars, collapse = " + ")))

# 10. Antrenare rețea neuronală
model <- neuralnet(formula, data = data_ready, hidden = 5, linear.output = FALSE, threshold = 0.01)
plot(model)

# 11. Predicții
pred <- compute(model, test_data[, input_vars])$net.result
pred
predicted_class <- apply(pred, 1, which.max)
predicted_class
actual_class <- apply(test_data[, c("C1", "C2", "C3")], 1, which.max)
actual_class

# 12. Evaluare
conf_matrix <- table(Actual = actual_class, Predicted = predicted_class)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

print(conf_matrix)
cat(" Acuratețea rețelei pentru clasificare în 3 clase (nivel educațional):", round(accuracy * 100, 2), "%\n")
