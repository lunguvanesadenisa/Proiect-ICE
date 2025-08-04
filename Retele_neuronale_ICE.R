install.packages("neuralnet")
install.packages("readxl")
install.packages("caTools")

library(neuralnet)
library(readxl)
library(caTools)

data <- read_excel("ICE.xlsx")
data <- na.omit(data)

# Verificare existență coloană
if (!"Nivel educațional general al țării" %in% colnames(data)) stop("Coloana 'Nivel educațional general al țării' nu există în fișier!")

# Transformare clasă multiclasa în one-hot encoding
data$C1 <- ifelse(data$`Nivel educațional general al țării` == 1, 1, 0)
data$C2 <- ifelse(data$`Nivel educațional general al țării` == 2, 1, 0)
data$C3 <- ifelse(data$`Nivel educațional general al țării` == 3, 1, 0)

# Selectăm doar coloane numerice utile 
data_num <- data[sapply(data, is.numeric)]
data_num <- subset(data_num, select = -c(`Nivel educațional general al țării`))

# Standardizare
minim <- apply(data_num, 2, min)
maxim <- apply(data_num, 2, max)
data_scaled <- as.data.frame(scale(data_num, center = minim, scale = maxim - minim))
names(data_scaled) <- make.names(names(data_scaled))
data_ready <- cbind(data_scaled, C1 = data$C1, C2 = data$C2, C3 = data$C3)

# Împărțim în train/test
set.seed(123)
split <- sample.split(data_ready$C1, SplitRatio = 0.75)
train_data <- subset(data_ready, split == TRUE)
test_data <- subset(data_ready, split == FALSE)

# Construim formula rețelei neuronale
names(data_scaled) <- make.names(names(data_scaled))
input_vars <- names(data_scaled)
formula <- as.formula(paste("C1 + C2 + C3 ~", paste(input_vars, collapse = " + ")))

# Antrenare rețea neuronală
model <- neuralnet(formula, data = data_ready, hidden = 5, linear.output = FALSE, threshold = 0.01)
plot(model)

# Predicții
pred <- compute(model, test_data[, input_vars])$net.result
pred
predicted_class <- apply(pred, 1, which.max)
predicted_class
actual_class <- apply(test_data[, c("C1", "C2", "C3")], 1, which.max)
actual_class

# Evaluare
conf_matrix <- table(Actual = actual_class, Predicted = predicted_class)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

print(conf_matrix)
cat(" Acuratețea rețelei pentru clasificare în 3 clase (nivel educațional):", round(accuracy * 100, 2), "%\n")

