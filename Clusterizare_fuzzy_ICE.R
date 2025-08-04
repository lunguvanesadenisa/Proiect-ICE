install.packages("cluster")
install.packages("readxl")
install.packages("factoextra")

library(cluster)
library(readxl)
library(factoextra)

# 2. Încărcare date
date <- read_excel("ICE.xlsx")  
tari <- date$Tara               
date1 <- date[ , sapply(date, is.numeric)]  
rownames(date1) <- tari

# 4.Standardizare
date1 <- scale(date1)

# 5. Clusterizare fuzzy cu `fanny()` (alegem 3 clustere)
res.fanny <- fanny(date1, k = 3, metric = "euclidean", stand = TRUE, memb.exp = 1.5)

# 6. Afișare rezultate
head(res.fanny$membership, 5)  # Primele 5 grade de apartenență
res.fanny$coeff               # Coeficientul Dunn (calitatea clusterizării)

# 7. Vizualizare clustere (cu etichete)
fviz_cluster(res.fanny, 
             ellipse.type = "norm", 
             repel = TRUE, 
             palette = "jco", 
             ggtheme = theme_minimal(), 
             legend = "right")

# 8. Analiza siluetei (calitatea fiecărui obiect și globală)
fviz_silhouette(res.fanny, 
                palette = "jco", 
                ggtheme = theme_minimal())

# 9. Rezultatul clusterizării (țară + cluster)
rezultate <- data.frame(Tara = tari, Cluster = res.fanny$clustering)
print(rezultate)

# 10. Silueta individuală (pentru interpretări avansate)
head(res.fanny$silinfo$widths)
