`install.packages("cluster")
install.packages("readxl")
install.packages("factoextra")

library(cluster)
library(readxl)
library(factoextra)

date <- read_excel("ICE.xlsx")  
tari <- date$Tara               
date1 <- date[ , sapply(date, is.numeric)]  
rownames(date1) <- tari

date1 <- scale(date1)

# Clusterizare fuzzy cu `fanny()` (alegem 3 clustere)
res.fanny <- fanny(date1, k = 3, metric = "euclidean", stand = TRUE, memb.exp = 1.5)

head(res.fanny$membership, 5)  # Primele 5 grade de apartenență
res.fanny$coeff               # Coeficientul Dunn (calitatea clusterizării)

# Vizualizare clustere (cu etichete)
fviz_cluster(res.fanny, 
             ellipse.type = "norm", 
             repel = TRUE, 
             palette = "jco", 
             ggtheme = theme_minimal(), 
             legend = "right")

# Graficul siluetă
fviz_silhouette(res.fanny, 
                palette = "jco", 
                ggtheme = theme_minimal())

# Rezultatul clusterizării (țară + cluster)
rezultate <- data.frame(Tara = tari, Cluster = res.fanny$clustering)
print(rezultate)

# Silueta individuală
head(res.fanny$silinfo$widths)

