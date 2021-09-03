library(tidyverse)
library(cluster)
library(factoextra)

#input data tanpa nama provinsi
data <- read.csv("~/SSO 2020/Final/data belum scaling.csv")
data
str(data)
head(data)
features = subset(data, select = -c(X, Provinsi.Asal))
features
#input data nama provinsi
namaprov = data$Provinsi.Asal
row.names(features)[namaprov$Provinsi.Asal]
View(features)
rownames(features) = c('Aceh', 'Bali', 'Banten','Bangka Belitung','Bengkulu','DI Yogyakarta','DKI Jakarta','Jambi','Jawa Barat','Jawa Tengah','Jawa Timur','Kalimantan Barat','Kalimantan Timur','Kalimantan Tengah','Kalimantan Selatan','Kalimantan Utara','Kepulauan Riau','Nusa Tenggara Barat','Sumatera Selatan','Sumatera Barat','Sulawesi Utara','Sumatera Utara','Sulawesi Tenggara','Sulawesi Selatan','Sulawesi Tengah','Lampung','Riau','Maluku Utara','Maluku','Papua Barat','Papua','Sulawesi Barat','Nusa Tenggara Timur','Gorontalo')
View(features)
datafix = scale(features)

fviz_nbclust(datafix, kmeans, method = "wss")
fviz_nbclust(datafix, kmeans, method = "silhouette")

set.seed(123)
final <- kmeans(datafix, 4, nstart = 25)
final
fviz_cluster(final, data=datafix)

library(NbClust)
fviz_nbclust(datafix, kmeans, method = c("silhouette", "wss", "gap_stat"))
# Elbow method
fviz_nbclust(datafix, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(datafix, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(datafix, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


set.seed(123)
final <- kmeans(datafix, 4, nstart = 25)
final
fviz_cluster(final, data=datafix)


plot()
