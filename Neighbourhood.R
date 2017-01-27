library(datasets)
df <- read.csv(file = "C:/Exp/Saratoga NY Homes.txt")
View(df)

crime0 <- na.omit(df)
crime <- data.matrix(crime0)
nrow(crime)

k <- 5

model <- kmeans(crime, k)

str(crime)


kmeans.wss.k <- function(crime, k){
  km = kmeans(crime, k)
  return (km$tot.withinss)
}

kmeans.wss.k(crime,2)


kmeans.dis <- function(crime, maxk){
  dis=(nrow(crime)-1)*sum(apply(crime,2,var))
  dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime=crime)
  return(dis)
   }

maxk <- 20
dis = kmeans.dis(crime, maxk);

plot(1:maxk, dis, type='b', xlab="Number of Clusters (#population groups)", ylab="Within-Cluster Sum of Squares (WCSSS)", col="blue")

install.packages("animation")
library(animation)
cl<- kmeans.ani(crime, 5)
cl$centers

# Plotting the clusters
install.packages("cluster")
library(cluster)
clusplot(crime, model$cluster, main = "clustering analysis", color = TRUE, shade = TRUE, label=5, lines = 0)

# Print the confusion matrix
table(crime[,1], model$cluster)
