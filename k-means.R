
#kmeans

data(iris)
df <- iris

cluster <-kmeans(df[,1:4], centers=3, iter.max=10, nstart=1)

inertia <- vector(mode="character", length=10)
for (i in 1:10) {
  cluster<-kmeans(df[,1:4], centers=i, iter.max=10, nstart=1)
  inertia[i]<-cluster$tot.withinss
}

plot(1:10, inertia, type="b")