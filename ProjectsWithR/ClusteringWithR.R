
str(BathSoap_Data_1_)
summary(BathSoap_Data_1_)

#making an objectcopy

Bath_data <- BathSoap_Data_1_
View(BathSoap_Data_1_)
var1 <- Bath_data[c(12:31)]
summary(var1)
str(var1)

##multiplying subsets with volume
brand_codes <- subset(var1[c(9:20)])
print(brand_codes)

volume <- function(x){
  x <- x*var1$`Total Volume`
  return(x)
}

per_volume <- as.data.frame(lapply(var1[c(9:20)],volume))
str(per_volume)
var1 <-cbind(var1,per_volume)
var2 <- var1[-c(9:20)]

##variable objsct containing variables for 1st question
var3 <- var2[c(1:8,12:20)]
str(var3)
View(var1)
dim(var1)

##max to one brand and share to others
var3$max_to_one <- apply(var3[,9:16],1,max)
str(var3)
View(max_to_one)

##Var_c1 for 1st clustering

var_c1 <- var3[-c(9:16,6,7)]
str(var_c1)
View(var_c1)
##normalizing the variables

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
bath_norm <- as.data.frame(lapply(var_c1[c(1:8)], normalize))

str(bath_norm)
print(bath_norm)
View(bath_norm)

##making clusters for purchase behavior


wss = (nrow(bath_norm)-1)*sum(apply(bath_norm,2,var))
for (i in 2:15){
  set.seed(18)
  wss[i] = sum(kmeans(bath_norm, centers=i)$withinss)} 
plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)


#Considering 6 as clusters
set.seed(12)
km1 <- kmeans(bath_norm, 6, nstart = 100)
km1
km1$withinss
km1$betweenss
km1$totss



# Plot results
plot(bath_norm, col =(km1$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

km1$centers

##comparing for variables
cluster1_instances=bath_norm[km1$cluster==1,]
cluster2_instances=bath_norm[km1$cluster==2,]
cluster3_instances=bath_norm[km1$cluster==3,]
cluster4_instances=bath_norm[km1$cluster==4,]
cluster5_instances=bath_norm[km1$cluster==5,]
cluster6_instances=bath_norm[km1$cluster==6,]


boxplot(bath_norm$No..of..Trans ~ km1$cluster, xlab='Cluster', ylab='No. Of Trans')
boxplot(bath_norm$No..of.Brands ~ km1$cluster, xlab='Cluster', ylab='No..of.Brands')
boxplot(bath_norm$Brand.Runs ~ km1$cluster, xlab='Cluster', ylab='Brand.Runs')
boxplot(bath_norm$Total.Volume ~ km1$cluster, xlab='Cluster', ylab='Total.Volume')
boxplot(bath_norm$Value ~ km1$cluster, xlab='Cluster', ylab='Value')
boxplot(bath_norm$Avg..Price ~ km1$cluster, xlab='Cluster', ylab='Avg..Price')
boxplot(bath_norm$Others.999 ~ km1$cluster, xlab='Cluster', ylab='Others.999')
boxplot(bath_norm$max_to_one ~ km1$cluster, xlab='Cluster', ylab='max_to_one')


##removinng the variables which do not contribute much to clusters
##variables avg prices and no. of trans dont vary much with clusters and thus seem to contribute lesser
bath_norm2=bath_norm[-c(4,6)]

##making clusters after removing the variables


wss = (nrow(bath_norm2)-1)*sum(apply(bath_norm2,2,var))
for (i in 2:15) wss[i] = sum(kmeans(bath_norm2, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)

#Considering 6 as clusters after removing variables
set.seed(13)
km2 <- kmeans(bath_norm2, 6, nstart = 100)
km2

set.seed(77)
km21 <- kmeans(bath_norm2, 5, nstart = 100)
km21

set.seed(78)
km23 <- kmeans(bath_norm2, 7, nstart = 100)
km23
km2$tot.withinss
plot(bath_norm2, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)


##Q1.b begins here..
bas_pur <- BathSoap_Data_1_[c(20:22,32:36)]
str(bas_pur)
dim(bas_pur)
bas_pur <- as.data.frame(lapply(bas_pur,volume))
bas_pur <- as.data.frame(lapply(bas_pur, normalize))

##finding optimum number of clusters

wss3 = (nrow(bas_pur)-1)*sum(apply(bas_pur,2,var))
print(wss3)
for (i in 2:15) wss3[i] = sum(kmeans(bas_pur, centers=i)$withinss)
plot(1:15, wss3, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)

#Considering 5 as clusters 
set.seed(15)
km3 <- kmeans(bas_pur, 6, nstart = 100)
km3
print(wss3)

set.seed(16)
km4 <- kmeans(bas_pur, 7, nstart = 100)
km4



##identifying imp selling prop categories based on number of instances

N <- 15
f <- vector(mode='integer',15)
set.seed(10)
x<- runif(N)
for(i in seq(N)) {
  if (bath_norm2[i] <0.25){ 
    f[i] <- i
  } 
}
print(f)

##not all  variables contribute to a great exten so we decide on a threshold
##aagin we need to convert % into volumes
##removing 9:13 selling props for this data as per conditions>25% and Freq>20

bath_norm3=bas_pur[-c(12:16,18)]
str(bath_norm3)
dim(bath_norm3)
set.seed(18)

wss4 = (nrow(bath_norm3)-1)*sum(apply(bath_norm3,2,var))
print(wss4)
for (i in 2:15) {
  set.seed(20)
  wss4[i] = sum(kmeans(bath_norm3, centers=i)$withinss)
}
plot(1:15, wss4, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)

set.seed(21)
km5 <- kmeans(bath_norm3, 5, nstart = 100)
km5
plot(bath_norm3, col =(km5$cluster +1) , main="K-Means result with 7 clusters", pch=20, cex=2)
str(bath_norm3)

##this gives the best output
set.seed(22)
km6 <- kmeans(bath_norm3, 6, nstart = 100)
km6


set.seed(24)
km7 <- kmeans(bath_norm3, 7, nstart = 100)
km7

plot(bath_norm3, col =(km4$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

View(bath_norm3)

##plotting for both purchase behavuior and basis of purchase
bath_norm4=cbind(bath_norm2,bath_norm3)
str(bath_norm4)
dim(bath_norm4)
View(bath_norm4)
##combining both datasets
wss5 = (nrow(bath_norm4)-1)*sum(apply(bath_norm4,2,var))
print(wss5)
for (i in 2:15) {
  set.seed(25)
  wss5[i] = sum(kmeans(bath_norm4, centers=i)$withinss)
}
plot(1:15, wss5, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", 
     pch=20, cex=2)

set.seed(29)
km8 <- kmeans(bath_norm4, 4, nstart = 100)
km8
set.seed(31)
km9 <- kmeans(bath_norm4, 5, nstart = 100)
km9

set.seed(33)
km10 <- kmeans(bath_norm4, 6, nstart = 100)
km10

set.seed(35)
km11 <- kmeans(bath_norm4, 7, nstart = 100)
km11


####thus k=6 for 1st purchase behavior criteria gives the best results in terms of variance

table(km2$cluster)
Cluster1Inst = BathSoap_Data_1_[km2$cluster == 1, ]
Cluster2Inst = BathSoap_Data_1_[km2$cluster == 2, ]
Cluster3Inst = BathSoap_Data_1_[km2$cluster == 3, ]
Cluster4Inst = BathSoap_Data_1_[km2$cluster == 4, ]
Cluster5Inst = BathSoap_Data_1_[km2$cluster == 5, ]
Cluster6Inst = BathSoap_Data_1_[km2$cluster == 6, ]

table(Cluster1Inst$`No. of Brands`)
table(Cluster1Inst$`Brand Runs`)
table(Cluster1Inst$`Total Volume`)
table(Cluster1Inst$Value)
table(Cluster1Inst)
table(Cluster1Inst$`No. of Brands`)


##means of cluster table for our selected km2
km2$centers

##demographics for sleeted cluster
table(Cluster1Inst$SEC)
table(Cluster1Inst$FEH)
table(Cluster1Inst$MT)
table(Cluster1Inst$SEX)
table(Cluster1Inst$AGE)
table(Cluster1Inst$EDU)
table(Cluster1Inst$HS)
table(Cluster1Inst$CHILD)
table(Cluster1Inst$CS)



BathSoap_Data_1_ <- cbind(km2$cluster)
BathSoap_Data_1_$CHILD
table(BathSoap_Data_1_$CHILD,BathSoap_Data_1_$`km2$cluster`)


str(Bath_data)
Bath_data<-cbind(Bath_data,km2$cluster)
str(BathSoap_Data_1_)

Bath_data$`km2$cluster`<-as.factor(Bath_data$`km2$cluster`)
table(Bath_data$SEC,Bath_data$`km2$cluster`)


boxplot(Bath_data$`Affluence Index`~Bath_data$`km2$cluster`,main="Distribution of Affluence Index across the clusters", xlab="Clusters", ylab="Affluence Index")

boxplot(Bath_data$`PropCat 5`~km2$cluster, main = "Distribution of PropCat5 across the clusters", xlab="Clusters", ylab="PropCat 5")


