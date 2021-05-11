#Validasi Cluster

#input data
data=read.csv("D:\\KULIAH\\Lomba\\Lomba KTI Cirebon\\data fix\\dieditdatanya.csv")

#recode data
crime <- data [1:33,c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12")]
rownames(crime) <- data$ID[1:33]
head(crime)

# Compute clValid
library(clValid)
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(crime, nClust = 4:8,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)


#input data untuk diagram kipas
data
kriminalitas <- data[,-1]  

#scaling data 
kriminalitas
scale(kriminalitas) 
str(kriminalitas)
summary(kriminalitas)
head(kriminalitas)


#algoritma SOM 
set.seed(1000)
grid <- somgrid(xdim=5, ydim=5, topo="hexagonal")
som.kriminalitas <- som(scale(kriminalitas), grid=somgrid(xdim=5, ydim=5, 
                                                topo="hexagonal"))
str(som.kriminalitas)
plot(som.kriminalitas, type="mapping")
som.kriminalitas$grid$pts
som.kriminalitas$unit.classif
plot(som.kriminalitas)
som.kriminalitas$codes[[1]]
dist(som.kriminalitas$codes[[1]])
som.kriminalitas
summary(som.kriminalitas)

#hclust to 4 cluster
hclust(dist(som.kriminalitas$codes[[1]]))
peta<-cutree(hclust(dist(som.kriminalitas$codes[[1]])),4) 
peta
summary(peta)
#menampilkan plot
plot(peta) 
plot(som.kriminalitas,type="codes",bgcol=rainbow(5)[peta])
add.cluster.boundaries(som.kriminalitas,peta)
kelompok3 <- data.frame(id=data$Provinsi, 
                        cluster=peta[som.kriminalitas$unit.classif])
kelompok3
View(kelompok3)
summary(kelompok3)

