#Lampiran DATA DIAGRAMKIPAS

#input data
data=read.csv("D:\\KULIAH\\Lomba\\Lomba KTI Cirebon\\data fix\\datafix.csv")
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

