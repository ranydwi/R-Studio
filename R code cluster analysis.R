#Cluster Analysis
## Install package
install.packages("cluster")
install.packages("dendextend")
install.packages("factoextra")
## Aktifkan package
library(cluster)
library(dendextend)
library(factoextra)
search()
## Input data
X1 = c(2,8,9,1,8.5)  #makanan
X2 = c(4,2,3,5,1)    #pakaian
data1 = data.frame(X1,X2)
data1
# Memberikan nama baris pada data
rownames(data1) = c("A","B","C","D","E")
data1
## Melihat plot data antar variabel
plot(data1)
text(data1,rownames(data1))
## Matriks jarak Euclidean
dist(data1)
## Cluster hirarki agglomerative dengan teknik complete linkage
hasil = hclust(dist(data1),"complete")
hasil2 = hclust(dist(data1),"single")
hasil3 = hclust(dist(data1),"ave")
hasil
hasil2
hasil3
# Dendrogram
plot(hasil)
plot(hasil2)
plot(hasil3)

##contoh pakai data iklim (hierarchial)
## Uji asumsi multikolinearitas
library(car)
data2<-Data_iklim
cor(data2[,2:4])
## Jarak antar data
dist(data2[,2:4])
## Perbandingan korelasi antar metode hirarki
# Single
d1=dist(data2[,2:4])
hc = hclust(d1,"single")
d2 = cophenetic(hc)
cor.sing = cor (d1,d2)
cor.sing
# Average
d1=dist(data2[,2:4])
hc = hclust(d1,"ave")
d2 = cophenetic(hc)
cor.ave = cor (d1,d2)
cor.ave
# Complete
d1=dist(data2[,2:4])
hc = hclust(d1,"complete")
d2 = cophenetic(hc)
cor.comp = cor (d1,d2)
cor.comp
## Analisis cluster dgn hirarki average
hirarki.ave=hclust(dist(data2[,2:4]),method = "ave")
hirarki.ave
# dendrogram
plot(hirarki.ave, labels = data2$Provinsi)
## Jumlah cluster yg optimal
library(factoextra)
fviz_nbclust(data2[,2:4], hcut, method = "silhouette")
fviz_nbclust(data2[,2:4], hcut, method = "wss")
fviz_nbclust(data2[,2:4], hcut, method = "gap_stat")
## Plot mengelompokkan data
plot(hirarki.ave, labels = data2$Provinsi)
rect.hclust(hirarki.ave, k=2, border=2:3)
## Plot mengelompokkan data
plot(hirarki.ave, labels = data2$Provinsi)
rect.hclust(hirarki.ave, k=3, border=2:3)
## Anggota cluster
hasil.cut = cutree(hirarki.ave,3)
table(hasil.cut)
rownames(data2)[hasil.cut==1]
rownames(data2)[hasil.cut==2]
rownames(data2)[hasil.cut==3]
