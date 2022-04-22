library(DT)     #Menampilkan tabel agar mudah dilihat di browser
library(MVN)    #Uji multivariate normal
library(MASS)   #Fungsi diskriminan analisis
library(biotools) #Melakukan uji Box-M
data("iris")
datatable(iris)
#pengujian asumsi
mvn(data = iris[, c(1:4)], multivariatePlot = 'qq') #hanya mengambil kolom variabel prediktor
boxM(data = iris[, c(1:4)], grouping = iris[,5])
#Terdapat perbedaan rata-rata antar kategori spesies
m <- manova(formula = cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length,
                            iris$Petal.Width) ~ iris$Species)
summary(object = m, test = 'Wilks')

#membagi dataset ke dalam training dan testing
set.seed(123)
train_index <- sample(seq(nrow(iris)), size = floor(0.75 * nrow(iris)), replace = F)
training_data <- iris[train_index, ] 
test_data <- iris[-train_index, ]

#membentuk fungsi diskriminan
linearDA <- lda(formula = Species ~., data = training_data)
linearDA
#plotting
plot(linearDA, col = as.integer(training_data$Species))

#Melakukan prediksi di Test Data dan Menguji Performa Model yang dibuat
predicted <- predict(object = linearDA, newdata = test_data)
table(actual = test_data$Species, predicted = predicted$class)

----------------------------------------------------------
library(DT)     #Menampilkan tabel agar mudah dilihat di browser
library(MVN)    #Uji multivariate normal
library(biotools) #Melakukan uji Box-M
library(dplyr)  #Data processing
insect <-insect
insect
datatable(insect)
#pengujian asumsi
#multivariat normal
mvn(data = insect[, c(2:4)], multivariatePlot = 'qq') #hanya mengambil kolom variabel prediktor
#matriks ragam-peragam antar kategori spesies sama
boxM(data = insect[, c(2:4)], grouping = insect[,1])
#Terdapat perbedaan rata-rata antar kategori spesies
m <- manova(formula = cbind(insect$V2, insect$V3, insect$V4) ~ insect$V1)
summary(object = m, test = 'Wilks')

#Canonical Correlation (CC)
library(candisc)
cc<-candisc(m)
cc

#LINEAR DISCRIMINANT ANALYSIS
#Fit the model
x1<-insect$V2
library(DFA.CANCOR)
library(klaR)
model.full <- lda(insect$V1~., data = insect)
model.full
model.stepwise<-greedy.wilks(x1~., data = insect,niveau=0.05)
model.stepwise<-greedy.wilks(insect$V2~., data = insect,niveau=0.05)
model.stepwise

