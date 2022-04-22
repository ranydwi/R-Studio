leukimia<-leukemia_remission
y<-leukimia$REMISS
as.numeric(y)
x1<-leukimia$CELL
x2<-leukimia$SMEAR
x3<-leukimia$INFIL
x4<-leukimia$LI
x5<-leukimia$BLAST
x6<-leukimia$TEMP
model.1 <- glm(y ~ x1 + x2+ x3 + x4 + x5 + x6, family="binomial")
summary(model.1)

#confidence interval based on asymptotic normality
confint.default(model.1)
#confidence interval based on profiling the least-squares estimation surface
confint(model.1)

library(MASS)
backward<-stepAIC(model.1,direction="backward")
backward$anova

model.2 <- glm(y ~ x1 + x4 + x6, family="binomial")
summary(model.2)

model.3<-glm(y~x4, family=binomial)
summary(model.3)

plot(x=x4, y,
     panel.last = lines(sort(x4), fitted(model.3)[order(x4)]), main = "Binary Fitted Line Plot")

#odds ratio
exp(coef(model.3)[2])
# 95% CI
exp(confint.default(model.3)[2,])

#
anova(model.3, test="Chisq")

#godness of fit test
#type deviance
sum(residuals(model.3, type="deviance")^2)
model.3$deviance
#type pearson
sum(residuals(model.3, type="pearson")^2)
#type  Hosmer and Lemeshow goodness of fit (GOF) test
library(ResourceSelection)
hoslem.test(model.3$y, fitted(model.3), g=9)

#r-square
1-model.3$deviance/model.3$null.deviance

#pearson and deviance residual
plot(1:27, residuals(model.3, type="pearson"), type="b")
plot(1:27, residuals(model.3, type="deviance"), type="b")

summary(influence.measures(model.3))

hatvalues(model.3)[8] # 0.1498395
residuals(model.3)[8] # -1.944852
rstudent(model.3)[8] # -2.185013
cooks.distance(model.3)[8] # 0.5833219

#Accuracy
library(caret)
library(dplyr)
library(magrittr)
trainIndex <- createDataPartition(leukemia_remission$REMISS, p = .67, list = FALSE, times = 1)
Train <- leukemia_remission[ trainIndex,]
Test <- leukemia_remission[-trainIndex,]
model <- glm(REMISS~ LI, 
             data = Train, family=binomial)

Test$model_prob <- predict(model, Test, type = "response")
Test<- Test %>% mutate(model_pred = 1*(model_prob > .65) + 0, visit_binary = 1*(REMISS == "2") + 0)
Test <- Test %>% mutate(accurate = 1*(model_pred == visit_binary))
sum(Test$accurate)/nrow(Test)
