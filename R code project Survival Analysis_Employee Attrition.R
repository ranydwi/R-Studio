library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(survival)

options( warn = -1 )

data<-Data_UAS_Employee_Attrition
data$event <- with(data,ifelse(Attrition=="Yes", 1, 0))
time<-data$YearsAtCompany #define time
event<-data$event #define event
group<-data$OverTime #define groups to compare multiple survival curves
envsatisfaction<-data$EnvironmentSatisfaction
gender<-data$Gender
dept<-data$Department
travel<-data$BusinessTravel
jobsatisfaction<-data$JobSatisfaction
#######
ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)
salary<-data$DailyRate

survival<-Surv(time,event) #creates the survival object
head(survival, n = 15)

model<- survfit(Surv(time,event) ~ 1)
print(model, print.rmean=TRUE)
summary(model)

plot(model, xlab="Time elapsed", ylab="% surviving",main="Survival Plot")

#to compare two survival curves based on a parameter
model_comp<- survfit(Surv(time,event) ~ group)
summary(model_comp)

cat("\n\nMedian values for the survival function by group are:\n")

#estimate the median survival time because this is the time with probability of survival=0.5
summary(model_comp)$table[,'median']

plot(model_comp, xlab="Time elapsed", ylab="% surviving",main="Survival Plot", legend=T, col=c('blue','red'))
legend('bottomleft', c("Overtime=No", "Overtime=Yes"), col=c('blue','red'), lty = 1, bty="n")

#Tests if there is a statistical difference between two or more survival curves using the G-rho family
#of tests, or for a single curve against a known alternative
survdiff(survival ~ group, rho=0)
h1<-survdiff(Surv(time,event)~ group)
h1
h2<-survdiff(Surv(time,event)~envsatisfaction)
h2
h3<-survdiff(Surv(time,event)~ dept)
h3
h4<-survdiff(Surv(time,event)~ travel)
h4
h5<-survdiff(Surv(time,event)~ jobsatisfaction)
h5
