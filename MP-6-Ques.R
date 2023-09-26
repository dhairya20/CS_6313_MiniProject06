# STATS-Mini-Project-6
# Setting working directory to Proj-6 folder.
setwd("C:/Users/dpd140130.CAMPUS/OneDrive - The University of Texas at Dallas/CS 6313/Projects/06/Shalin/Mini-Proj-6")
getwd()
library(BSDA)

# Exploring our .csv file
data <- read.csv("prostate_cancer.csv")
data
data$vesinv<-as.factor(data$vesinv)
us_targ_var <- data$psa
us_predict_var <- data[,-c(1,2,7)]


scaled_tarvar <- scale(us_targ_var)
scaled_predctors <- scale(us_predict_var)

scaled_inp <- cbind(scaled_predctors, vesinv = data$vesinv)
model <- lm(scaled_tarvar ~ scaled_inp)
summary(model)

# final regression function G(xi)
model1<-lm(log(psa) ~ log(cancervol)+vesinv+gleason+benpros*log(weight)+log(age),data= data)
summary(model1)
test_data <- data.frame(mean((data$cancervol)),factor(0),mean(data$gleason),mean(data$benpros),mean(log(data$age)),mean(log(data$age)))
res <- predict(model1, test_data)
