# STATS-Mini-Project-6
# Setting working directory to Proj-6 folder.
setwd("C:/Users/dpd140130.CAMPUS/OneDrive - The University of Texas at Dallas/CS 6313/Projects/06")
getwd()
library(BSDA)

# Exploring our .csv file
data <- read.csv("prostate_cancer.csv")
data
data$vesinv<-as.factor(data$vesinv)
model <- lm(psa ~ cancervol + weight + age + benpros + vesinv + capspen + gleason, data = data)
summary(model)
# refinement - 1
ModRef1 <- update (model, .~. -weight)
anova(ModRef1,model)
# refinement - 2
ModRef2 <- update (ModRef1, .~. -capspen)
anova(ModRef2,ModRef1)
# refinement - 3
ModRef3 <- lm(log(psa) ~ log(cancervol)+log(age)+vesinv+benpros*gleason, data = data)
summary(ModRef3)
#data_cv <- log(mean(data$cancervol))#c(1.945722)
#data_age <- log(mean(data$age))#c(4.156787)
#data_combi <- mean(data$benpros)
#Taking mean of the quantitative predictors and most frequent category of the qualitative predictor
test_data <- data.frame(cancervol=log(mean(data$cancervol)), age=log(mean(data$age)),vesinv= factor(0), benpros=(mean(data$benpros)),gleason=(mean(data$gleason)))
test_data

res <- predict(ModRef3, newdata = test_data)
res
exp(res)
