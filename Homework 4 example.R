# Birth weight logistic regression example
# Read in the birthweight.csv file

my.data<-read.csv(file.choose())

##Question 3###
#Create mosaic plots for categorical variables
if (!require("vcd")) install.packages("vcd")
library(vcd)
mosaic(lowbwt~smoker, data=my.data)


#Create scatterplots for continuous variables
if (!require("popbio")) install.packages("popbio")
library(popbio)
my.data.nona<-na.omit(data.frame("age"=my.data$motherage,"birthweight"=my.data$lowbwt))
logi.hist.plot(my.data.nona$age,my.data.nona$birthweight,boxp=FALSE,type="hist",col="gray", xlabel="Age")

###Question 4###
#automatic selection procedure
if (!require("bestglm")) install.packages("bestglm")
library(bestglm)
my.variables=data.frame("age"=my.data$motherage,"smoker"=my.data$smoker,"mothercigs"=my.data$mnocig,"motherweight"=my.data$mppwt,"Gestation"=my.data$Gestation,"lowbwt"=my.data$lowbwt)
my.variables.nona=na.omit(my.variables)  #get rid of observations with NA
bestglm(my.variables.nona,IC="AIC",family=binomial) #response variable must be last column in dataframe

###Question 5###
#fit best model
model1<-glm(lowbwt~smoker+motherweight+Gestation, data=my.variables.nona)
summary.lm(model1)

###Question 6###
#purposeful selection e.g.
univariate.smoker=glm(lowbwt~smoker, data=my.variables.nona, family=binomial(link="logit"))
summary(univariate.smoker)
univariate.mothercigs=glm(lowbwt~mothercigs, data=my.variables.nona, family=binomial(link="logit"))
summary(univariate.mothercigs)
univariate.weight=glm(lowbwt~motherweight, data=my.variables.nona, family=binomial(link="logit"))
summary(univariate.weight)
univariate.gestation=glm(lowbwt~Gestation, data=my.variables.nona, family=binomial(link="logit"))
summary(univariate.gestation)
univariate.age=glm(lowbwt~age, data=my.variables.nona, family=binomial(link="logit"))
summary(univariate.age)

#include anything with p<0.25 in above univariate regressions
model2<-glm(lowbwt~smoker+motherweight+Gestation, data=my.variables.nona, family=binomial(link="logit"))
summary(model2)

#try a simpler model without motherweight
model3<-glm(lowbwt~smoker+Gestation, data=my.variables.nona, family=binomial(link="logit"))
summary(model3)

#compare models, maximize log likelihood for best model
library(lmtest)
lrtest(model2,model3)

###Question 8###
#convenient way to view effects for the best model
library(effects)
plot(allEffects(model1))



###Question 9###
library(car)
#note you are looking to make sure there is a linear relationship (fitted green line) and to examine plots to 
#see if there are any differences in the variability of residuals as the value for each predictor variable increases.
residualPlots(model1)

#check for studentized residuals with a Bonferonni p<0.05
outlierTest(model1)

#Test for leverage. Look at hat values plot that indicate leverage
#Note that id.n=3 means that it will pick out the three values furthest from the average
influenceIndexPlot(model1, id.n=3)

#test for influential observations. If removal of an observation causes substantial change in the estimates of coefficient, it is called influential observation. Influence can be thought of as the product of 
#leverage and outlier (e.g., it has high hat value and response value is unusual conditional on covariate pattern)
influencePlot(model1)

#Examine relationship between predictors. Is there any multicollinearity?
#The general rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 
#are signs of serious multicollinearity requiring correction.
vif(model1)



###Question 11###
#Test ability of model to accurate classify birthweigth into low/high
#k-fold cross validation 
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
my.variables.nona$lowbwt=as.factor(my.variables.nona$lowbwt)
train(lowbwt ~smoker+motherweight+Gestation,data=my.variables.nona, method="glm", family=binomial(link='logit'),
                 trControl = ctrl, tuneLength = 5)

###Question 12###
predictions<-predict(model1, newdata=my.variables.nona,type="response")
confusionMatrix(data=as.numeric(predictions>0.5),reference=my.variables.nona$lowbwt)



train <- data.frame(LoanStatus_B = as.numeric(rnorm(100)>0.5), b= rnorm(100), c = rnorm(100), d = rnorm(100))
logitMod <- glm(LoanStatus_B ~ ., data=train, family=binomial(link="logit"))
pdata <- predict(logitMod, newdata = train, type = "response")

# use caret and compute a confusion matrix
confusionMatrix(data = as.numeric(pdata>0.5), reference = train$LoanStatus_B)

