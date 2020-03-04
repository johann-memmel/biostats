
#For updating r version
install.packages("installr"); library(installr) # install+load installr
updateR() # updating R.


#used for auto finding the best transformation to normalize the data
install.packages("bestNormalize")

#used for checking how good your linear regesion is
install.packages("gvlma")
library(gvlma)

library(boot)
library(MASS)
data("UScereal")
UScereal
str(UScereal)
boxplot((UScereal$sugar~as.factor(UScereal$shelf)))
anova.result = aov(UScereal$sugar~as.factor(UScereal$shelf) * mfr, data = UScereal)
summary(anova.result)
TukeyHSD(anova.result)



data("crabs")
View(crabs)
?crabs

anova.result2 = aov(FL~sex * sp, data = crabs)
summary(anova.result2)
TukeyHSD(anova.result2)

interaction.plot(crabs$sp,crabs$sex, crabs$FL)


install.packages("boot")
library(boot)
data("iris")

func <- function(data, indices, cor.type){
  dt<-data[indices,]
  c(
    cor(dt[,1], dt[,2], method=cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}
bootTest = boot(iris, func, R=1500, cor.type='s')


#https://www.datacamp.com/community/tutorials/bootstrap-r
head(bootTest$t)

plot(bootTest, index = 1)

boot.ci(bootTest, index=1)


data("iris")
Beavers = read.csv("Beavers.csv", fileEncoding="UTF-8-BOM")

install.packages("bootstrap")
library(bootstrap)

#jackknife stuff
hist(Beavers$temp)
theta <- function(x){mean(x)}
result = jackknife(Beavers$temp, theta)
abline(v = (mean(result$jack.values)), col = "Red")
abline(v = (mean(result$jack.values) + 1.96* result$jack.se), col = "blue")
abline(v = (mean(result$jack.values) - 1.96 * result$jack.se), col = "blue")
result



xdata = null
xdata = cbind(xdata, Beavers$)
xdata = cbind(xdata, Beavers$)
xdata <- matrix(rnorm(30),ncol=2)
n <- length(Beavers$temp)
theta <- function(x,xdata){ cor(xdata[x,1],xdata[x,2]) }
results <- jackknife(1:n,theta,xdata)




#Bootstrap stuff
theta <- function(x){mean(x)}
result2 = bootstrap(iris$Sepal.Length, 10000, theta)
hist(result2$thetastar)
abline(v = mean(iris$Sepal.Length))
abline(v= mean(result2$thetastar), col = "red")


if (!require("coin")) install.packages("coin")
library(coin)

dataPermutation = read.csv("KillarneyMod.csv", fileEncoding="UTF-8-BOM")

plot(Diversity ~ status, data = dataPermutation)

independence_test(Diversity ~ status, data = dataPermutation)


symmetry_test(Diversity ~ status | Lake, data = dataPermutation)

independence_test(temp ~ activ, data = Beavers)
data("catsM")

