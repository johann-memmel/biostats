
#For updating r version
install.packages("installr"); library(installr) # install+load installr
updateR() # updating R.


#used for auto finding the best transformation to normalize the data
install.packages("bestNormalize")

#used for checking how good your linear regesion is
install.packages("gvlma")
library(gvlma)


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

