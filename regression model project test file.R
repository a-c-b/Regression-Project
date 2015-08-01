
data(mtcars)
```{r, print = FALSE, cache=TRUE}
#### load requisite libraries
library(ggplot2)
library(dplyr)

#  load the dataset
data(mtcars)
dim(mtcars)
head(mtcars)
names(mtcars)
summary(mtcars)
```
mtcars1<-mtcars[,c(1:3,5,6,8:11)]
fit<-lm(mpg ~ factor(am), data = mtcars)
fit1<-lm(mpg ~ factor(am), data = mtcars1)
anova(fit, fit1)


plot(mpg ~ factor(am), data = mtcars)
model1<-lm(mpg ~ factor(am), data = mtcars) # returns the same value as am not a factor
> 7.245+2*1.764
[1] 10.773
> 7.245-2*1.764
[1] 3.717

 summary(lm(mpg ~ factor(am), data = mtcars))
Call:
        lm(formula = mpg ~ factor(am), data = mtcars)

Residuals:
        Min      1Q  Median      3Q     Max 
-9.3923 -3.0923 -0.2974  3.2439  9.5077 

Coefficients:
              Estimate        Std. Error      t value         Pr(>|t|)    
(Intercept)   17.147          1.125           15.247          1.13e-15 ***
factor(am)    1               7.245           1.764           4.106 0.000285 ***
        ---
        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.902 on 30 degrees of freedom
Multiple R-squared:  0.3598,        Adjusted R-squared:  0.3385 
F-statistic: 16.86 on 1 and 30 DF,  p-value: 0.000285



95 per cent confidence interval does not include zero.  
Thus, we can reject the null hypothesis that there is no association
between mpg and transmission type.
Adjusted R^2 = 34% (.3385) so 66 percent of the variation in mpg remains unexplained.  
There are clearly factors other than height which should help us understand
the effect on mpg.

coef(modelAm)
(Intercept)          am 
17.147368    7.244939 

We estimate a difference between automatic and manual transmission of 7.25 mpg with a base line of 17.15 mpg.
fit2 <- lm(mpg ~ I(am - mean(am)),  data = mtcars)
coef(fit2)
(Intercept) I(am - mean(am)) 
20.090625         7.244939 

Thus $20.09 is the expected mpg for the average transmission of the data (7.25 mpg)

summary(lm(mpg ~ wt, data = mtcars))
Call:
        lm(formula = mpg ~ wt, data = mtcars)

Residuals:
        Min      1Q  Median      3Q     Max 
-4.5432 -2.3647 -0.1252  1.4096  6.8727 

Coefficients:
        Estimate Std. Error t value Pr(>|t|)    
(Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
        wt           -5.3445     0.5591  -9.559 1.29e-10 ***
        ---
        Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.046 on 30 degrees of freedom
Multiple R-squared:  0.7528,        Adjusted R-squared:  0.7446 
F-statistic: 91.38 on 1 and 30 DF,  p-value: 1.294e-10





## check the correlation between all the values & mpg
cor.mtcars<-data.frame(cor(mtcars))
View(cor.mtcars)
carinf<-row.names(cor.mtcars)
mpg.cor<-cbind(carinf,cor.mtcars[,1])
mpg.cor<-data.frame(cbind(carinf,cor.mtcars[,1]))
View(mpg.cor)





#file for automatic transmission
AMT<-mtcars[which(mtcars$am==0),]
#subsetted for only the values being looked at in the latest model
# cylinder, displacement, weight
AMT1<-AMT[,c(1:3,5)]
am + wt+ cyl + disp
#file for manual transmission
MAT<-mtcars[which(mtcars$am==1),]
MAT1<-MAT[,c(1:3,5)]
mean(AMT$mpg)
mean(MAT$mpg)
mean(mtcars[which(mtcars$am==1),])


fit2<-lm(mpg ~ am + wt+ cyl + disp, data = mtcars)
anova(fit1, fit2)
anova(lm(mpg~am, data = mtcars),lm(mpg~am + wt, data = mtcars) )
anova(lm(mpg~am + wt, data = mtcars1),lm(mpg~am + wt + cyl, data = mtcars1) )
anova(lm(mpg~am + wt + cyl, data = mtcars1),lm(mpg~am + wt + cyl + disp, data = mtcars1) )

# Analysis of Variance Table
# 
# Model 1: mpg ~ am
# Model 2: mpg ~ am + wt
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     30 720.90                                  
# 2     29 278.32  1    442.58 46.115 1.867e-07 ***
#         ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # Analysis of Variance Table
# 
# Model 1: mpg ~ am + wt
# Model 2: mpg ~ am + wt + cyl
# Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
# 1     29 278.32                                
# 2     28 191.05  1    87.273 12.791 0.001292 **
#         ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > anova(lm(mpg~am + wt + cyl, data = mtcars1),lm(mpg~am + wt + cyl + disp, data = mtcars1) )
# Analysis of Variance Table
# 
# Model 1: mpg ~ am + wt + cyl
# Model 2: mpg ~ am + wt + cyl + disp
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     28 191.05                           
# 2     27 188.43  1    2.6212 0.3756 0.5451
# 
# 

summary(lm(formula = mpg ~ am + wt+ cyl, data = mtcars))
summary(lm(formula = mpg ~ am + wt+ cyl + disp, data = mtcars))









plot(mpg ~ factor(am), data = mtcars)


testAll<-cor(mtcars)
testAll[,1]
testAMT<-cor(AMT) #  WEIGHT, cyl, disp
testAMT[,1]
testMAT<-cor(MAT)
test[,1]


# findam<- lm(mpg ~ am, mtcars)
# findAll<- lm(mpg ~ ., mtcars)
# step(findam, scope=list(upper=findAll), direction="both")
# 
# findAll<- lm(formula = mpg ~ am + wt + disp + cyl + hp + gear + carb + drat, data = mtcars)
# step(findam, scope=list(upper=findAll), direction="both")

fit<-lm(formula = mpg ~ am + wt+ cyl + disp, data = mtcars)



## for automatic transmissions
fit0<-lm(formula = mpg ~ ., am == 0, data = mtcars)
## for manual transmissions
fit1<-lm(formula = mpg ~ ., am == 1, data = mtcars)
summary(fit)
summary(fit0)
summary(fit1)

par(mfrow = c(1,3))
plot(fit, which = 1, main = "Auto & Manual Trans")
plot(fit0, which = 1, main = "Automatic Trans")
plot(fit1, which = 1, main = "Manual Trans")





The correlations show that the lower amount of weight, cylinders, and displacement we should expect an increase in the mpg.  These are just the top 3 factors  
mpg        cyl       disp       drat         wt         vs         am       gear       carb 
1.0000000 -0.8521620 -0.8475514  0.6811719 -0.8676594  0.6640389  0.5998324  0.4802848 -0.5509251 


















##########Throw Away





findam<- lm(formula = mpg ~ am + wt + disp + cyl + hp + gear + carb + drat, data = AMT)
findAll<- lm(mpg ~ ., AMT)
step(findam, scope=list(upper=findAll), direction="both")
#lm(formula = mpg ~ disp + hp + gear + carb, data = AMT)

findam<- lm(mpg ~ am, MAT)
findAll<- lm(mpg ~ ., MAT)
step(findam, scope=list(upper=findAll), direction="both")
#qsec appears


findam<- lm(formula = mpg ~ am + wt + disp + cyl + hp + gear + carb + drat, am == 1, data = mtcars)
step(findam, scope=list(upper=fit), direction="both")


findam<- lm(formula = mpg ~ am + wt + disp + cyl + hp + gear + carb + drat, am == 1, data = mtcars)
step(findam, scope=list(upper=fit), direction="both")






mdl<- 
abline(mdl, lwd=3, col='red')
plot(mdl)



summary(fit)$coefficients

summary(lm(mpg ~ am, data = mtcars))


summary(lm(mpg ~ factor(cyl), data = mtcars))
lm(mpg ~ factor(wt), data = mtcars)


summary(lm(formula = mpg ~ I(1/disp) + factor(am), data = mtcars))

plot.new() 
with(mtcars, plot(1/disp,mpg, col = factor(am))) 
fit <- lm(mpg~I(1/disp),mtcars) 
abline(fit) 



lm(mpg ~ factor(am), data = mtcars)
lm(mpg ~ factor(am), data = mtcars)
lm(mpg ~ factor(am), data = mtcars)
lm(mpg ~ factor(am), data = mtcars)
lm(mpg ~ factor(am), data = mtcars)

require(graphics)
pairs(mtcars, panel = panel.smooth, main = "Car Data", col = 3 + (mtcars$mpg > 0))
summary(lm(mpg ~ . , data = mtcars))
