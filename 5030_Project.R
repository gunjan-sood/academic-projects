
library(faraway)
library(ISLR)
library(leaps)
library(glmnet)
library(boot)
counties=read.csv("C:/Users/gsood/Desktop/Cornell/HW/5030/Project/counties.csv",header=T,sep=",")
# Numerical summary of the data
head(counties)
summary(counties)

# Checking if there are any missing values in the dataset
sum(is.na(counties))

# graphical summary of the data
pairs(crime~.,data= counties)

# Fitting a linear model with all the predictors in the model
m=lm(crime~.-county-state-region,data=counties)
par(mfrow=c(2,2))
plot(m)
mtext("Plot A: Diagnostics plots for the initial linear model", side = 1, line = -30, outer = TRUE)
summary(m)
par(mfrow=c(1,1))
plot(fitted(m),residuals(m),xlab="Fitted",ylab="Residuals")
abline(h=0)

# Checking correlation among the predictors
counties1 = counties[,-1]
counties2 = counties1[,-1]
counties3=cor(counties2[,-13])
counties3
#utils::View(counties3)

## Comments: There is very high correlation between 1. nhospbeds and pop and 2. nphysician and pop 3. nphysician and nhospbeds
## Removing nphysician as it not even showed as significant predictor in the summary of the first fitted model

m2 = lm(crime~.-county-state-nphysician-region,data=counties)
summary(m2)

# area*
par(mfrow=c(2,2))
# check residual consistency
plot(counties$area,residuals(m2),xlab="area",ylab="Residuals")
# check normality
qqplot(counties$area,counties$crime)
# both residual plot and qqplot suggest that area need a transformation
plot(log(counties$area),residuals(m2),xlab="log-area",ylab="Residuals")
qqplot(log(counties$area),counties$crime)
mtext("Plot B: Diagnostics plots for area before and after log-transformation", side = 1, line = -30, outer = TRUE)

#pop*
par(mfrow=c(2,2))
# check residual consistency
plot(counties$pop,residuals(m2),xlab="pop",ylab="Residuals")
# check normality
qqplot(counties$pop,counties$crime)
# both residual plot and qqplot suggest that pop need a transformation
plot(log(counties$pop),residuals(m2),xlab="pop",ylab="Residuals")
qqplot(log(counties$pop),counties$crime)
mtext("Plot C: Diagnostics plots for pop before and after log-transformation", side = 1, line = -30, outer = TRUE)

# p18_25
par(mfrow=c(1,2))
# check residual consistency
plot(counties$p18_25,residuals(m2),xlab="p18_25",ylab="Residuals")
# check normality
qqplot(counties$p18_25,counties$crime)
mtext("Plot D: Diagnostics plots for p18_25", side = 1, line = -20, outer = TRUE)
# both plots look ok


# p65
par(mfrow=c(1,2))
# check residual consistency
plot(counties$p65,residuals(m2),xlab="p65",ylab="Residuals")
# check normality
qqplot(counties$p65,counties$crime)
mtext("Plot E: Diagnostics plots for p65", side = 1, line = -20, outer = TRUE)
# both plots look ok


# nhospbeds*
par(mfrow=c(2,2))
# check residual consistency
plot(counties$nhospbeds,residuals(m2),xlab="nhospbeds",ylab="Residuals")
# check normality
qqplot(counties$nhospbeds,counties$crime)
# do log transformation for nhospbeds
plot(log(counties$nhospbeds),residuals(m2),xlab="log-nhospbeds",ylab="Residuals")
qqplot(log(counties$nhospbeds),counties$crime)
mtext("Plot F: Diagnostics plots for nhospbeds before and after log-transformation", side = 1, line = -30, outer = TRUE)


# phs
par(mfrow=c(1,2))
# check residual consistency
plot(counties$phs,residuals(m2),xlab="phs",ylab="Residuals")
# check normality
qqplot(counties$phs,counties$crime)
mtext("Plot G: Diagnostics plots for phs", side = 1, line = -20, outer = TRUE)
# both plots look ok

# pcollege
par(mfrow=c(1,2))
# check residual consistency
plot(counties$pcollege,residuals(m2),xlab="pcollege",ylab="Residuals")
# check normality
qqplot(counties$pcollege,counties$crime)
mtext("Plot H: Diagnostics plots for pcollege", side = 1, line = -20, outer = TRUE)
# both plots look ok


# ppoverty
par(mfrow=c(1,2))
# check residual consistency
plot(log(counties$ppoverty),residuals(m2),xlab="ppoverty",ylab="Residuals")
# check normality
qqplot(counties$ppoverty,counties$crime)
mtext("Plot I: Diagnostics plots for ppoverty", side = 1, line = -20, outer = TRUE)
# both plots look ok


# punemployed
# ppoverty
par(mfrow=c(1,2))
# check residual consistency
plot(counties$punemployed,residuals(m2),xlab="punemployed",ylab="Residuals")
# check normality
qqplot(counties$punemployed,counties$crime)
mtext("Plot J: Diagnostics plots for punemployed", side = 1, line = -20, outer = TRUE)
# both plots look ok


# avg_income
par(mfrow=c(1,2))
# check residual consistency
plot(counties$avg_income,residuals(m2),xlab="avg_income",ylab="Residuals")
# check normality
qqplot(counties$avg_income,counties$crime)
mtext("Plot K: Diagnostics plots for avg_income", side = 1, line = -20, outer = TRUE)
# both plots look ok


# tot_income*
par(mfrow=c(2,2))
# check residual consistency
plot(counties$tot_income,residuals(m2),xlab="tot_income",ylab="Residuals")
# check normality
qqplot(counties$tot_income,counties$crime)
# both residual plot and qqplot suggest that tot_income need a transformation
plot(log(counties$tot_income),residuals(m2),xlab="log-tot_income",ylab="Residuals")
qqplot(log(counties$tot_income),counties$crime)
mtext("Plot L: Diagnostics plots for tot_income before and after log-transformation", side = 1, line = -30, outer = TRUE)


# New model with transformed variables
m3 = lm(crime~log(area)+p18_25+p65+log(pop)+log(nhospbeds)+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
summary(m3)

# Removing the outlier obtained from the above model diagnostic
counties = counties[-6,]
m4 = lm(crime~log(area)+p18_25+p65+log(pop)+log(nhospbeds)+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
summary(m4)

# Model Diagnostic to check outliers in the model 
par(mfrow=c(2,4))
plot(m3)
plot(m4)
mtext("Plot M: Diagnostics plots for the transformed model before and after removing the outlier", side = 1, line = -30, outer = TRUE)

######################################    Variable selection    ############################################

# Ridge Regression 
x= model.matrix(crime~log(area)+p18_25+p65+log(pop)+log(nhospbeds)+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income)-1,data =counties)
y = counties$crime
fit_ridge= glmnet(x,y,alpha=0)
plot(fit_ridge,xvar = "lambda", label = TRUE)
cv_ridge= cv.glmnet(x,y,alpha=0)
plot(cv_ridge)

# Coefficients obtained from the ridge regression
coef(cv_ridge)


# Lasso Regression
fit_lasso = glmnet(x,y)
plot(fit_lasso,xvar = "lambda",label = TRUE)
plot(fit_lasso,xvar = "dev",label = TRUE)

cv_lasso = cv.glmnet(x,y)
plot(cv_lasso)

# Coefficients obtained from the lasso regression
coef(cv_lasso)

#Lasso result: crime~log(area)+p18_25+log(nhospbeds)+pcollege+ppoverty+punemployed+log(tot_income)

# Minimum CV error obtained from Lasso Regression
mse_min_l = cv_lasso$cvm[cv_lasso$lambda == cv_lasso$lambda.min]
mse_min_l


# Minimum CV error obtained from Rigde Regression
mse_min_R = cv_ridge$cvm[cv_ridge$lambda == cv_ridge$lambda.min]
mse_min_R


## Comments : Lasso gives the best result out of these models

# Variable selection through Best subset method
regfit_bs <- regsubsets(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+p65+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income), nvmax=11, data=counties)
bs.summary <- summary(regfit_bs)


bs.summary$adjr2
which.max(bs.summary$adjr2)
coef(regfit_bs,8)
# Best subset r2: crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income)

bs.summary$cp
which.min(bs.summary$cp)
coef(regfit_bs,7)
# Best subset Cp:crime~log(area)+log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income)

bs.summary$bic
which.min(bs.summary$bic)
coef(regfit_bs,6)
# Best subset BIC:crime~log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income)

##Plot for Best Subset
old.par <- par(mfrow=c(1,3))
plot(bs.summary$adjr2,type="l",xlab="Number of variables", ylab="Best Subset_Adj Rsq")
points(8,bs.summary$adjr2[8],col="red",cex=2,pch=20)
plot(bs.summary$cp,type="l",xlab="Number of variables", ylab="Best Subset_Cp")
points(7,bs.summary$cp[7],col="red",cex=2,pch=20)
plot(bs.summary$bic,type="l",xlab="Number of variables", ylab="Best Subset_BIC")
points(6,bs.summary$bic[6],col="red",cex=2,pch=20)
par(old.par)


##Variable Selection using forward Selection 
regfit_forward <- regsubsets(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+p65+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),method = "forward", nvmax=11, data=counties)
forward.summary <- summary(regfit_forward)

forward.summary$adjr2
which.max(forward.summary$adjr2)
coef(regfit_forward,8)
# Forward r2:crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income) 

forward.summary$cp
which.min(forward.summary$cp)
coef(regfit_forward,7)

forward.summary$bic
which.min(forward.summary$bic)
coef(regfit_forward,7)
# Forward Cp/BIC:crime~log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income)


##Plot for Forward Selection 
old.par <- par(mfrow=c(1,3))
plot(forward.summary$adjr2,type="l",xlab="Number of variables", ylab="Forward_Adj Rsq")
points(8,forward.summary$adjr2[[8]],col="red",cex=2,pch=20)
plot(forward.summary$cp,type="l", pch=20, xlab="Number of variables", ylab="Forward_Cp")
points(7,forward.summary$cp[[7]],col="red",cex=2,pch=20)
plot(forward.summary$bic,type="l",xlab="Number of variables", ylab="Forward_BIC")
points(7,forward.summary$bic[[7]],col="red",cex=2,pch=20)
par(old.par)


# Variable selection using backward selection
regfit_backward = regsubsets(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+p65+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),method = "backward",nvmax=11,data=counties)
backward.summary <- summary(regfit_backward)


backward.summary$adjr2
which.max(backward.summary$adjr2)
coef(regfit_backward,8)
# Backward r2: crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income)

backward.summary$cp
which.min(backward.summary$cp)
coef(regfit_backward,7)
# Backward Cp: crime~log(area)+log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income)

backward.summary$bic
which.min(backward.summary$bic)
coef(regfit_backward,6)
# Backward BIC:crime~log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income) 


##Plot Backward Selection
old.par <- par(mfrow=c(1,3))
plot(backward.summary$adjr2,type="l",xlab="Number of variables", ylab="Backward_Adj Rsq")
points(8,backward.summary$adjr2[8],col="red",cex=2,pch=20)
plot(backward.summary$cp,type="l",xlab="Number of variables", ylab="Backward_Cp")
points(7,backward.summary$cp[7],col="red",cex=2,pch=20)
plot(backward.summary$bic,type="l",xlab="Number of variables", ylab="Backward_BIC")
points(6,backward.summary$bic[6],col="red",cex=2,pch=20)
par(old.par)


######################################### Cross Valdation- LOOCV    #####################################################
# LASSO
glm.fit1=glm(crime~log(area)+p18_25+log(nhospbeds)+pcollege+ppoverty+punemployed+log(tot_income)
,data=counties)
cv.err=cv.glm(counties,glm.fit1)
err1=cv.err$delta[1]
err1
# Error=355.9835

# Ridge/Full Linear Model
glm.fit2=glm(crime~log(area)+p18_25+p65+log(pop)+log(nhospbeds)+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),data =counties)
cv.err=cv.glm(counties,glm.fit2)
err2=cv.err$delta[1]
err2
# Error=338.4752

# Best subset/Backward r2: 
glm.fit3=glm(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit3)
err3=cv.err$delta[1]
err3
# Error=333.843


# Best subset/Backward Cp:
glm.fit4=glm(crime~log(area)+log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit4)
err4=cv.err$delta[1]
err4
# Error=333.9345


# Best subset/Backward BIC:
glm.fit5=glm(crime~log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit5)
err5=cv.err$delta[1]
err5
# Error=334.105


# Forward r2:
glm.fit6=glm(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit6)
err6=cv.err$delta[1]
err6
# Error=333.843


# Forward Cp/BIC:
glm.fit7=glm(crime~log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit7)
err7=cv.err$delta[1]
err7
# Error=333.5206

# MSE plot using LOOCV
par(mfrow=c(1,1))
final_comparison<-data.frame(Method=c('LASSO','Ridge','Best/Backward R2','Best/Backward Cp','Best/Backward BIC','Forward R2','Forward Cp/BIC'),CV_Error=
                               c(err1,err2,err3,err4,err5,err6,err7))
final_comparison
plot(final_comparison[,2],xlab="Models",ylab="Mean Squared Errors",type = "b",lwd=3, main= "MSE for Different Models Using LOOCV")

ggplot(data=final_comparison,aes(x=Method,y=CV_Error))+geom_point(aes(x=Method,y=CV_Error),shape=16, size=3, type ="l")


############################################ 10-fold CV   ##################################################
# LASSO
set.seed(1)
glm.fit1=glm(crime~log(area)+p18_25+log(nhospbeds)+pcollege+ppoverty+punemployed+log(tot_income)
,data=counties)
cv.err=cv.glm(counties,glm.fit1,K=10)
err1=cv.err$delta[1]
err1
# Error=356.3485

# Ridge/Full Linear Model
set.seed(1)
glm.fit2=glm(crime~log(area)+p18_25+p65+log(pop)+log(nhospbeds)+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),data =counties)
cv.err=cv.glm(counties,glm.fit2,K=10)
err2=cv.err$delta[1]
err2
# Error=334.2299

# Best subset/Backward r2: 
set.seed(1)
glm.fit3=glm(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit3,K=10)
err3=cv.err$delta[1]
err3
# Error=331.9782


# Best subset/Backward Cp:
set.seed(1)
glm.fit4=glm(crime~log(area)+log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit4,K=10)
err4=cv.err$delta[1]
err4
# Error=332.5523


# Best subset/Backward BIC:
set.seed(1)
glm.fit5=glm(crime~log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit5,K=10)
err5=cv.err$delta[1]
err5
# Error=333.6209


# Forward r2:
set.seed(1)
glm.fit6=glm(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit6,K=10)
err6=cv.err$delta[1]
err6
# Error=331.9782


# Forward Cp/BIC:
set.seed(1)
glm.fit7=glm(crime~log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
cv.err=cv.glm(counties,glm.fit7,K=10)
err7=cv.err$delta[1]
err7
# Error=332.3918

# MSE plot using 10-fold CV
par(mfrow=c(1,1))
final_comparison<-data.frame(Method=c('LASSO','Ridge','Best/Backward R2','Best/Backward Cp','Best/Backward BIC','Forward R2','Forward Cp/BIC'),CV_Error=
                               c(err1,err2,err3,err4,err5,err6,err7))
final_comparison
plot(final_comparison[,2],xlab="Models",ylab="Mean Squared Errors",type = "b",lwd=3, main= "MSE for Different Models Using 10-Fold CV")


# Fit linear models
#Lasso 
lm.fit1=lm(crime~log(area)+p18_25+log(pop)+log(nhospbeds)+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),data=counties)

#Ridge
lm.fit2=lm(crime~log(area)+p18_25+p65+log(pop)+log(nhospbeds)+phs+pcollege+ppoverty+punemployed+avg_income+log(tot_income),data=counties)

#Best subset/Backward r2: 
lm.fit3=lm(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)

#Best subset/Backward Cp:
lm.fit4=lm(crime~log(area)+log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)

#Best subset/Backward BIC:
lm.fit5=lm(crime~log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)

#Forward r2:
lm.fit6=lm(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)

#Forward Cp/BIC:
lm.fit7=lm(crime~log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)

# AIC
AIC=AIC(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5,lm.fit6,lm.fit7)
AIC

# BIC
BIC = BIC(lm.fit1,lm.fit2,lm.fit3,lm.fit4,lm.fit5,lm.fit6,lm.fit7)
BIC


############################################ Two Best Models    #############################################
# Forward Cp/BIC:
#lm.fit7
lm.fit.final = lm(crime~log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
summary(lm.fit.final)
# Best subset/Backward/Forward r2: 
#lm.fit3
lm.fit.final =lm(crime~log(area)+log(pop)+log(nhospbeds)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data=counties)
summary(lm.fit.final)

############################################     Final Model    #############################################
final=lm(crime~log(pop)+p18_25+ppoverty+punemployed+avg_income+log(tot_income),data = counties)
summary(final)

############################################     Forecasting ``##############################################
# Predicting the data from the final model for some other county not present in the current dataset used 
counties_new <- counties
counties_new$pop <- log(counties$pop)
counties_new$tot_income <- log(counties$tot_income)
counties_new = counties[-6,]
counties_new = counties[,3:16]
counties_new=counties_new[,-13]
# Final model
final_v=lm(crime~pop+p18_25+ppoverty+punemployed+avg_income+tot_income,data = counties_new)

predict(final_v,new=data.frame(pop =217280, p18_25=28.1 , ppoverty= 7.9,punemployed=6.2, avg_income=16458, tot_income=3857))
# Forecasting Interval
predict(final_v,new=data.frame(pop =217280, p18_25=28.1 , ppoverty= 7.9,punemployed=6.2, avg_income=16458, tot_income=3857), interval="prediction")
# Confidence Interval
predict(final_v,new=data.frame(pop =217280, p18_25=28.1 , ppoverty= 7.9,punemployed=6.2, avg_income=16458, tot_income=3857), interval="confidence")