library(glmnet); library(car)
# section 6.8, page 263 JWHT
college = read.csv("/Users/ecm/teach/IEMS304/data/College.csv")
head(college)
row.names(college) = college$X
college$X=NULL
set.seed(12345)
train = runif(nrow(college))<.5    # pick train/test split
dim(college)
table(train)  # Question 2
hist(college$logApps)  # Question 3

# Question 4
fit = lm(Apps ~ ., college, subset=train)
plot(fit, pch=16, cex=.5)

# Question 5
# I am overwriting a variable, which is not good practice, but it simplifies the code below substantially
#college$Apps = sqrt(college$Apps)  
college$logApps = log(college$Apps)
hist(college$logApps)
college$Apps=NULL # drop variable so that I can use ~.

# Question 6: full model
fit = lm(logApps ~ ., college, subset=train)
plot(fit, which=1, pch=16, cex=.8)
yhat = predict(fit, college[!train,])
mean((college$logApps[!train] - yhat)^2)       # compute test set MSE
summary(fit)
vif(fit) # don't care about multicollinearity, but it's bad

# with square root of apps:
# Full 92.75
# Step 91.15
# ridge 84.23
# lasso 89.47


# Question 7: stepwise model
fit2 = step(fit)
yhat = predict(fit2, college[!train,])
mean((college$logApps[!train] - yhat)^2)       # compute test set MSE
summary(fit2)

# Question 8: ridge model
x = model.matrix(logApps ~ ., college)
fit.ridge = glmnet(x[train,], college$logApps[train], alpha=0) # alpha=0 means ridge
plot(fit.ridge, xvar="lambda")
fit.cv = cv.glmnet(x[train,], college$logApps[train], alpha=0) # find optimal lambda
fit.cv$lambda.min        # optimal value of lambda
abline(v=log(fit.cv$lambda.min), col=2)
plot(fit.cv)          # plot MSE vs. log(lambda)
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[!train,])  # find yhat for best model
mean((college$logApps[!train] - yhat)^2)      # compute test set MSE

# Question 9: lasso model
fit.lasso = glmnet(x[train,], college$logApps[train], alpha=1) # lambda=1 means lasso
plot(fit.lasso, xvar="lambda")
fit.cv = cv.glmnet(x[train,], college$logApps[train], alpha=1)
plot(fit.cv)
abline(v=log(fit.cv$lambda.min), col=2)
abline(v=log(fit.cv$lambda.1se), col=3)
yhat = predict(fit.lasso, s=fit.cv$lambda.min, newx=x[!train,])
mean((college$logApps[!train] - yhat)^2)       # compute test set MSE

# Question 10: improved model with transformations
plot(college[train,-1], pch=16, cex=.5)
par(mfrow=c(2,4))
for(i in 3:10){
  plot(college[,i], college$logApps, pch=16, cex=.5, main=names(college)[i])
  lines(smooth.spline(college[,i], college$logApps, df=5), col=2, lwd=2)
} 
for(i in 11:18){
  plot(college[,i], college$logApps, pch=16, cex=.5, main=names(college)[i])
  lines(smooth.spline(college[,i], college$logApps, df=5), col=2, lwd=2)
}
par(mfrow=c(1,1))
plot(logApps ~ log(Accept), college, pch=16)
plot(logApps ~ log(Enroll), college, pch=16)
plot(logApps ~ log(F.Undergrad), college, pch=16)

# forward stepwise model
fit= lm(logApps ~ 1, college, subset=train)
fit2 = step(fit, scope=~Private
        +Accept+sqrt(Accept)+log(Accept)
        +Enroll+sqrt(Enroll)+log(Enroll)
        +Top10perc+I(Top10perc^2)
        +Top25perc+I(Top25perc^2)
        +F.Undergrad+sqrt(F.Undergrad)+log(F.Undergrad)
        +P.Undergrad+sqrt(P.Undergrad)+log(P.Undergrad)
        +Outstate+Room.Board
        +Books+sqrt(Books)+log(Books)+Personal
        +PhD+I(PhD^2)+Terminal+S.F.Ratio
        +perc.alumni+Expend+Grad.Rate)
plot(fit2, pch=16, cex=.5)
yhat = predict(fit2, college[!train,])
mean((college$logApps[!train] - yhat)^2)

# ridge model
x = model.matrix(logApps ~ Private+Accept+sqrt(Accept)+log(Accept)+
        Enroll+sqrt(Enroll)+log(Enroll)
        +Top10perc+I(Top10perc^2)+Top25perc+I(Top25perc^2)+F.Undergrad+sqrt(F.Undergrad)+log(F.Undergrad)+P.Undergrad+sqrt(P.Undergrad)+log(P.Undergrad)+Outstate+Room.Board+Books+sqrt(Books)+log(Books)+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate, college)
fit.ridge = glmnet(x[train,], college$logApps[train], alpha=0)
round(fit.ridge$beta[,100], 6)
plot(fit.ridge, xvar="lambda")
fit.cv = cv.glmnet(x[train,], college$logApps[train], alpha=0)
abline(v=log(fit.cv$lambda.min))
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[!train,])
mean((college$logApps[!train] - yhat)^2)

# lasso model
fit.lasso = glmnet(x[train,], college$logApps[train], alpha=1)
fit.cv = cv.glmnet(x[train,], college$logApps[train], alpha=1)
plot(fit.lasso, xvar="lambda"); abline(v=log(fit.cv$lambda.min))
round(predict(fit.lasso, s=fit.cv$lambda.min, type="coefficients")[1:29,], 4)
yhat = predict(fit.lasso, s=fit.cv$lambda.min, newx=x[!train,])
mean((college$logApps[!train] - yhat)^2)
#Lasso MSE test set=.03646 with log trans on DV

# forward stepwise GAM model
library(gam)
fit= gam(logApps ~ 1, data=college, subset=train)
fit2 = step.Gam(fit, scope=list(
  "Private"=~1+Private,
    "Accept"=~1+Accept+s(Accept),
    "Enroll"=~1+Enroll+s(Enroll),
    "Top10perc"=~1+Top10perc+s(Top10perc),
    "Top25perc"=~1+Top25perc+s(Top25perc),
    "F.Undergrad"=~1+F.Undergrad+s(F.Undergrad),
    "P.Undergrad"=~1+P.Undergrad+s(P.Undergrad),
    "Outstate"=~1+Outstate+s(Outstate),
    "Room.Board"=~1+Room.Board+s(Room.Board),
    "Books" = ~1+Books+s(Books),
    "Personal"=~1+Personal+s(Personal),
    "PhD"=~1+PhD+s(PhD),
    "Terminal"=~1+Terminal+s(Terminal),
    "S.F.Ratio"=~1+S.F.Ratio+s(S.F.Ratio),
    "perc.alumni"=~1+perc.alumni+s(perc.alumni),
    "Expend"=~1+Expend+s(Expend),
    "Grad.Rate"=~1+Grad.Rate+s(Grad.Rate)
    ))
summary(fit2)
plot(fit2, ask=T, se=T, scale=10)
yhat = predict(fit2, college[!train,])
mean((college$logApps[!train] - yhat)^2)
plot(fit2$fitted.values, fit2$residuals, pch=16, cex=.7)
lines(smooth.spline(fit2$fitted.values, fit2$residuals, df=5), col=2)

# add log(Apps) to improve model
college$logaccept = log(college$Accept)
hist(college$logaccept)
qqnorm(college$logaccept)
college$logenroll = log(college$Enroll)
college$logexpend = log(college$Expend)
college$logpunder = log(college$P.Undergrad)
fit= gam(logApps ~ 1, data=college, subset=train)
fit2 = step.Gam(fit, scope=list(
  "Private"=~1+Private,
  "logaccept" = ~1 + logaccept + s(logaccept),
  "Enroll"=~1+Enroll+s(Enroll)+logenroll + s(logenroll),
  "Top10perc"=~1 + Top10perc+s(Top10perc),
  "Top25perc"=~1+Top25perc+s(Top25perc),
  "F.Undergrad"=~1+F.Undergrad+s(F.Undergrad),
  "P.Undergrad"=~1+P.Undergrad+s(P.Undergrad)+logpunder +s(logpunder),
  "Outstate"=~1+Outstate+s(Outstate),
  "Room.Board"=~1+Room.Board+s(Room.Board),
  "Books" = ~1+Books+s(Books),
  "Personal"=~1+Personal+s(Personal),
  "PhD"=~1+PhD+s(PhD),
  "Terminal"=~1+Terminal+s(Terminal),
  "S.F.Ratio"=~1+S.F.Ratio+s(S.F.Ratio),
  "perc.alumni"=~1+perc.alumni+s(perc.alumni),
  "Expend"=~1+Expend+s(Expend)+logexpend+s(logexpend),
  "Grad.Rate"=~1+Grad.Rate+s(Grad.Rate)
))
summary(fit2)
plot(fit2, ask=T, se=T)
yhat = predict(fit2, college[!train,])
mean((college$logApps[!train] - yhat)^2)


# try tree
library(tree)
fit = tree(logApps ~ ., college[train,])
fit
plot(fit, type="uniform")
text(fit, cex=.7)
yhat = predict(fit, newdata=college[!train,])
mean((college$logApps[!train] - yhat)^2) # 109.7879

# overgrow the tree
fit = tree(logApps ~ ., college[train,], mindev= .0001)
fit
plot(fit, type="uniform")
text(fit, cex=.3)
plot(cv.tree(fit))
yhat = predict(prune.tree(fit, best=10), newdata=college[!train,])
mean((college$logApps[!train] - yhat)^2) # 95.93813
yhat = predict(prune.tree(fit, best=20), newdata=college[!train,])
mean((college$logApps[!train] - yhat)^2) # 89.3257
yhat = predict(prune.tree(fit, best=30), newdata=college[!train,])
mean((college$logApps[!train] - yhat)^2) # 92.32201

# Random Forest
library(randomForest)
set.seed(12345)
fit  = randomForest(x=college[train, -2], y=college$logApps[train], xtest=college[!train,-2], ntree=100)
plot(fit)
varImpPlot(fit)
mean((college$logApps[!train] - fit$test$predicted)^2) # 66.99894 need more trees???

# Random forest with more trees
set.seed(12345)
fit  = randomForest(x=college[train, -2], y=college$logApps[train], xtest=college[!train,-2], ntree=1000)
plot(fit)
mean((college$logApps[!train] - fit$test$predicted)^2) # 65.02892

# now try bagging
set.seed(12345)
fit  = randomForest(x=college[train, -2], y=college$logApps[train], xtest=college[!train,-2], ntree=100, mtry=17)
mean((college$logApps[!train] - fit$test$predicted)^2) #  53.60469 need more trees???

# bagging with more trees
set.seed(12345)
fit  = randomForest(x=college[train, -2], y=college$logApps[train], xtest=college[!train,-2], ntree=500, mtry=17)
plot(fit)
mean((college$logApps[!train] - fit$test$predicted)^2) #  53.10197

# bagging with even more trees
set.seed(12345)
fit  = randomForest(x=college[train, -2], y=college$logApps[train], xtest=college[!train,-2], ntree=1000, mtry=17)
mean((college$logApps[!train] - fit$test$predicted)^2) #  53.32566

# boosted tree
library(gbm)
set.seed(12345)
fit = gbm(logApps ~ ., data=college[train,], interaction.depth=2, cv.folds=10, n.trees=5000)
gbm.perf(fit)
summary(fit)
yhat = predict(fit, newdata=college[!train,], n.trees=500)
mean((college$logApps[!train] - yhat)^2) # 58.5233 Note: try more trees

# boosted tree with more trees
yhat = predict(fit, newdata=college[!train,], n.trees=500)
mean((college$logApps[!train] - yhat)^2) # 70.51755 Note: try more trees

# boosted tree with even more trees
yhat = predict(fit, newdata=college[!train,], n.trees=10000)
mean((college$logApps[!train] - yhat)^2) # 58.41246 Note: try more trees

# boosted tree with learning rate .01
set.seed(12345)
fit = gbm(logApps ~ ., data=college[train,], interaction.depth=2, n.trees=10000, shrinkage=.01, cv.folds=10)
gbm.perf(fit)
yhat = predict(fit, newdata=college[!train,], n.trees=1000)
mean((college$logApps[!train] - yhat)^2) # 63.54462 
yhat = predict(fit, newdata=college[!train,], n.trees=2000)
mean((college$logApps[!train] - yhat)^2) # 58.93312 
yhat = predict(fit, newdata=college[!train,], n.trees=5000)
mean((college$logApps[!train] - yhat)^2) # 

# boosted tree with learning rate .005
set.seed(12345)
fit = gbm(logApps ~ ., data=college[train,], interaction.depth=2, n.trees=10000, shrinkage=.005, cv.folds=10)
gbm.perf(fit)
yhat = predict(fit, newdata=college[!train,], n.trees=3000)
mean((college$logApps[!train] - yhat)^2) # 60.53273 
yhat = predict(fit, newdata=college[!train,], n.trees=5000)
mean((college$logApps[!train] - yhat)^2) # 58.05411 

# boosted tree with increased learning rate and more trees
yhat = predict(fit, newdata=college[!train,], n.trees=10000)
mean((college$logApps[!train] - yhat)^2) # 56.09985 
yhat = predict(fit, newdata=college[!train,], n.trees=20000)
mean((college$logApps[!train] - yhat)^2) # 56.09985 

# boosted tree with increased learning rate .01, lower interaction depth
fit = gbm(logApps ~ ., data=college[train,], interaction.depth=1, n.trees=20000, shrinkage=.01)
yhat = predict(fit, newdata=college[!train,], n.trees=500)
mean((college$logApps[!train] - yhat)^2) # 56.76147 
yhat = predict(fit, newdata=college[!train,], n.trees=1000)
mean((college$logApps[!train] - yhat)^2) # 56.76147 
yhat = predict(fit, newdata=college[!train,], n.trees=5000)
mean((college$logApps[!train] - yhat)^2) # 56.76147 
yhat = predict(fit, newdata=college[!train,], n.trees=10000)
mean((college$logApps[!train] - yhat)^2) # 56.76147 
yhat = predict(fit, newdata=college[!train,], n.trees=20000)
mean((college$logApps[!train] - yhat)^2) # 56.76147 

# clean up
rm(college, train, fit, yhat, fit2, fit.lasso, fit.ridge, fit.cv)

