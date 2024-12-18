library(car)
setwd("/Users/ecm/teach/lectures/plots451/")
bodyfat = data.frame(
  x1=c(19.5,24.7,30.7,29.8,19.1,25.6,31.4,27.9,22.1,25.5,31.1,30.4,
    18.7,19.7,14.6,29.5,27.7,30.2,22.7,25.2),
  x2=c(43.1,49.8,51.9,54.3,42.2,53.9,58.5,52.1,49.9,53.5,56.6,56.7,
    46.5,44.2,42.7,54.4,55.3,58.6,48.2,51.0),
  x3=c(29.1,28.2,37.0,31.1,30.9,23.7,27.6,30.6,23.2,24.8,30.0,28.3,
    23.0,28.6,21.3,30.1,25.7,24.6,27.1,27.5),
  y=c(11.9,22.8,18.7,20.1,12.9,21.7,27.1,25.4,21.3,19.3,25.4,27.2,
    11.7,17.8,12.8,23.9,22.6,25.4,14.8,21.1)
)


#postscript("bodyfat.ps", horizontal=F, height=5, width=5)
#pdf("bodyfat.pdf", height=5, width=5)
plot(bodyfat)
#dev.off()
round(cor(bodyfat), 4)
fit.lm = lm(y~x1+x2+x3, bodyfat)
vif(fit.lm)
summary(fit.lm)

# old way using MASS
library(MASS)
Zbodyfat = data.frame(lapply(bodyfat, scale))
summary(lm(y~x1+x2+x3-1, Zbodyfat))
fit2 = lm.ridge(y~x1+x2+x3-1, Zbodyfat, lambda=seq(0,0.4,length=41))
#postscript("teach/lectures/plots451/bodyfat2.ps", horizontal=F, height=5, width=5)
#pdf("/Users/ecm/teach/lectures/plots451/bodyfat2.pdf", height=5, width=5)
plot(fit2, lwd=3); abline(h=0)
#dev.off()
summary(fit2)
round(coef(fit2), 4)

# using glmnet to match old way
library(glmnet)
lam = seq(0,.4,length=41)/nrow(Zbodyfat)
#fit = glmnet(as.matrix(bodyfat[,1:3]), bodyfat$y, alpha=0)
#matplot(fit$lambda*nrow(bodyfat), t(fit$beta), type="l"); abline(h=0)

#fit = glmnet(as.matrix(bodyfat[,1:3]), bodyfat$y, alpha=0, lambda=lam)
# This matches the plot from lm.ridge

# use glmnet, new lecture in ridge2
x = model.matrix(y ~ .-1, bodyfat)
head(x)
# we usually let glmnet pick lambda
# I pick them for class example
lam = exp(seq(-10, 7 ,length=100))
fit = glmnet(x, bodyfat$y, alpha=0, lambda=lam)
#pdf("bodyfat2a.pdf", height=5, width=5)
#postscript("bodyfat2a.ps", horizontal=F, height=5, width=5)
plot(fit, xvar="lambda", label=T, main="Ridge Trace"); abline(h=0)
round(cbind(lambda=fit$lambda, loglambda=log(fit$lambda), t(fit$beta)),2)

set.seed(12345)
fit2 = cv.glmnet(x, bodyfat$y, alpha=0, nfolds=5, lambda=lam)
names(fit2)
(l=fit2$lambda.min);log(l); abline(v=log(l),lty=2)
(l2=fit2$lambda.1se);log(l2); abline(v=log(l2), lty=3)
#dev.off()
#pdf("bodyfat2b.pdf", height=5, width=5)
#postscript("bodyfat2b.ps", horizontal=F, height=5, width=5)
plot(fit2)
#dev.off()

predict(fit2$glmnet.fit, s=l, type="coef")
predict(fit2$glmnet.fit, s=l2, type="coef")

# lasso
set.seed(12345)
fit.l1 = cv.glmnet(x, bodyfat$y, lambda=lam, nfolds=5)
#pdf("bodyfat2c.pdf", height=5, width=5)
#postscript("bodyfat2c.ps", horizontal=F, height=5, width=5)
plot(fit.l1$glmnet.fit, xvar="lambda", label=T, main="Lasso Trace"); abline(h=0)
(l=fit.l1$lambda.min); log(l); abline(v=log(l),lty=2)
(l=fit.l1$lambda.1se);log(l); abline(v=log(l), lty=3)
#dev.off()
round(cbind(
  lambda=fit.l1$glmnet.fit$lambda, 
  loglambda=log(fit.l1$glmnet.fit$lambda), 
    t(fit.l1$glmnet.fit$beta)
  ), 2)

#pdf("bodyfat2d.pdf", height=5, width=5)
#postscript("bodyfat2d.ps", horizontal=F, height=5, width=5)
plot(fit.l1)
#dev.off()


