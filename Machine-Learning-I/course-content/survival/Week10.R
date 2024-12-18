setwd("/Users/ecm/teach/lectures/mlogit")
library(dplyr)
library(nnet)

# 3-class Gaussian mixture
mu = matrix(2, nrow=3, ncol=2)
mu[2,] = sqrt(5)*c(1,1)+c(3,1)
mu[3,] = sqrt(5)*c(1,1)+c(1,3)
makedat = function(mu, n=50, sd=1.5,
                   seed=12345){
  dat=data.frame(y=c(rep(1,n),
                     rep(2,n),rep(3,n)))
  set.seed(seed)
  dat$x1 = c(
    rnorm(n, mu[1,1], sd),
    rnorm(n, mu[2,1], sd),
    rnorm(n, mu[3,1], sd)
  )
  dat$x2 = c(
    rnorm(n, mu[1,2], sd),
    rnorm(n, mu[2,2], sd),
    rnorm(n, mu[3,2], sd)
  )
  dat
}
test = makedat(mu, n=3000, seed=54321)
train = makedat(mu, n=20)
plot(x2~x1, train, col=train$y, pch=train$y+15)
fit2 = multinom(y~x1+x2, data=train)
summary(fit2)
coef(fit2)
apply(coef(fit2), 2, diff)
# apply to test set and compute classification rate
pred = predict(fit2, test, type="class") # apply to test set
sum(diag(table(test$y, pred))) / nrow(test) # classification rate

# let's try predicting x1=x2=4
predict(fit2, data.frame(x1=4, x2=4), type="prob")
predict(fit2, data.frame(x1=4, x2=4))
# now do it by hand
coef(fit2)
(eta = coef(fit2) %*% c(1,4,4))
(pi1 = 1/(1+sum(exp(eta))))
pi1*exp(eta)

# illustration of softmax
fit = nnet(factor(y) ~ x1+x2, data=train, skip=T, size=0)
fit
summary(fit)
(b=coef(fit))
b[9] - b[3]
coef(fit2)

# iris example
plot(Sepal.Width~Petal.Width, iris, col=Species, pch=15+as.numeric(iris$Species), cex=.7)
legend("topright", 1:3, levels(iris$Species), col=1:3, pch=16:18, cex=.7)
fit = multinom(Species~Sepal.Width+Petal.Width, iris)
summary(fit)

#Geriatric study
dat = read.table("/Users/ecm/teach/IEMS304/kutner/ch14/CH14PR39.txt", header=T)
# Q1
plot(dat, pch=16, cex=.5) # no obvious problems
summary(dat) # y is between 0 and 11
fit = glm(y~., poisson, dat) # Q2
plot(fit)  # no problems
summary(fit) # Q3
library(car)
vif(fit)
-1.069403/0.133154 # should give z=-8.031
(teststat=fit$null.deviance-fit$deviance)
1-pchisq(teststat,4)
drop1(fit, test="LRT") # Q4
summary(lm(log(y+1)~., dat)) #Q5
summary(fit) # Poisson tends to have more power

#fit2 = update(fit, .~.-male)
fit2 = glm(y~.-male, poisson, dat) #Q6
summary(fit2) #Q7
drop1(fit2, test="LRT")
plot(fit2, which=1)
# subject 100: intervention=0, balance=37, strength=56, y=2
dat[100,]
(eta=t(coef(fit2)) %*% c(1, 0, 37, 56)) # eta=1.297158
exp(eta)  # 3.658884
fit2$fitted.values[100] # muhat = 3.658884
predict(fit2, data.frame(dat[100,])) # gives eta
predict(fit2, data.frame(dat[100,]), type="resp") # gives muhat

summary(glm(y~intervention, poisson, dat))
summary(glm(y~balance, poisson, dat))
summary(glm(y~strength, poisson, dat))
# major problems: 
# (1) why are predictor uncorrelated? 
# (2) why does more strength go with more falls?
# (3) why does more balance go with more falls?


par(mfrow=c(2,2))
myt = seq(0,20, by=.1)
plot(myt, dexp(myt, rate=1), type="l", xlab="t", ylab="pdf f(t)")
lines(myt, dexp(myt, rate=.5), col=2)
lines(myt, dexp(myt, rate=.2), col=3)
legend("topright", c("1", "0.5", "0.2"), col=1:3, lty=1, cex=.6 )

plot(myt, pexp(myt, rate=1), type="l", xlab="t", ylab="cdf F(t)")
lines(myt, pexp(myt, rate=.5), col=2)
lines(myt, pexp(myt, rate=.2), col=3)
#legend("bottomright", c("1", "0.5", "0.2"), col=1:3, lty=1)

plot(myt, 1-pexp(myt, rate=1), type="l", xlab="t", ylab="survival S(t)")
lines(myt, 1-pexp(myt, rate=.5), col=2)
lines(myt, 1-pexp(myt, rate=.2), col=3)
#legend("topright", c("1", "0.5", "0.2"), col=1:3, lty=1)

plot(myt, rep(1, length(myt)), type="l", xlab="t", ylab="hazard h(t)", ylim=c(0,1))
lines(myt, rep(.5, length(myt)), col=2)
lines(myt, rep(.2, length(myt)), col=3)
#legend("bottomright", c("1", "0.5", "0.2"), col=1:3, lty=1)
par(mfrow=c(1,1))


library(survival)
setwd("/Users/ecm/teach/lectures/clv")
#dat= read.table("service1yr.txt", header=T) 
dat = data.frame(
  bigT = c(2:12, 1, 3:12),
  cancel = c(rep(1,11), rep(0,11)),
  count = c(4,16,20,37,28,61,24,19,13,10,13,3,2,1,7,33,49,63,30,16,34,188)
)
dat
fit = survfit(Surv(bigT, cancel) ~ 1, data=dat, weight=count)
summary(fit)

plot(fit)
library(ggsurvfit)
#postscript("Rserv1yr.ps", horizontal=F, height=5, width=5)
#pdf("Rserv1yr.pdf", height=5, width=5)
fit %>%
  ggsurvfit(size = 1) +
  add_confidence_interval()
#dev.off()

service5yr = read.table("/Users/ecm/prose/sasltv/tiger/service5yr.txt", header=T) 
fit2 = survfit(Surv(bigT, cancel) ~ 1, data=service5yr, weight=count)
plot(fit2)
fit2 %>%
  ggsurvfit(size = 1) +
  add_confidence_interval()

fit3 = survfit(Surv(bigT, cancel) ~ startlen, data=service5yr, weight=count)
plot(fit3)
#postscript("ex3.1gg.ps", horizontal=F, height=5, width=5)
#pdf("ex3.1gg.pdf", height=5, width=5)
fit3 %>%
  ggsurvfit(size = 1) +
  add_confidence_interval()
#dev.off()

library(MASS)
head(gehan) # cens poorly labeled, 1 if event happened and 0 for censored
fit = survfit(formula = Surv(time, cens) ~ treat, data = gehan)
with(gehan, Surv(time, cens))
summary(fit)
#postscript("gehangg.ps", horizontal=F, height=5, width=5)
#pdf("gehangg.pdf", height=5, width=5)
fit %>%
  ggsurvfit(size = 1) +
  add_confidence_interval()
#dev.off()
survdiff(Surv(time, cens)~treat, gehan)  # log-rank test of differences


