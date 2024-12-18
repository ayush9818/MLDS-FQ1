height = read.table("height.txt", header=T)
head(height)

m = height$Male # or height[,1] or height[, "Male"] 
f = height$Female
mean(m) # mean
mean(f)
var(m) # sample variance
var(f)
sd(f) # standard deviation
cov(m,f) # covariance
cor(m,f) # correlation
par(cex=0.7) # resize text 
hist(f)


par(mfrow=c(1,2), cex=0.7) # 2 plots side-by-side 
hist(f, breaks=4) # set number of bins
hist(f, breaks=seq(1.40,1.75,by=0.03)) # set bins



library(ggplot2) # load package 
ggplot(height,aes(Male))+geom_histogram(binwidth=0.02)


sigma = 0.0036; Mbar = mean(m); n = length(m)
E = qnorm(1-0.05/2)*sigma/sqrt(n) # margin of error 
CI_m = Mbar + c(-E,E)
CI_m


S = sd(m)
E = qt(1-0.05/2, df=n-1)*S/sqrt(n)
CI_M = Mbar + c(-E,E)
CI_M


z = sqrt(n)*(Mbar-1.728)/sigma
z

z.half.alpha = qnorm(1-0.05/2) # check if z is in the interval 
c(-z.half.alpha, z.half.alpha)

z = sqrt(n)*(Mbar-1.728)/sigma
z

z.alpha = -qnorm(1-0.05)
z.alpha

t = sqrt(n)*(Mbar-1.728)/sd(m)
t

t.half.alpha = qt(1-0.05/2, df=n-1)
c(-t.half.alpha, t.half.alpha)

t.test(m,mu=1.728,alternative="less")

t.test(m,f,paired=T)

t.test(m,f,var.equal=T)


# Generate 200 random variates from N(10,2ˆ2)
x.norm = rnorm(n=200,m=10,sd=2)
par(mfrow=c(1,2), cex=0.7)
hist(x.norm,breaks=10,main="Histogram of observed data"）
plot(density(x.norm),main="Density estimate of data")

par(mfrow=c(1,2), cex=0.5)
h = hist(x.norm, breaks=10, plot=F)
x.hist = c(min(h$breaks),h$breaks)
y.hist = c(0,h$density,0)
x.fit = seq(min(x.norm),max(x.norm),length=40)
y.fit = dnorm(x.fit, mean=mean(x.norm), sd=sd(x.norm))
plot(x.hist, y.hist, type="s", ylim=c(0,max(y.hist,y.fit)),
     main="Normal pdf and histogram")
lines(x.fit, y.fit, col="red")
plot(density(x.norm), main="Density estimate of data")
lines(x.fit,y.fit, col="red")


library("fitdistrplus")
f_weibull = fitdist(x.norm, "weibull")
summary(f_weibull)

f_normal = fitdist(x.norm, "norm")
summary(f_normal)

z.norm = (x.norm-mean(x.norm))/sd(x.norm)
qqnorm(x.norm)
#Use qqplot(data1, data2) to compare distributions of 2 datasets 
abline(mean(x.norm),b=sd(x.norm),col="red") #slope & intercept



