# ACT example
course = data.frame(
  type=factor(c(rep(1,20), rep(2,20)), 1:2, c("Trad","Online")),
  length=factor(c(rep(1,10), rep(2,10), rep(1,10), rep(2,10)),
    1:2, c("Condensed","Regular")),
  act=c(26,27,25,21,21,18,24,19,20,18,  34,24,35,31,28,28,21,23,29,26,
        27,29,30,24,30,21,32,20,28,29,  24,16,22,20,23,21,19,19,24,25))

fit2 = lm(act ~ type*length, course)
summary(fit2)
interaction.plot(course$length, course$type, course$act, col=1:2)
tapply(course$act, course[,1:2], mean)
library(sqldf)
sqldf("select length, type, avg(act) from course group by length, type")
fit2$coef

# Orange Juice Example
oj = data.frame(brand=c(rep("A",12), rep("B",12), rep("C",12)), 
        time = factor(rep(c(0,0,0,0,3,3,3,3,7,7,7,7), 3)), 
        acid = c(52.6,54.2,49.8,46.5,49.4,49.2,42.8,
                 53.2,42.7,48.8,40.4,47.6,56,48,  
                 49.6,48.4,48.8,44,44,42.4,49.2,44,42, 
                 43.2,52.5,52,51.8,53.6,48,47,48.2, 
                 49.6,48.5,43.4,45.2,47.6))
#pdf("/Users/ecm/teach/lectures/plots451/Roj.pdf", height=5, width=5)
with(oj, interaction.plot(time, brand, acid, col=1:3, lwd=2))
#dev.off()
fit = lm(acid ~ time*brand, oj)
summary(fit)
drop1(fit, test="F")
fit2 = lm(acid ~ time+brand, oj)
summary(fit2)
drop1(fit2, test="F")
fit3 = lm(acid ~ time, oj)
drop1(fit3, test="F")
summary(fit3)

# Capacitor Example in R
capacitor = data.frame(
  bondmat=factor(c(rep(1,12),rep(2,12),rep(3,12),rep(4,12))),
  substrate=rep(c(rep("A",4),rep("B",4),rep("C",4)), 4), y=c(1.51,1.96,1.83,1.98,1.63,1.92,1.8,1.71,3.04,3.16,3.09, 3.5,2.62,2.82,2.69,2.93,3.12,2.94,3.23,2.99,1.91,2.11,1.78,2.25,2.96,2.82,3.11,3.11,  2.91,2.93,3.01,2.93,3.04,2.91,2.48,2.83,3.67,3.4,3.25,2.9,3.48,3.51,
    3.24,3.45,3.47,3.42,3.31,3.76)
)

with(capacitor, table(substrate, bondmat))   # note orthogonal design
with(capacitor, tapply(y, data.frame(substrate, bondmat), mean))
capacitor %>%
  group_by(substrate, bondmat) %>%
  summarize(meany = mean(y)) %>%
  spread(bondmat, meany)
with(capacitor, interaction.plot(substrate, bondmat, y, col=1:4))
with(capacitor, interaction.plot(bondmat, substrate, y, col=1:4))
fit = lm(y~bondmat*substrate, capacitor)
summary(fit)
drop1(fit, test="F")
anova(fit)
plot(fit)
fit2 = lm(y ~ bondmat:substrate, capacitor)
summary(fit2)
# Montgomery 14.2
paint = data.frame(
type=factor(c(rep(1,9), rep(2,9))),
  time=factor(rep(c(rep(20,3), rep(25,3), rep(30,3)), 2)), 
  y=c(74,64,50, 73,61,44, 78,85,92, 92,86,68, 98,73,88, 66,45,85))
fit = lm(y~type*time, paint)
summary(fit)
drop1(fit, test="F")
summary(fit)
with(paint, interaction.plot(type, time, y, col=1:3))
with(paint, interaction.plot(time, type, y, col=1:2))

# Montgomery 14.4
anode = data.frame(
  pos = factor(c(rep(1,9), rep(2,9))),
  temp = factor(rep(c(rep(800,3), rep(825,3), rep(850,3)), 2)), 
  density = c( 570,565,583, 1063,1080,1043, 565,510,590, 528,547,521, 988,1026,1004, 526,538,532))
fit = lm(density ~ pos*temp, anode)
drop1(fit, test="F")
summary(fit)
with(anode, interaction.plot(pos, temp, density, col=1:3))
with(anode, interaction.plot(temp, pos, density, col=1:2))
fit = lm(density ~ pos+temp, anode)
drop1(fit, test="F")
summary(fit)

# Newfood with a Numerical*Categorial Interaction
fit = lm(sales ~ price*ad + volume, newfood)
drop1(fit, test="F")
summary(fit)


	⁃	# Chemical Engineering Example
cheme = read.table("teach/304/data/cheme.dat", header=T)
fit = lm(yield ~ time*temp+I(time^2)+I(temp^2), cheme)
summary(fit)
b = coef(fit)[2:3]
B = matrix(c(coef(fit)[c(4,6,6,5)])*c(1,.5,.5,1), nrow=2)
xs = -solve(B) %*% b/2
xs
eigen(B)$values
size = 30
x1 = seq(75, 95, length=size)
x2 = seq(165, 185, length=size)
y = outer(x1, x2, function(x1 ,x2)
    -0.001431 + 7.809*x1 + 13.27*x2 -0.05506*x1^2
    -0.04005*x2^2 + 0.01*x1*x2
  )
persp(x1, x2, y, xlab="Time", ylab="Temperature")

image(x1, x2, y, xlab="Time", ylab="Temperature")
contour(x1, x2, y, add=T)
points(jitter(cheme$time), jitter(cheme$temp))
points(xs[1], xs[2], pch="X", cex=4)
rm(cheme, x1, x2, y, b, B, xs)