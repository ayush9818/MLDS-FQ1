setwd("/Users/ecm/teach/data")
newfood = read.csv("newfood.csv")
names(newfood)
head(newfood)
# run plots and crosstab
plot(newfood)
ans = with(newfood, table(city, ad, loc, price))
ftable(ans, row.vars=1:2, cols.vars = 3:4)

round(cor(newfood), 3)

summary(lm(sales ~ price, newfood))
summary(lm(sales ~ price+ad, newfood))
summary(lm(sales ~ price+ad+loc, newfood))
summary(lm(sales ~ price+ad+loc+volume, newfood))
summary(lm(sales ~ price+loc+volume, newfood))


fit = lm(sales ~ price+ad+loc+volume, newfood)
vif(fit)



