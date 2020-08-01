# https://r-analytics.blogspot.com/2014/09/1.html
# https://r-analytics.blogspot.com/2015/01/2.html
# https://r-analytics.blogspot.com/2015/03/3.html
# https://r-analytics.blogspot.com/2016/12/blog-post.html

df <- read.csv("VF_data.csv", sep = ";", dec = ",")

library(psych)
library(ggplot2)
describe(df)

fit <- lm(LN_Y~LN_K+LN_L, df)
summary(fit)
df$fitted <- fit$fitted.values

ggplot(data = df, aes(y = LN_Y, x = year)) +
  geom_point() +
  geom_line(aes(x = year, y = fitted), col = 'red', lwd=1) +
  geom_smooth()

library(lmtest)

dwtest(fit)
# waldtest(fit)
# bptest(fit)

library(car)
vif(fit)

library(mctest)
imcdiag(fit)
mc.plot(fit)
mctest(fit)
