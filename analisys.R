# https://r-analytics.blogspot.com/2014/09/1.html
# https://r-analytics.blogspot.com/2015/01/2.html
# https://r-analytics.blogspot.com/2015/03/3.html
# https://r-analytics.blogspot.com/2016/12/blog-post.html

df <- read.csv("VF_data.csv", sep = ";", dec = ",")

library(psych)
describe(df)

summary(lm(Y~K*L, df))

summary(lm(LN_Y~LN_K*LN_L, df))

ggplot(data = df, aes(LN_Y, LN_K)) +
  geom_point() + 
  geom_smooth()

ggplot(data = df, aes(LN_Y, LN_L)) +
  geom_point() + 
  geom_smooth()

library(micEcon)
estResult <- translogEst(yName =  "Y", xNames = c("K", "L"), data = df)
fitted <- cobbDouglasCalc( c("K", "L"), df, coef(estResult)[1:3])

library(lmtest)

dwtest(formula = LN_Y~LN_K:LN_L, data = df)
