library(lmtest)
library(gvlma)
library(mctest)
library(car)
library(magrittr)

central.difference <- function(y) {
  n <- length(y)
  fdx <- vector(length = n)
  fdx[1] <- (y[2] - y[1])/y[1]
  fdx[n] <- (y[n] - y[n-1])/y[n]
  
  for (t in 2:(n-1)) {
    fdx[t] <- (y[t+1] - y[t-1])/(2*y[t])
  }
  
  return(fdx)
}

concat <- function(item){
  paste("Y~",paste(item, collapse = '+'), sep="")
}

df <- read.csv("VF_data.csv", sep = ";", dec = ",")
df$X <- NULL
df$K_own <- NULL
df$Op_res <- NULL

N <- nrow(df)
norm1 <- as.data.frame(do.call(rbind, lapply(1:nrow(df), function(i) log(df[i,]/df[1,]))))
idf <- as.data.frame(do.call(rbind, lapply(1:nrow(df), function(i) df[i,]/df[1,])))
idf$dY <- central.difference(idf$Y)

lm_comb <- function(f_str){
  stats <- summary(lm(as.formula(f_str), norm1))
  c(f_str, stats$adj.r.squared, stats$fstatistic)
}

f <- names(norm1)

res_df <- as.data.frame(
  t(
    as.data.frame(
      lapply(
        lapply(as.list(as.data.frame(
          combn(f[-which(f == "Y")], 2))), concat), lm_comb)
      )))

names(res_df) <- c("Formula", "R_adj", "F_stat")
res_df$R_adj <- as.numeric(res_df$R_adj)
res_df$F_stat <- as.numeric(res_df$F_stat)

best <- res_df[res_df$R_adj>0.75,]
best <- best[order(best$R_adj, decreasing = T),]

tests <- function(fit){ 
  t <- dwtest(fit)
  dw <- setNames(c(t$statistic, t$p.value), c("Durbin-Watson", "p-value"))
  t <- summary(gvlma(fit))
  het <- setNames(c(t$Value[5], t$`p-value`[5]), c("Heteroskedasticity", "p-value"))
  vf <- setNames(vif(fit), c("VIF_1", "VIF_2"))
  wi <- setNames(mctest(fit)$odiags[2, ], c("Farrar Chi-Square", "detection"))
  md <- idf[,names(fit$model)]
  
  dev_c <- sum(((idf$Y < md[2]) & (idf$Y < md[3])) | 
                 ((idf$Y > md[2]) & (idf$Y > md[3])))/N
  dev <- setNames(c(dev_c), c("Y deviation"))
  
  md$dX1 <- central.difference(md[2])
  md$dX2 <- central.difference(md[3])
  ddev_c <- sum(((idf$dY < md$dX1) & (idf$dY < md$dX2)) | 
                 ((idf$dY > md$dX1) & (idf$dY > md$dX2)))/N
  ddev <- setNames(c(ddev_c), c("dY deviation"))
  
  c(dw, het, vf, wi, dev, ddev)
}

# fit <- lm(Y~Hired_count+Y_industry, data = norm1)
# md <- fit$model

# md$dY <- central.difference(md$Y)
# md$dX1 <- central.difference(md[2])
# md$dX2 <- central.difference(md[3])

best <- cbind(best, as.data.frame(do.call(rbind, 
                                          lapply(best$Formula, function(f_str) lm(as.formula(f_str), norm1)) %>% 
                                            lapply(tests))))

# ggplot(idf, aes(x = 1:9, y = Y)) + 
#   geom_line(col = 'green', lwd = 2) + 
#   geom_line(aes(x = 1:9, y = Hired_count), col = 'red', lwd=1) +
#   geom_line(aes(x = 1:9, y = Y_industry), col = 'blue', lwd=1)
