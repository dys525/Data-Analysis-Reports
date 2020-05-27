#our data
data <- read.table("gauge.txt", header=TRUE)
head(data)

#gain histogram
hist(data$gain)

# fitting without transformation
fit <- lm(formula=density~gain, data=data)
summary(fit)

# set up the predictied and residuals variables
copy <- data.frame(data)
predicted <- predict(fit)
residuals <- residuals(fit)

# ggplot to show residual distance and regression line
library(ggplot2)
(ggplot(data, aes(x = gain, y = density))+geom_smooth(method = "lm", se = FALSE, color = "red")+
 geom_segment(aes(xend = gain, yend = predicted), alpha = .2)+geom_point()+geom_point(aes(y = predicted), shape = 1)
 +theme_bw())

plot(fit$residuals)
abline(0, 0, col="red")

hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals, col="red")

#When we see the data through the histogram, we can observe that our data is highly skewed to the right side. Thus, we apply the logarithm to our gain data to make less skewed data
log_df = data.frame(data)
logarithmic <- function(x) {
return (log(x,base=exp(1)))
}
log_df$gain <- logarithmic(log_df$gain)
head(log_df)

# we can observed the data is less skewed
hist(log_df$gain)

#fit data and setup predicted and residuals
log_fit <- lm(formula=density~gain, data=log_df)
predicted <- predict(log_fit)
residuals <- residuals(log_fit)
summary(log_fit)

rmse = function(pred, act){
  sqrt(mean((pred - act)^2))
}
rmse(predicted,log_df$gain)

library(ggplot2)
(ggplot(log_df, aes(x = gain, y = density))+geom_smooth(method = "lm", se = FALSE, color = "red")+
 geom_segment(aes(xend = gain, yend = predicted), alpha = .2)+geom_point()+geom_point(aes(y = predicted), shape = 1)
 +theme_bw())

plot(log_fit$residuals)
abline(0, 0, col="red")

# residual be more normalized than before
hist(log_fit$residuals)
qqnorm(log_fit$residuals)
qqline(log_fit$residuals, col="red")

Density= log_df$density
Gain = log_df$gain
new_fit <- lm(Density ~ Gain)
pts <- seq(0, 600, length.out=90)
CI.conf <- predict(new_fit, data.frame(Gain = pts), interval = "confidence") #confidence interval
CI.pred <- predict(new_fit, data.frame(Gain = pts), interval = "predict") #prediction interval
plot(Gain, Density)
lines(pts, CI.conf[,"fit"], col="black", lwd=2)
lines(pts, CI.conf[,"lwr"], col="blue", lwd=1) 
lines(pts, CI.conf[,"upr"], col="blue", lwd=1)
lines(pts, CI.pred[,"lwr"], col="red", lwd=1)
lines(pts, CI.pred[,"upr"], col="red", lwd=1)

summary(fit)$r.squared
summary(log_fit)$r.squared

library(randomForest)
df_bag = randomForest(gain~density, data = log_df)

predicted <- predict(df_bag)
residuals <- residuals(df_bag)

1- (sum((Density-predicted)^2)/sum((Density-mean(Density))^2))

rmse = function(pred, act){
  sqrt(mean((pred - act)^2))
}
rmse(predicted,log_df$gain)