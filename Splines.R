# Homework 10 Abigail Kaff
#Use the unemployment.csv dataset and subset it to include only data from the United States. Use at least two of the approaches we covered this week (cubic splines, natural splines, smoothing splines, local regression, or GAMs) and model the unemployment rate over the years in these data. Show a figure with two curves (on top of the scatter plot will help your interpretation) that show the results for the two methods, making sure to label the curves so I can tell which is which. Do some interpretation of what you are seeing in the data (point is applying the methods rather than to show the best model).

library(ISLR)
library(splines)

setwd("~/DATA 881/Week 10")
data <- read.csv("./unemployment_1.csv")

# Cubic
data <- subset(data, country == 'United States')
data <- subset(data, select = -c(country) )
data <- as.data.frame(t(data))
rownames(data2) <- data2$Year
mod = lm(Unemployment ~ Year, data)

plot(data$Year, data$Unemployment)
preds = predict(mod)

ord = order(data$Year)
lines(data$Year[ord], preds[ord], col='red')

# Natural spline
set.seed(1)
train = sample(1:nrow(data), 2/3*nrow(data))
test = setdiff(1:nrow(data), train)

mod2 = lm(Unemployment ~ ns(Year, df=5), data[train,])

preds2 = predict(mod2, data[test,])

mod_poly = lm(Unemployment ~ poly(Year, 15), data[train,])

preds_poly = predict(mod_poly, data[test,])

sum((data[test,]$Unemployment - preds)^2) / length(test)
sum((data[test,]$Unemployment - preds_poly)^2) / length(test)

#with(data[test,], plot(Year, Unemployment))
ord = order(data[test,]$Year)
lines(data[test,]$Year[ord], preds[ord], col='orange')
lines(data[test,]$Year[ord], preds_poly[ord], col='green')
legend(1950, 8, legend=c("Cubic", "Natural Spline","Poly-Spline"),
       col=c("red", "orange","green"), lty=1:3, cex=0.5)

# I'd say just looking at cubic vs natural spline methods, it seems to me as cubic does take into account better than natural spline the variety of the data. as it has a larger incline allowing it to compensate for the incredibly high unemployment that happens around the 2010 time frame. 