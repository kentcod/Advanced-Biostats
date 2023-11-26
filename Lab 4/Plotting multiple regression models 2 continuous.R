#Example of how to plot models with multiple predictor variables
#Model with 2 continuous predictors
#10/5/2022 JS

#load data and library
data(mtcars)
library(car)

#make a linear model explaining fuel efficiency (mpg) using
#car horsepower and weight

mod1 <- lm(mpg ~ hp*wt, data= mtcars)
summary(mod1)

#notice significant interaction bw hp and wt

#Quick and easy way
avPlots(mod1)

#This way is a bit more work, but far more flexible

#Make a dummy data set for a range of horse power, and three level of weight
#this is a little tricky so we'll go step by step

#first, make a sequence of 5 values that covers the full range of hp in the data
hp <- seq(min(mtcars$hp),max(mtcars$hp),length = 5)

#now make a vector of three values for wt, representing a 
#low value ( i.e. the 1st quartile), and medium value (i.e. median), an a higher value
#(i.e. 3rd quartile)
#we can get teh quartiles and median from the summary function:
wt <- summary(mtcars$wt)[c(2,3,5)]
print(wt)

#now use expand.grid to make a data frame with all possible combinations
#of hp and wt
dd <- expand.grid(hp,wt)
names(dd) <- c("hp","wt")
print(dd)

#Now use "predict" and your model to predict mpg for every 
#combination of hp and wt in the dummy data
#and add that to the dummy data (dd)
dd$mpg.pred <- predict(mod1, dd)
print(dd)

#plot the actual data using mpg explained by hp
#Note that I cleverly used the
#cex arguement to scale the point size by square root of car weight (wt)
plot(mpg ~ hp, data = mtcars,
     cex = sqrt(mtcars$wt), 
     pch = 21, bg = "grey75")

#now use subset to plot lines for the dummy data
#I used "lwd" to scale line thickness to indicate predictions for 
#small cars (1st quartile of wt), medium cars (median), and large cars (3rd Q)
lines(mpg.pred ~ hp, data = subset(dd, wt == summary(mtcars$wt)[2]),
      lwd = 1)
lines(mpg.pred ~ hp, data = subset(dd, wt == summary(mtcars$wt)[3]),
      lwd = 2)
lines(mpg.pred ~ hp, data = subset(dd, wt == summary(mtcars$wt)[5]),
      lwd = 3)

#legend
legend("topright", lty = 1, lwd = c(1,2,3), bty = 'n',
       legend = c("2.58 tons","3.32 tons","3.61 tons"))
