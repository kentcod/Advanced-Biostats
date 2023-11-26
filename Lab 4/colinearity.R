# Example of colinearity

# x1 is random sample of number 1 to 100 
x1 <- sample(1:100, 50)

# x2 is some other random variable that happens to be strongly correlated with x1
x2 <- x1 + rnorm(mean= 0, sd = 1, n = 50)

plot(x1,x2)

# Y is a function of x1 according to the following formula
# Pay attention to the coefficients!
y = 50 + 5*x1 + rnorm(50,0,20)

plot(y~x1)
plot(y ~x2)

mod1 <- lm(y ~ x1)
summary(mod1)
#spits back estimate and x1 coefficients (y intercept and slope - Beta) pretty good

mod2 <- lm(y~x2 + x1)
summary(mod2)
# not good 

#Overall, if colinear variables exist in model, it can mess up model's prediction of actually important var
# if variables not collinear, model will consistently deliver significant results to IMPORTANT VARIABLE

#Re-run the previous 7 lines of code several times. 
#Note how consistent mod1 is, and how inconsistent mod2 is.