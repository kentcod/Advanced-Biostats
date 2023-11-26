#Using GAM
#October 17, 2023
library(gam)
library(mgcv)

#WARNING
#This is for demonstration purposes only
#A real analysis of temporal trends requires additional
#consideration beyond our scope for today
## this is because of non-independence

#get temp data from web
CET <- url("https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt")

#take a look
writeLines(readLines(CET, n = 10))

cet <- read.table(CET, sep = "", skip = 4, header = TRUE,
                   fill = TRUE, na.string = c(-99.99, -99.9))

#remove incomplete 2023 data row
cet <- cet[!cet$Year=="2023",]

#make variable for year of record
cet$yr_rec <- as.numeric(rownames(cet))
cet$Annual <- cet$Annual - mean(cet$Annual)
#plot data
plot(Annual ~ Year, data = cet)

#Try simple linear model#
mod1 <- lm(Annual ~ yr_rec, data = cet)
summary(mod1)

par(mfrow=c(1,2))

#simple plot of model over raw data
plot(Annual ~ yr_rec, data = cet)
  abline(mod1, lwd = 3, col = "red")

#check for nonindependence/ non-linearity
plot(cet$yr_rec, residuals(mod1)) #residuals do not look linear (could be dependent)
  abline(h=0,lwd = 3, col = "red")
  
#Try quadratic linear model#
mod2 <- lm(Annual ~ poly(yr_rec,3), data = cet) # poly() means polynomial for the variable yr_rec and for each power up to 3
  summary(mod2)

  #and check again  
plot(Annual ~ yr_rec, data = cet)
  lines(cet$yr_rec,predict(mod2), lwd = 3, col = "red")
plot(cet$yr_rec, residuals(mod2))
  abline(h=0, lwd = 3, col = "red")

#still some misses in this model although its a bit better
  
###########################################
#fit a GAM using gam
mod.gam.gam <-  gam::gam(cet$Annual ~ lo(cet$yr_rec, span = .2), family = "gaussian") 

#visually examine the model fit
pdat1 <- predict(mod.gam.gam, se = T)
  
  plot(Annual ~ yr_rec, data = cet,
       pch = 21, bg = "grey95", 
       col = "grey50",
       ylab = "mean annual temperature (Celsius)",
       xlab = "calendar year")
  
  lines(cet$yr_rec, pdat1$fit, lwd = 2)
  lines(cet$yr_rec, pdat1$fit + 1.96*pdat1$se.fit,
        lwd = 1.5, lty = 2) 
  lines(cet$yr_rec, pdat1$fit - 1.96*pdat1$se.fit, 
        lwd = 1.5, lty = 2)  
  

  plot(cet$yr_rec, residuals(mod.gam.gam))
  abline(h=0, col = "red", lwd = 3)
  
#examine model output
summary(mod.gam.gam)  
  #compare parametric and non-par results to determine if a smoother is needed
  #Anova for non-par sig <- leave as a smoother
  #Anova for non-par in not sig, but parametric is, change to a regular linear
  #Neither are sig, drop the variable.

#How could we get a R2 from this output? Null dev. - Residual dev./null dev = pseudo R squared
#How does F and p on non-par change with span?
###########################################

#Fit a GAM using mgcv
mod.gam2 <- mgcv::gam(Annual ~ s(yr_rec, bs = "cr", k = 40),  data = cet)

pdat2 <- predict(mod.gam2, se = T)

plot(Annual ~ yr_rec, data = cet,
     pch = 21, bg = "grey95", 
     col = "grey50",
     ylab = "mean annual temperature (Celsius)",
     xlab = "calendar year", main = "gamma = 5")

  lines(cet$yr_rec[order(cet$yr_rec)], pdat2$fit[order(cet$yr_rec)], lwd = 2)
  lines(cet$yr_rec[order(cet$yr_rec)], pdat2$fit[order(cet$yr_rec)] + 1.96*pdat2$se.fit[order(cet$yr_rec)],
        lwd = 1, lty = 2) 
  lines(cet$yr_rec[order(cet$yr_rec)], pdat2$fit[order(cet$yr_rec)] - 1.96*pdat2$se.fit[order(cet$yr_rec)], 
        lwd = 1, lty = 2) 


  plot(cet$yr_rec, residuals(mod.gam2))
  abline(h=0, lwd = 3, col = "red")

  summary(mod.gam2)
  gam.check(mod.gam2)

  