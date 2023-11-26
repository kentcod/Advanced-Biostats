#intro to R plotting example
#sep20 2022 JS


#installing and loading packages
####################################################################
#installing the actual code to your computer from an outside source
#Only needs to be done once per computer (unless it gets deleted)
#install.packages("vegan")
  #or use Tools --> Install packages and search for package
#load the contents of the package into your workspace in r
#this must be done during every R session
library(vegan)
  #can also use require(vegan)


#load the combined crayfish data from last week and have a look
setwd("C:/Users/Kent Codding/Desktop/Adv Biostat/Lab 3")
df <- read.csv("combined_crayworm_data.csv")

df <-na.omit(df) #remove missing data


#Make a scatter plot to examine the relationship between crayfish length and mass
#show examples of $,~, pch, cex, bg, data=
# ~ means 'by' or more specifically 'explained by'
# e.g. response variable explained by explanatory

plot(C_ingens ~ LENGTH, data = df,
     xlab = "length (mm)", ylab = "worm diversity)",
     main = "mudbug worms vs length",
     xlim = c(22,40), pch =21, 
     bg = "grey75", cex = 0.03 * df$LENGTH) #bg is background color (fill); cex is size (can scale by length)

#plot output of a function
worm_D <- diversity(df[,6:15]) #vegan calculates a diversity index for each worm (columns)
plot(df$LENGTH, worm_D) #bigger crayfish have more worm diversity!


#boxplot of mass of all crayfish
boxplot(df$LENGTH)

#Find the outliers!!
summary(df$LENGTH)

# get IQR
IQR(df$LENGTH)


# get threshold values for outliers
Tmin = 32.25-(1.5*3) #first quartile minus 1.5*IQR
Tmax = 35.25+(1.5*35) ##third quartile plus 1.5*IQR

# show rows of df with outliers for LENGTH
df[df$LENGTH < Tmin | df$LENGTH > Tmax,]



#boxplot to compare distribution of masses of two species
boxplot(df$MASS ~ df$SPEC, col = c("purple","peachpuff"))
#boxplot has no data = df arg, also has to be in ~ form

#histogram
hist(df$MASS) #breaks arg det. # of bins

#side by side or multipanel plotting
##################################
par(mfrow = c(1,2)) #mfrow set up like matrix c(numrows, numcols)

dev.off() #turns off multipanel plotting

#saving directly to file
pdf(file = "crayfish_mass_histogram_square.pdf", width = 5, height = 5)
  #pdfs are vector based, so can export to pdf at 5x5 and then increase size without resolution decreasing
hist(df$MASS)
dev.off()
