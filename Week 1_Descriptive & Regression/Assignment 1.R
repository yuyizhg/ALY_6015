#Part A:You may use the Trees dataset in R after you invoke R.
trees <- read.csv("FCR/NEU/CPS/Analytics_2018/ALY 6015_Intermediate Analytics/Week 1/Assignment 1_trees.csv", header = TRUE)


#A.1 Using summary() to find the five summary numbers
summary(trees)


#A.2 Using summary() and lm () functions for straight line regression function
## Independent Variable
Girth <- trees[,c('Girth')]

## Dependent Variables
Height <- trees[,c('Height')]
Volume <- trees[,c('Volume')]

## Creating Regression Equation
Regression <- lm(Girth ~ Height + Volume)

## Show the results
summary(Regression)


#A.3 Using hist (), density() function for Histograms and Density Plots
## Create Histogram
hist(Girth, breaks = 20, xlim = c(5,25), ylim = c(0,6), col = "#ffffe6", main = "Distribution of Black Cherry Trees by Girth", xlab = "Girth")

## Create Density Plots
g <- density(Girth)
plot(g, main = "Girth")
polygon(g, col = "#ffffe6")


#A.4 Using boxplot() and rug() (also see section 3.6.3) for Boxplots
boxplot(Girth, col = "#ffffe6", main = "Black Cheery Trees", xlab = "Grith", ylim = c(7,22), frame.plot = TRUE, boxwex = 0.35, horizontal = TRUE)
rug(Girth, side = 1)


#A.5 Using rnorm () and qqnorm () for Normal probability plots
##rnorm
plot(rnorm(5000), main = "rnorm plots (mean=0, sd=1)")

##qqnorm
head(trees)
qqnorm(trees$Height)
qqline(trees$Height, col = "red")



#Part B:You may use the Rubber and oddbooks data set in R after you invoke R 
#to show the model with insights, correlation matrix and explanations.
library(MASS)
library(ggplot2)
library(ggcorrplot)
library(DAAG)


##B.1 Rubber
summary(Rubber)
pairs(Rubber)

Rubber.lm <- lm(loss~hard+tens, data = Rubber)
summary(Rubber.lm)
Residuals <- resid(Rubber.lm)
plot(Rubber$loss,Residuals, main = 'Residual Plots')

par(mfrow=c(1,2))
termplot(Rubber.lm, partial = TRUE, smooth = panel.smooth)
par(mfrow=c(1,1))

ggcorrplot(cor(Rubber), method = "circle")
###Explanations: 
###To illustrate, the value of multiple coefficient of determination, R-squared is 0.84. This high value of R-squared implies that using the independent variables hard and tens explains 84% of the total sample variation in loss.
###According to correlation plots, hard and loss are quite negative related. 


##B.2 Oddbooks
logbooks <- log(oddbooks)

logbooks.lm1 <- lm(weight ~ thick, data = logbooks)
summary(logbooks.lm1)

logbooks.lm2 <- lm(weight ~ thick + height, data = logbooks)
summary(logbooks.lm2)

logbooks.lm3 <- lm(weight ~ thick + height + breadth, data = logbooks)
summary(logbooks.lm3)

ggcorrplot(cor(oddbooks), hc.order = TRUE, method = "circle")
### Explanations: 
###The value of R-squared is gradually increased since more indenpandent factors have been added one by one. Therefore in logbooks.lm3, independent variables thick, height and width could explains 89.8% of the total book weight.
###According to correlation plot, breadth and height are positive related. Thick is negative related with breadth and height.
