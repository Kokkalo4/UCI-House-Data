#The dataset can be found on the following link: http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data
#and Concerns housing values in suburbs of Boston.
#There are 506 observations of 14 variables.
#By analyzing this data I will try to answer the following questions.
#Questions: 
#1) Is there any difference in data when CHAS is 1 or 0? 
#2) Is there a linear relationship between MEDV and all the other or one of the other variables of the data?
#3) Is there any relationship between CRIM and all the other or one of the other variables of the data?

#load file and add the column names
library(data.table)
home <- fread("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
colnames(home) <- c("CRIM", "ZN", "INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

#loading libraries
library(ggplot2)
library(Hmisc)
library(dplyr)


#checking the data
head(home)
tail(home)
str(home)
describe(home)

#Plotting the data, to identify any patterns outliers etc.
plot(home)

ggplot(home, aes(CRIM)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Per capita crime rate by town",y= "Frequency" , title = "Plot of per capita crime rate by town")

ggplot(home, aes(ZN)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Proportion of residential land zoned for lots over 
                 25,000 sq.ft.",y= "Frequency" , title = "Plot of residential land zoned for lots over 
                 25,000 sq.ft.")

ggplot(home, aes(INDUS)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "proportion of non-retail business acres per town",y= "Frequency" , title = "Plot of proportion of non-retail business acres per town")

ggplot(home, aes(CHAS) ) + geom_bar(aes(fill = as.factor(home$CHAS))) + 
  scale_fill_discrete(name="Tract Bounds River",
                      labels=c( "No","Yes")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Tract Bounds River",y= "Frequency" , title = "Plot Examining if Tract Binds River")

ggplot(home, aes(NOX)) + geom_histogram(binwidth=0.015, bins = 50 ,colour="black", fill="green") +
  labs(x= "Nitric oxides concentration (parts per 10 million)",y= "Frequency" , title = "Plot of nitric oxides concentration (parts per 10 million)")

ggplot(home, aes(RM)) + geom_histogram(binwidth=0.05,colour="black", fill="green") +
  labs(x= "Average number of rooms per dwelling",y= "Frequency" , title = "Plot of average number of rooms per dwelling")

ggplot(home, aes(AGE)) + geom_histogram(binwidth=2, colour="black", fill="green") +
  labs(x= "Proportion of owner-occupied units built prior to 1940",y= "Frequency" , title = "Plot of proportion of owner-occupied units built prior to 1940")

ggplot(home, aes(DIS)) + geom_histogram(binwidth=0.15, colour="black", fill="green") +
  labs(x= "Weighted distances to five Boston employment centres",y= "Frequency" , title = "Plot of weighted distances to five Boston employment centres")

ggplot(home, aes(RAD) ) + geom_bar(aes(fill = as.factor(home$RAD))) + 
  scale_fill_discrete(name="Accessibility to Radial Highways") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Accessibility to Radial Highways",y= "Frequency" , title = "Plot Examining Accessibility to Radial Highways")

ggplot(home, aes(TAX)) + geom_histogram(binwidth=5, colour="black", fill="green") +
  labs(x= "Full-value property-tax rate per $10,000",y= "Frequency" , title = "Plot of full-value property-tax rate per $10,000")

ggplot(home, aes(PTRATIO)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Pupil-teacher ratio by town",y= "Frequency" , title = "Plot of pupil-teacher ratio by town")

ggplot(home, aes(B)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
                 by town",y= "Frequency" , title = "Plot of 1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
                 by town")

ggplot(home, aes(LSTAT)) + geom_histogram(binwidth=0.5, colour="black", fill="green") +
  labs(x= "% lower status of the population",y= "Frequency" , title = "Plot of % lower status of the population")

ggplot(home, aes(MEDV)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Median value of owner-occupied homes in $1000's",y= "Frequency" , 
       title = "Plot of Median value of owner-occupied homes in $1000's")


#Split data with home$CHAS set to either 0 or 1, where CHAS is Charles River dummy variable and has 2 values. 
#1 if tract bounds river and 0 otherwise.

home0 <- home %>%
           filter(CHAS == 0)

home1 <- home %>% 
           filter(CHAS == 1)


#and comparing the results.
library(gridExtra)

grid.arrange(crim0, crim1 , nrow = 2, ncol = 1)
crim0 <- ggplot(home0, aes(CRIM)) + geom_histogram(binwidth=2, colour="black", fill="green") +
  labs(x= "Per capita crime rate by town",y= "Frequency" , title = "Plot of per capita crime rate by town")
crim1 <- ggplot(home1, aes(CRIM)) + geom_histogram(binwidth=0.5, colour="black", fill="green") +
  labs(x= "Per capita crime rate by town",y= "Frequency" , title = "Plot of per capita crime rate by town")

grid.arrange(zn0, zn1 , nrow = 2, ncol = 1)
zn0 <- ggplot(home0, aes(ZN)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Proportion of residential land zoned for lots over 
       25,000 sq.ft.",y= "Frequency" , title = "Plot of residential land zoned for lots over 
       25,000 sq.ft.")
zn1 <- ggplot(home1, aes(ZN)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "Proportion of residential land zoned for lots over 
       25,000 sq.ft.",y= "Frequency" , title = "Plot of residential land zoned for lots over 
       25,000 sq.ft.")

grid.arrange(indus0, indus1 , nrow = 2, ncol = 1)
indus0 <- ggplot(home0, aes(INDUS)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "proportion of non-retail business acres per town",y= "Frequency" , title = "Plot of proportion of non-retail business acres per town")
indus1 <- ggplot(home1, aes(INDUS)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "proportion of non-retail business acres per town",y= "Frequency" , title = "Plot of proportion of non-retail business acres per town")

grid.arrange(nox0, nox1 , nrow = 2, ncol = 1)
nox0 <- ggplot(home0, aes(NOX)) + geom_histogram(binwidth=0.015, bins = 50 ,colour="black", fill="green") +
  labs(x= "Nitric oxides concentration (parts per 10 million)",y= "Frequency" , title = "Plot of nitric oxides concentration (parts per 10 million)")
nox1 <- ggplot(home1, aes(NOX)) + geom_histogram(binwidth=0.015, bins = 50 ,colour="black", fill="green") +
  labs(x= "Nitric oxides concentration (parts per 10 million)",y= "Frequency" , title = "Plot of nitric oxides concentration (parts per 10 million)")

grid.arrange(rm0, rm1 , nrow = 2, ncol = 1)
rm0 <- ggplot(home0, aes(RM)) + geom_histogram(binwidth=0.05,colour="black", fill="green") +
  labs(x= "Average number of rooms per dwelling",y= "Frequency" , title = "Plot of average number of rooms per dwelling")
rm1 <- ggplot(home1, aes(RM)) + geom_histogram(binwidth=0.05,colour="black", fill="green") +
  labs(x= "Average number of rooms per dwelling",y= "Frequency" , title = "Plot of average number of rooms per dwelling")

grid.arrange(age0, age1 , nrow = 2, ncol = 1)
age0 <- ggplot(home0, aes(AGE)) + geom_histogram(binwidth=2, colour="black", fill="green") +
  labs(x= "Proportion of owner-occupied units built prior to 1940",y= "Frequency" , title = "Plot of proportion of owner-occupied units built prior to 1940")
age1 <- ggplot(home1, aes(AGE)) + geom_histogram(binwidth=2, colour="black", fill="green") +
  labs(x= "Proportion of owner-occupied units built prior to 1940",y= "Frequency" , title = "Plot of proportion of owner-occupied units built prior to 1940")

grid.arrange(dis0, dis1 , nrow = 2, ncol = 1)
dis0 <- ggplot(home0, aes(DIS)) + geom_histogram(binwidth=0.15, colour="black", fill="green") +
  labs(x= "Weighted distances to five Boston employment centres",y= "Frequency" , title = "Plot of weighted distances to five Boston employment centres")
dis1 <- ggplot(home1, aes(DIS)) + geom_histogram(binwidth=0.15, colour="black", fill="green") +
  labs(x= "Weighted distances to five Boston employment centres",y= "Frequency" , title = "Plot of weighted distances to five Boston employment centres")

grid.arrange(rad0, rad1 , nrow = 2, ncol = 1)
rad0 <- ggplot(home0, aes(RAD) ) + geom_bar(aes(fill = as.factor(home$RAD))) + 
  scale_fill_discrete(name="Accessibility to Radial Highways") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Accessibility to Radial Highways",y= "Frequency" , title = "Plot Examining Accessibility to Radial Highways")
rad1 <- ggplot(home1, aes(RAD) ) + geom_bar(aes(fill = as.factor(home$RAD))) + 
  scale_fill_discrete(name="Accessibility to Radial Highways") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Accessibility to Radial Highways",y= "Frequency" , title = "Plot Examining Accessibility to Radial Highways")

grid.arrange(tax0, tax1 , nrow = 2, ncol = 1)
tax0 <- ggplot(home0, aes(TAX)) + geom_histogram(binwidth=5, colour="black", fill="green") +
  labs(x= "Full-value property-tax rate per $10,000",y= "Frequency" , title = "Plot of full-value property-tax rate per $10,000")
tax1 <- ggplot(home1, aes(TAX)) + geom_histogram(binwidth=5, colour="black", fill="green") +
  labs(x= "Full-value property-tax rate per $10,000",y= "Frequency" , title = "Plot of full-value property-tax rate per $10,000")

grid.arrange(ptratio0, ptratio1 , nrow = 2, ncol = 1)
ptratio0 <- ggplot(home0, aes(PTRATIO)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Pupil-teacher ratio by town",y= "Frequency" , title = "Plot of pupil-teacher ratio by town")
ptratio1 <- ggplot(home1, aes(PTRATIO)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Pupil-teacher ratio by town",y= "Frequency" , title = "Plot of pupil-teacher ratio by town")

grid.arrange(b0, b1 , nrow = 2, ncol = 1)
b0 <- ggplot(home0, aes(B)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
                 by town",y= "Frequency" , title = "Plot of 1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
                 by town")
b1 <- ggplot(home1, aes(B)) + geom_histogram(binwidth=4, colour="black", fill="green") +
  labs(x= "1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
                 by town",y= "Frequency" , title = "Plot of 1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
                 by town")

grid.arrange(lstat0, lstat1 , nrow = 2, ncol = 1)
lstat0 <- ggplot(home0, aes(LSTAT)) + geom_histogram(binwidth=0.5, colour="black", fill="green") +
  labs(x= "% lower status of the population",y= "Frequency" , title = "Plot of % lower status of the population")
lstat1 <- ggplot(home1, aes(LSTAT)) + geom_histogram(binwidth=0.5, colour="black", fill="green") +
  labs(x= "% lower status of the population",y= "Frequency" , title = "Plot of % lower status of the population")

grid.arrange(medv0, medv1 , nrow = 2, ncol = 1)
medv0 <- ggplot(home0, aes(MEDV)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Median value of owner-occupied homes in $1000's",y= "Frequency" , 
       title = "Plot of Median value of owner-occupied homes in $1000's")
medv1 <- ggplot(home1, aes(MEDV)) + geom_histogram(binwidth=1, colour="black", fill="green") +
  labs(x= "Median value of owner-occupied homes in $1000's",y= "Frequency" , 
       title = "Plot of Median value of owner-occupied homes in $1000's")


#Identify any correlations. Using corr() function and plotting it with corrplot() function.
library(corrplot)
cormat <- cor(home)
corrplot(cormat, type="upper", order="hclust", tl.col="black", tl.srt=45)

#From corrplot chart we see that there are some strong indications of correlation between the variables. On many cases
#there are strong positive correlations and on other cases strong negative correlation that we are going to examine further.



#For the regression part of the analysis I will try to work with "MULTIPLE REGRESSION" tutorial by William B. King
#that was found online at: https://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr.html

#SPLITTING THE DATA
set.seed(123)
rows <- sample(1:nrow(home),size = 0.75*nrow(home))
train <- home[train]
test <- home[-train]

#THE MINIMAL ADEQUATE MODEL.
#First we begin by throwing all predictors inside a linear model. 

model1 <- lm(MEDV ~ ., data = train)
summary(model1)

#From the model summary we see that the dependent variable MEDV(which is the Median value of owner-occupied homes in $1000's)
#is related with: CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, PTRATIO, B and LSTAT. While on the other hand it is not related with
#INDUS and AGE.

#Now what we want to do is to reduce the model to a point where all the predictors are significant. So, what we will do 
#is to start eliminating the non-significant predictors, one by one.

model2 <- lm(MEDV ~ . - INDUS, data = train)
summary(model2)

#From the summaries of the 2 models we see that Multiple R-squared has remained the same but the Adjusted R-Squared has gone
#a bit up due to removing the variable INDUS.
#We can compare the 2 models as follows:
anova(model2,model1)

#Now we remove the variable AGE, which is the proportion of owner-occupied units built prior to 1940.

model3 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT , data = train)
summary(model3)

#Checking the summary of the third model we see that there have been left only the variables that have strong relations to the 
#dependent variable MEDV. From the model R-squared has remained the same as it was on the first model and Adjusted R-squared
#has gone a little bit up from 0.7338(model1) to 0.7348(model3).
#Also, some pvalues have gone up or down but there is no change in their significance, as they all remain significant.
#This means that we have reached the minimal adequate model in which all the slopes are statistically significant.

#STEPWISE REGRESSION
#The same as the above procedure can be done also with stepwise regression using the step() function.

step(model1, direction = "backward")

#From the output we see that the model that the step function proposes is the the one that we have reached above with
#minimal adequate model.
#So, I'll work with model3.

#Confidence Limits on the Estimated Coefficients
confint(model3)

#Model predictions.
#Predictions can be made from a model equation using the predict() function.

#predict(model3, .... )
#eg 
newdata = test
pred <- predict(model3, newdata)

#Where in the dotted space(....) we feed the model with some values for the predictors.

#Predicted values
head(pred)
#Actual Values
head(test[,"MEDV"])



#REGRESSION DIAGNOSTICS
par(mfrow = c(2,2))
plot(model3)
par(mfrow = c(1,1))

#The first of these plots (upper left) is a standard residuals plot with which we can evaluate linearity 
#and homoscedasticity. The second plot (upper right) is a normal quantile plot of the residuals, which should 
#be normally distributed. We have a little deviation from normality in the tails, but it's not too bad. The scale-location
#plot is supposedly a more sensitive way to look for heteroscedasticity.

#Extracting Elements of the Model Object
names(model3)

#To get the coefficients of the model we use:
coef(model3)

