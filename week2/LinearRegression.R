wine = read.csv("wine.csv")

str(wine)
 # create a one-variable linear regression equation using AGST to predict Price.
model1 = lm(Price ~ AGST, data = wine)

summary(model1) 

 # residuals, or error terms, are stored in the vector
model1$residuals

 # compute the sum of squared errors, or SSE
SSE = sum(model1$residuals^2)

 # new modal
model2 = lm(formula = Price ~ AGST + HarvestRain, data = wine)

 # better
SSE = sum(model2$residuals^2)

model3 = lm(formula = Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)

model4 = lm(formula = Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)

model5 = lm(formula = Price ~ AGST + HarvestRain + WinterRain, data = wine)

 # The standard error column gives a measure of how much the coefficient is likely to vary from the estimate value.
 # t_value = estimate/std.error
 # The larger the absolute value of the t value, the more likely the coefficient(estimate) is to be significant.
 # look at the stars at the end pf summary, more stars is better

 #correlation: 
cor(wine$WinterRain, wine$Price)

cor(wine)

 # A high correlation between an independent variable and the dependent variable is a good thing

wineTest = read.csv("wineTest.csv")

str(wineTest)
 #make a prediction with model 
predictTest = predict(model4, newdata = wineTest)

 # R-value of model
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

