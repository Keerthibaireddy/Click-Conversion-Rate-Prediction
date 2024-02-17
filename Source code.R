#Predicting Click Conversion Rates

rm(list = ls())
library(rio)
df = import("Final SDM Project File.xlsx")
View(df)
str(df)
table(df$CURRENCY)
table(df$BRAND)
table(df$SHOP_LOCATION)
table(df$CATEGORY)

#Check for NULLs
colSums(is.na(df))
#review rating, review count, sold, historical sold, 
#liked count has NULLs
df$SHOW_DISCOUNT = NULL

df <- df[complete.cases(df),]  
colSums(is.na(df))

#factors
df$CATEGORY = factor(df$CATEGORY)
df$SHOP_LOCATION = factor(df$SHOP_LOCATION)

df$CREATE_TIME_YEAR = format(df$CREATE_TIME, "%Y")
df$CREATE_TIME_YEAR = as.factor(df$CREATE_TIME_YEAR)
df$CREATE_TIME_MONTH = format(df$CREATE_TIME, "%b")
df$CREATE_TIME_MONTH = as.factor(df$CREATE_TIME_MONTH)

plot(df$conv_rate ~ df$CREATE_TIME_YEAR)

#Predictors
#Sale price
#review rating
#review count
#historical sold
#stock
#discount %
#create_year
#category

#response variable - conversion
df$conversion = ifelse(df$SOLD != 0 & df$VIEW_COUNT !=0, df$SOLD/df$VIEW_COUNT, 0)

df$conv_rate = df$conversion*100
summary(df$conversion)
df$conv_rate = round(df$conv_rate,0)

plot(df$conversion~df$SOLD)

#exploratory data
hist(df$conversion, col = "red", probability = T)
hist(log(df$conversion), col = "red", probability = T)
par(mfrow = c(1,1))
hist(df$conv_rate, col = "red", probability = T)
hist(log(df$conv_rate), col = "red", probability = T)
#log distribution looks even more dispersed


hist(df$VIEW_COUNT, col = "blue", prob = T)
hist(log(df$VIEW_COUNT), col = "blue", prob = T)

plot(df$REVIEW_COUNT,df$conversion)
unique(df$conversion)
str(df)

df$CURRENCY = factor(df$CURRENCY)

plot(df$conversion~df$CREATE_TIME_YEAR, pch = 19)
bwplot(~df$conversion | df$CATEGORY)

##check for collinearity
df_num = c("SALE_PRICE","REVIEW_RATING","REVIEW_COUNT","LIKED_COUNT",
           "Discount_percent","conv_rate")
library(corrplot)
m = cor(df[,df_num])
corrplot(m, method = "number")

View(df)

summary(df$conv_rate)

#conversion rate is a censored data
#use tobit model 

#since DV: Conversion is sold/impression which is count/count
# a percentage, therefore we can use OLS regression 


lm1 = lm(conv_rate~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT + VIEW_COUNT +
           HISTORICAL_SOLD + STOCK + Discount_percent + CATEGORY + CREATE_TIME_YEAR, 
         data = df)

summary(lm1)

library(AER)
library(censReg)
tobit1 = tobit(conv_rate~ SALE_PRICE + REVIEW_RATING + REVIEW_COUNT + VIEW_COUNT +
                   HISTORICAL_SOLD + STOCK + Discount_percent + CATEGORY + 
                   CREATE_TIME_YEAR, left = 0, right = 100, data = df)
summary(tobit1)

#Assumptions
#Linearity
plot(df$conv_rate,tobit1$fitted.values,
     pch=19,main="Actual vs Fitted Values")
abline(0,1,col="red",lwd=3)

#Normality
qqnorm(resid(tobit1))


#interaction between stock and price to check when price is low 
#and stock is high - does conversion improve?
#how different categories affect conversion with discount and higher review rating respectively

tobit2 = tobit(conv_rate~ CATEGORY*REVIEW_RATING + REVIEW_COUNT*VIEW_COUNT + 
                   LIKED_COUNT + HISTORICAL_SOLD + STOCK*SALE_PRICE +
                   Discount_percent*CATEGORY + CREATE_TIME_YEAR, left = 0, 
                 right = 100, data = df)
summary(tobit2)

library(stargazer)
stargazer(lm1, tobit1, tobit2, single.row = TRUE, type = "text", out = "out.txt")


#calculate psedo R2
library(pscl)
pR2(tobit2)['McFadden']

pR2(tobit2, method = "mckelveyZavoina")

#assumptions
plot(tobit2)

summary(df$conv_rate)

#linearity
plot(df$conv_rate, tobit2$fitted.values, pch = 19,
     main = "Conversion rate Vs Fitted Values", xlab = "Conversion rate", 
     ylab = "Fitted Values")
abline(0,1,lwd = 3, col = "red")

ggplot(data.frame(fitted = fitted(model), residual = residuals(model)), aes(x = fitted, y = residual)) +
  geom_point() +
  geom_smooth(method = "loess") +
  ggtitle("Linearity: Residuals vs Fitted")

#independence
tobit2$fitted.values
durbinWatsonTest(resid(tobit2))


library(AER)
dispersiontest(glm1)

library(car)
vif(glm1)