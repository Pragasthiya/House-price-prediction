setwd("C:/Users/welcome/Downloads")
a=read.csv("housing.csv")

head(a)
tail(a)
summary(a)
str(a)

library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(caretEnsemble)
library(randomForest)
library(e1071)


#converting to factor
a$mainroad =as.factor(a$mainroad)
a$guestroom =as.factor(a$guestroom)
a$basement =as.factor(a$basement)
a$hotwaterheating = as.factor(a$hotwaterheating)
a$airconditioning= as.factor(a$airconditioning)
a$prefarea = as.factor(a$prefarea)
a$furnishingstatus = as.factor(a$furnishingstatus)

#To find the number of missing value
is.na(a)
sum(is.na(a))

#finding outliers
boxplot(a$price)
boxplot(a$area)
boxplot(a$bedrooms)
boxplot(a$bathrooms)
boxplot(a$stories)
boxplot(a$parking)

#treating outliers

a$price[a$price > 8e+06] <- 8e+06
boxplot(a$price)

a$area[a$area > 10000] <- 10000
boxplot(a$area)

a$bedrooms[a$bedrooms > 4] <-4
boxplot(a$bedrooms)

a$bathrooms[a$bathrooms > 3] <- 3
boxplot(a$bathrooms)

a$stories[a$stories > 3] <- 3
boxplot(a$stories)

a$parking[a$parking > 2] <- 2
boxplot(a$parking)

#dplyr

a %>% select(bedrooms,mainroad,price) %>% 
  filter(mainroad=='yes') %>% 
  group_by(bedrooms) %>% 
  summarise(avgprice = mean(price))

a %>% select(bedrooms,mainroad,price) %>% 
  filter(mainroad=='no') %>% 
  group_by(bedrooms) %>% 
  summarise(avgprice = mean(price))

a %>% select(price,parking) %>% 
  group_by(parking) %>% 
  summarise(avgprice = mean(price))

a %>% filter(area<5000) %>% 
  summarise(avgprice = mean(price))
  
a %>% filter(area>5000) %>% 
  summarise(avgprice = mean(price))


#ggplot2
ggplot(a,aes(area,price,color = parking))+geom_point()+facet_grid(~bedrooms)
ggplot(a,aes(stories,price))+geom_bar(stat = "identity")+facet_grid(~mainroad)
ggplot(a,aes(price))+geom_histogram()+facet_grid(~bathrooms)
ggplot(a,aes(bedrooms))+geom_histogram()




# find numeric values
nums = unlist(lapply(a, is.numeric)) 
# save numeric variables for later
anums = a[,nums]
# show numeric variables
head(anums)


#correlation and plot
corrplot(cor(anums))
cor(anums)


# select factor variables to convert
varstodummy = a[,sapply(a, is.factor)]
head(varstodummy)
# Create dummy variables with caret
dummies = dummyVars( ~ ., data = varstodummy)
adummy = predict(dummies, newdata = varstodummy)

colnames(adummy)
afull= data.frame(adummy, anums)
View(afull)

#remove near zero variables (except for attr)
removecols = nearZeroVar(afull, names = TRUE)
removecols
# Get all column names 
allcols = names(afull)
# Remove from data
afinal= afull[ , setdiff(allcols, removecols)]



#Data Split
spl = createDataPartition(afinal$price,p = 0.75,list = FALSE)
spl
atrain = afinal[spl,]
atest = afinal[-spl,]

print(dim(atrain));print(dim(atest))


#Model Building

set.seed(123)
model_lm <- train(price ~., 
                  data = atrain, 
                  method = "lm")

summary(model_lm)
prediction = predict(model_lm,atest)
summary(prediction)
rmse <- sqrt(mean((prediction - atest$price)^2))
print(rmse)

rf_model <- train(
  price ~ ., 
  data = atrain, 
  method = "rf")
summary(rf_model)
prediction = predict(rf_model,atest)
summary(prediction)
rmse <- sqrt(mean((prediction - atest$price)^2))
print(rmse)


model <- train(price ~ ., 
               data = atrain, 
               method = "svmRadial")
print(model)
summary(model)
prediction = predict(model,atest)
summary(prediction)
rmse <- sqrt(mean((prediction - atest$price)^2))
print(rmse)


model <- train(price ~ ., 
               data = atrain, 
               method = "rpart")
summary(model)
prediction = predict(model,atest)
summary(prediction)
rmse <- sqrt(mean((prediction - atest$price)^2))
print(rmse)

