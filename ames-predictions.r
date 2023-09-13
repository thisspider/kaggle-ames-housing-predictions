#prep
library(randomForest)
set.seed(123)
amestrain <- read.csv("amestrain.csv")
amestest <- read.csv("amestest.csv")

## remove outliers
plot(amestrain$Sale_Price, amestrain$Gr_Liv_Area) # there are three strange observations: two houses that are priced very low compring to their areaa and one house that is unusually large
amestrain <- subset(amestrain, amestrain$Gr_Liv_Area<4000) # outliers removed

## bind datasets
train <- c(1:nrow(amestrain))
Ames <- rbind(amestrain, amestest)

dim(Ames[train,])
dim(Ames[-train,])

## check for missing values
summary(Ames) #no missing values outside of Sale_Price in the tesst portion of the dataset

# I run multiple randiom forests to find the mtry value that minimizes the Out-Of-Bag error.

rf.ames73=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=73,importance=TRUE) # bagging
rf.ames60=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=60,importance=TRUE)
rf.ames50=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=50,importance=TRUE)
rf.ames40=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=40,importance=TRUE)
rf.ames30=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=30,importance=TRUE)
rf.ames20=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=20,importance=TRUE)
rf.ames10=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=10,importance=TRUE)
rf.ames=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=5,importance=TRUE)

# list of OOB errors shows lowest OOB error for mtry=30
c(rf.ames73$mse[length(rf.ames$mse)],rf.ames60$mse[length(rf.ames$mse)],rf.ames50$mse[length(rf.ames$mse)],rf.ames40$mse[length(rf.ames$mse)],rf.ames30$mse[length(rf.ames$mse)],rf.ames20$mse[length(rf.ames$mse)],rf.ames10$mse[length(rf.ames$mse)],rf.ames$mse[length(rf.ames$mse)]) # Whene mtry=30 the OOB error is lowest

# I check surrounding mtry values to find the lowest OOB error. I average the OOB error from five models. 

OOB.errors = rep(0, 13)
for (i in 1:5){
rf.ames26=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=26,importance=TRUE)
rf.ames27=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=27,importance=TRUE)
rf.ames28=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=28,importance=TRUE)
rf.ames29=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=29,importance=TRUE)
rf.ames30=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=30,importance=TRUE)
rf.ames31=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=31,importance=TRUE)
rf.ames32=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=32,importance=TRUE)
rf.ames33=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=33,importance=TRUE)
rf.ames35=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=35,importance=TRUE)
rf.ames36=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=36,importance=TRUE)
rf.ames37=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=37,importance=TRUE)
rf.ames38=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=38,importance=TRUE)
rf.ames39=randomForest(Sale_Price~.,data=Ames,subset=train,mtry=39,importance=TRUE)

OOB.errors <- OOB.errors + c(rf.ames26$mse[length(rf.ames$mse)],rf.ames27$mse[length(rf.ames$mse)],rf.ames28$mse[length(rf.ames$mse)],rf.ames29$mse[length(rf.ames$mse)], rf.ames30$mse[length(rf.ames$mse)], rf.ames31$mse[length(rf.ames$mse)], rf.ames32$mse[length(rf.ames$mse)], rf.ames33$mse[length(rf.ames$mse)], rf.ames35$mse[length(rf.ames$mse)],rf.ames36$mse[length(rf.ames$mse)],rf.ames37$mse[length(rf.ames$mse)],rf.ames38$mse[length(rf.ames$mse)],rf.ames39$mse[length(rf.ames$mse)])
}

OOB.errors/5 # mtry=32 slightly outperforms other values

# plot of mtry = 32 vs bagging

plot(1:500, rf.ames73$mse, type ="l")
lines(1:500, rf.ames32$mse, col = "red")

# I try a second method of finding the best model. I use five-fold cross validation. This is not necessary and only serves to validate the model arrived at above. 
# five-fold cross validation, first method
result <- rfcv(Ames[train,-74], Ames[train,]$Sale_Price, cv.fold=5, recursive=FALSE) # lowest cross-validation error for mtry=36
result10 <- rfcv(Ames[train,-74], Ames[train,]$Sale_Price, cv.fold=10, recursive=FALSE) # lowest cross-validation error for mtry=36

# five-fold cross validation, second method
# I create five folds of data of approximately of equal sizes.
fold.index <- c(rep(1,385),rep(2, 385), rep(3,385), rep(4,386), rep(5, 386))[sample(1:1927, 1927)]

head(fold.index) #The numbers in fold.index will indicate which fold an observation is in. 
table(fold.index)

cv.error = 0 
mtry_values <- c(73, 36,35,34,33,32,31,30,29,28,27)
mtry_values <- c(36,32)
mtry_cv_errors <- rep(0, length(mtry_values))
for(n in 1:length(mtry_values)){
  cv.error = 0
  for (i in 1:5){
  fold  = which(fold.index!=i)
  rf.ames=randomForest(Sale_Price~.,data=amestrain,subset=fold,mtry=mtry_values[n],importance=TRUE)
  yhat.rf = predict(rf.ames,newdata=amestrain[-fold,])
  cv.error =  cv.error + mean((amestrain[-fold,"Sale_Price"] - yhat.rf)^2)
  }
  mtry_cv_errors[n] <- cv.error/5
}
mtry_cv_errors

# Which vaariables are important?

importance(rf.ames32)
varImpPlot(rf.ames32)

# Would the model perform better if we use only important variables? 

rf.ames32.important=randomForest(Sale_Price~Gr_Liv_Area + Neighborhood + Total_Bsmt_SF  + Garage_Cars   + First_Flr_SF + Year_Built ,data=Ames,subset=train,importance=TRUE)
rf.ames32.important$mse[length(rf.ames$mse)] # the model with fewer variables does not perform better according to the Out-Of-Bag measure. 

# Predict Salae_Price in the test set using the mtry=32 model
yhat.rf = predict(rf.ames32,newdata=Ames[-train,])
yhat.rf
Ames[-train,]$Sale_Price <- yhat.rf

# save anaswer as cvs
write.csv(Ames[-train,], file = "amesPredictions_Wilam.csv")



