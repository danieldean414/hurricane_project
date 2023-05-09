## example code for TC mortality prediction function ##

library(Hmisc)
library(data.table)

## read in the prediction function and the model output (object modobj), which includes both posterior samples and the training data used to fit the model ##
source('pred_function.R')

## to demonstrate how the function works, we can try predicting for the training data (stored in modobj$data) ##
## to see info about required variables and formats, see pred_function.R file ##
preds<-predict(modobj,newdata=modobj$data)

## the output is a matrix with units (county/TC events) in the rows and posterior samples of the predicted values in the columns ##
## to get point predictions and confidence bounds for each county/TC event, you can take mean/percentiles of the rows ##

## plot predictions by windspeed ##
plot(modobj$data$vmax_sust,rowMeans(preds))

