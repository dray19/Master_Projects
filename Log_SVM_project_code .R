#### Heart Disease
#### Date: 11/16/18


#####################################################################################


# This file only includes the codes for this project. 
# All details and decriptions can be seen in the Rmd or Docx file.
# A brief overview: read, summarize, clean, train & test, logistic regression, support
# vector machines, predict, compare, accuracy, log
 

### Read, View, Structure, Summary  
setwd("~/Desktop")
heart <- read.csv("heart.csv")
str(heart)
summary(heart)

# 303 obs. of 15 variables (12 numeric, 3 factors)
anyNA(heart)
# TRUE: contains NA values


### Clean
# the docx report has a description for the variables
# removed NA rows and 'X' variable
heart1 <- heart[,-c(1)]
heart1 <- na.omit(heart1)
anyNA(heart1)
# FALSE
# Convert to Factors 
heart1$Sex <- as.factor(heart1$Sex)
levels(heart1$Sex) <- c("female","male") 
heart1$ChestPain <- as.factor(heart1$ChestPain)
heart1$Fbs <- as.factor(heart1$Fbs)
levels(heart1$Fbs) <- c("false","true")
heart1$RestECG <- as.factor(heart1$RestECG)
levels(heart1$RestECG) <- c("normal","stt","hypertrophy")
heart1$ExAng <- as.factor(heart1$ExAng)
levels(heart1$ExAng) <- c("no","yes")
heart1$Slope <- as.factor(heart1$Slope)
levels(heart1$Slope) <- c("upsloping","flat","downsloping")
heart1$Ca <- as.factor(heart1$Ca)
str(heart1) 
### Correlation of continuous variables
library(corrplot)
heart2 <- heart1[, c("Age","RestBP","Chol","MaxHR","Oldpeak")]
heart2 <- as.data.frame(heart2)
g <- cor(heart2)
corrplot.mixed(g, upper = "color", lower = "number", lower.col = "black", number.cex = .7,
               tl.pos = "lt", tl.col = "black", tl.offset=1)

#############################################################################################


###Tables of variables
not_num <- heart1[, -which(names(heart1) %in% c("Age","RestBP", "Chol", "MaxHR", "Oldpeak"))]
label_val <- function(x){
  w <- table( x , AHD = heart1$AHD)
  return(w)
}
apply(not_num, 2, label_val)


#Mean, median, and SD for all columns that are not factors ###############  
mean <- sapply(heart1[,c("Age", "RestBP", "Chol", "MaxHR", "Oldpeak")], tapply, INDEX=heart1$AHD, mean)
median <- sapply(heart1[,c("Age", "RestBP", "Chol", "MaxHR", "Oldpeak")], tapply, INDEX=heart1$AHD, median)
sd <- sapply(heart1[,c("Age", "RestBP", "Chol", "MaxHR", "Oldpeak")], tapply, INDEX=heart1$AHD, sd)
stat_list <- list(MEAN=mean, MEDIAN=median, SD=sd)
stat_list


### Plots: variables
library(ggplot2)
###################
ggplot(heart1, aes(x=Age, y=MaxHR, color=AHD)) + geom_point() + geom_smooth(method = "loess") +
  ggtitle("Age vs MaxHR by AHD") + theme(plot.title = element_text(hjust = 0.5))

ggplot(heart1, aes(x=AHD, y=RestBP, color= ChestPain, size=Oldpeak)) + geom_point() + 
  ggtitle("AHD vs RestBP by ChestPain") + theme(plot.title = element_text(hjust = 0.5))



####### Box plots 
####################################################
num <- heart1[,c("Age", "RestBP", "Chol", "MaxHR", "Oldpeak", "AHD")]
for(i in names(num)[-6]){
  plot <- ggplot(num, aes_string("AHD", y = i)) + geom_boxplot() + ggtitle(paste0(i, " vs. AHD")) + 
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
}


######## Bar Chart                 
not_num <- heart1[ , -which(names(heart1) %in% c("Age", "RestBP", "Chol", "MaxHR", "Oldpeak"))]
for(i in names(not_num)[-8]){
  plot <- ggplot(not_num, aes_string("AHD", fill = i)) + geom_bar() + ggtitle(paste0(i, " vs. AHD")) + 
    theme(plot.title = element_text(hjust = 0.5)) 
  print(plot)
} 


### Train & Test Data Sets
###### Make sure you set.seed(123), if not we are all going to get different answers
set.seed(123)
s <- sample(1:nrow(heart1), nrow(heart1)*0.8)
train <- heart1[s,]
test <- heart1[-s,]
### Accuracy Function for models  #########################################
acc <- function(x){
  w <- sum(diag(x))/ sum(x)*100
  return(w)
}
### Individual logistic regression ###################################
library(tidyverse)
#library(broom)
library(pROC)
trainAge <- glm(AHD ~ Age, data=train, family='binomial') 
summary(trainAge)
pred_age <- predict(trainAge, train, type = "response")
roccurve_age <- roc(train$AHD ~ pred_age)
pred2_age <- ifelse(pred_age > 0.5, 1,0)
tab_age <- table(Pred = pred2_age, Actual = train$AHD)
tab_age
##############
trainSex <- glm(AHD ~ Sex, data=train, family='binomial') 
summary(trainSex)
pred_sex <- predict(trainSex, train, type = "response")
roccurve_sex <- roc(train$AHD ~ pred_sex)
pred2_sex <- ifelse(pred_sex > 0.5, 1,0)
tab_sex <- table(Pred = pred2_sex, Actual = train$AHD)
tab_sex 
##############
trainChestPain <- glm(AHD ~ ChestPain, data=train, family='binomial') 
summary(trainChestPain)
pred_cp <- predict(trainChestPain, train, type = "response")
roccurve_cp <- roc(train$AHD ~ pred_cp)
pred2_cp <- ifelse(pred_cp > 0.5, 1,0)
tab_cp <- table(Pred = pred2_cp, Actual = train$AHD)
tab_cp
##################
trainRestBP <- glm(AHD ~ RestBP, data=train, family='binomial') 
summary(trainRestBP)
pred_rbp <- predict(trainRestBP, train, type = "response")
roccurve_rbp <- roc(train$AHD ~ pred_rbp)
pred2_rbp <- ifelse(pred_rbp > 0.5, 1,0)
tab_rbp <- table(Pred = pred2_rbp, Actual = train$AHD)
tab_rbp
##################
trainChol <- glm(AHD ~ Chol, data=train, family='binomial') 
summary(trainChol)
pred_chol <- predict(trainChol, train, type = "response")
roccurve_chol <- roc(train$AHD ~ pred_chol)
pred2_chol <- ifelse(pred_chol > 0.5, 1,0)
tab_chol <- table(Pred = pred2_chol, Actual = train$AHD)
tab_chol 
##################
trainFbs <- glm(AHD ~ Fbs, data=train, family='binomial') 
summary(trainFbs)
pred_fbs <- predict(trainFbs, train, type = "response")
roccurve_fbs <- roc(train$AHD ~ pred_fbs)
pred2_fbs <- ifelse(pred_fbs > 0.5, 1,0)
tab_fbs <- table(Pred = pred2_fbs, Actual = train$AHD)
tab_fbs 
##################
trainRestECG <- glm(AHD ~ RestECG, data=train, family='binomial') 
summary(trainRestECG)
pred_ecg <- predict(trainRestECG, train, type = "response")
roccurve_ecg <- roc(train$AHD ~ pred_ecg)
pred2_ecg <- ifelse(pred_ecg > 0.5, 1,0)
tab_ecg <- table(Pred = pred2_ecg, Actual = train$AHD)
tab_ecg 
##################
trainMaxHR <- glm(AHD ~ MaxHR, data=train, family='binomial') 
summary(trainMaxHR)
pred_hr <- predict(trainMaxHR, train, type = "response")
roccurve_hr <- roc(train$AHD ~ pred_hr)
pred2_hr <- ifelse(pred_hr > 0.5, 1,0)
tab_hr <- table(Pred = pred2_hr, Actual = train$AHD)
tab_hr 
##################
trainExAng <- glm(AHD ~ ExAng, data=train, family='binomial') 
summary(trainExAng)
pred_ang <- predict(trainExAng, train, type = "response")
roccurve_ang <- roc(train$AHD ~ pred_ang)
pred2_ang <- ifelse(pred_ang > 0.5, 1,0)
tab_ang <- table(Pred = pred2_ang, Actual = train$AHD)
tab_ang 
##################
trainOldpeak <- glm(AHD ~ Oldpeak, data=train, family='binomial') 
summary(trainOldpeak)
pred_op <- predict(trainOldpeak, train, type = "response")
roccurve_op <- roc(train$AHD ~ pred_op)
pred2_op <- ifelse(pred_op > 0.5, 1,0)
tab_op <- table(Pred = pred2_op, Actual = train$AHD)
tab_op 
##################
trainSlope <- glm(AHD ~ Slope, data=train, family='binomial') 
summary(trainSlope)
pred_sp <- predict(trainSlope, train, type = "response")
roccurve_sp <- roc(train$AHD ~ pred_sp)
pred2_sp <- ifelse(pred_sp > 0.5, 1,0)
tab_sp <- table(Pred = pred2_sp, Actual = train$AHD)
tab_sp 
##################
trainCa <- glm(AHD ~ Ca, data=train, family='binomial') 
summary(trainCa)
pred_ca <- predict(trainCa, train, type = "response")
roccurve_ca <- roc(train$AHD ~ pred_ca)
pred2_ca <- ifelse(pred_ca > 0.5, 1,0)
tab_ca <- table(Pred = pred2_ca, Actual = train$AHD)
tab_ca 
################
trainThal <- glm(AHD ~ Thal, data=train, family='binomial') 
summary(trainThal)
pred_t <- predict(trainThal, train, type = "response")
roccurve_t <- roc(train$AHD ~ pred_t)
pred2_t <- ifelse(pred_t > 0.5, 1,0)
tab_t <- table(Pred = pred2_t, Actual = train$AHD)
tab_t 
########################
### Models 
model <- glm(AHD ~., data = train, family = "binomial")
summary(model)
pred <- predict(model, train, type = "response")
roccurve <- roc(train$AHD ~ pred)
pred1 <- ifelse(pred > 0.5, 1,0)
tab1 <- table(Pred = pred1, Actual = train$AHD)
## Used stepwise regression to get our best models 
# Model with the lowest AIC 
step(model, scope = list(upper = model), direction = "backward")
################################################ 
## Model 2
model2 <- glm(AHD ~ Age + Sex + ChestPain + RestBP + Chol + Fbs + MaxHR + ExAng + 
                Oldpeak + Slope + Ca + Thal , data = train, family = "binomial")
summary(model2)  
pred2 <- predict(model2, train, type = "response")
roccurve2 <- roc(train$AHD ~ pred2)
pred2 <- ifelse(pred2 > 0.5, 1,0)
tab2 <- table(Pred = pred2, Actual = train$AHD)
############################### 
# Model 3
model3 <- glm(AHD ~ Sex + ChestPain + RestBP + Chol + Fbs + MaxHR + ExAng + 
                Oldpeak + Slope + Ca + Thal,data =  train, family = "binomial")
summary(model3)
pred3 <- predict(model3, train, type = "response")
roccurve3 <- roc(train$AHD ~ pred3)
pred3 <- ifelse(pred3 > 0.5, 1,0)
tab3 <- table(Pred = pred3, Actual = train$AHD)
###########################
# Model 4
model4 <- glm(AHD ~ Sex + ChestPain + RestBP + Fbs + MaxHR + ExAng + Oldpeak + 
                Slope + Ca + Thal, data = train, family = "binomial")
summary(model4)
pred4 <- predict(model4, train, type = "response")
roccurve4 <- roc(train$AHD ~ pred4)
pred4 <- ifelse(pred4 > 0.5, 1,0)
tab4 <- table(Pred = pred4, Actual = train$AHD)
######################
#Model 5
model5 <- glm(AHD ~ Sex + ChestPain + RestBP + MaxHR + ExAng + Oldpeak + Slope + 
                Ca + Thal  , data = train, family = "binomial")
summary(model5)
pred5 <- predict(model5, train, type = "response")
roccurve5 <- roc(train$AHD ~ pred5)
pred5 <- ifelse(pred5 > 0.5, 1,0)
tab5 <- table(Pred = pred5, Actual = train$AHD)
####################
# Model 6
model6 <- glm(AHD ~ Sex + ChestPain + RestBP + ExAng + Oldpeak + Slope + Ca + 
                Thal  , data = train, family = "binomial")
summary(model6)
pred6 <- predict(model6, train, type = "response")
roccurve6 <- roc(train$AHD ~ pred6)
pred6 <- ifelse(pred6 > 0.5, 1,0)
tab6 <- table(Pred = pred6, Actual = train$AHD)
#############################
# Model 7
model7 <- glm(AHD ~  ChestPain + Slope + Ca + Thal + RestBP , data = train, family = "binomial")
summary(model7)
pred7 <- predict(model7, train, type = "response")
roccurve7 <- roc(train$AHD ~ pred7)
pred7 <- ifelse(pred7 > 0.5, 1,0)
tab7 <- table(Pred = pred7, Actual = train$AHD)
################################################################################# 
##### Table of all the info that Professor wanted 
info_list <- list(TrainAge = list(Accuracy = acc(tab_age), Null_Deviance = trainAge$null.deviance, Residual_Deviance = trainAge$deviance, AIC = trainAge$aic, AUC = auc(roccurve_age)),
                  TrainSex = list(Accuracy = acc(tab_sex), Null_Deviance = trainSex$null.deviance, Residual_Deviance = trainSex$deviance, AIC = trainSex$aic, AUC = auc(roccurve_sex)),
                  TrainChestPain = list(Accuracy = acc(tab_cp), Null_Deviance = trainChestPain$null.deviance, Residual_Deviance = trainChestPain$deviance, AIC = trainChestPain$aic, AUC = auc(roccurve_cp)),
                  TrainRestBP = list(Accuracy = acc(tab_rbp), Null_Deviance = trainRestBP$null.deviance, Residual_Deviance = trainRestBP$deviance, AIC = trainRestBP$aic, AUC = auc(roccurve_rbp)),
                  TrainChol = list(Accuracy = acc(tab_chol), Null_Deviance = trainChol$null.deviance, Residual_Deviance = trainChol$deviance, AIC = trainChol$aic, AUC = auc(roccurve_chol)),
                  TrainFbs = list(Accuracy = acc(tab_fbs), Null_Deviance = trainFbs$null.deviance, Residual_Deviance = trainFbs$deviance, AIC = trainFbs$aic, AUC = auc(roccurve_fbs)),
                  TrainRestECG = list(Accuracy = acc(tab_ecg), Null_Deviance = trainRestECG$null.deviance, Residual_Deviance = trainRestECG$deviance, AIC = trainRestECG$aic, AUC = auc(roccurve_ecg)),
                  TrainMaxHR = list(Accuracy = acc(tab_hr), Null_Deviance = trainMaxHR$null.deviance, Residual_Deviance = trainMaxHR$deviance, AIC = trainMaxHR$aic, AUC = auc(roccurve_hr)),
                  TrainExAng = list(Accuracy = acc(tab_ang), Null_Deviance = trainExAng$null.deviance, Residual_Deviance = trainExAng$deviance, AIC = trainExAng$aic, AUC = auc(roccurve_ang)),
                  TrainOldpeak = list(Accuracy = acc(tab_op), Null_Deviance = trainOldpeak$null.deviance, Residual_Deviance = trainOldpeak$deviance, AIC = trainOldpeak$aic, AUC = auc(roccurve_op)),
                  TrainSlope = list(Accuracy = acc(tab_sp), Null_Deviance = trainSlope$null.deviance, Residual_Deviance = trainSlope$deviance, AIC = trainSlope$aic, AUC = auc(roccurve_sp)),
                  TrainCa = list(Accuracy = acc(tab_ca), Null_Deviance = trainCa$null.deviance, Residual_Deviance = trainCa$deviance, AIC = trainCa$aic, AUC = auc(roccurve_ca)),
                  TrainThal = list(Accuracy = acc(tab_t), Null_Deviance = trainThal$null.deviance, Residual_Deviance = trainThal$deviance, AIC = trainThal$aic, AUC = auc(roccurve_t)),
                  Model1 = list(Accuracy = acc(tab1), Null_Deviance = model$null.deviance, Residual_Deviance = model$deviance, AIC = model$aic, AUC = auc(roccurve)),
                  Model2 = list(Accuracy = acc(tab2), Null_Deviance = model2$null.deviance, Residual_Deviance = model2$deviance, AIC = model2$aic, AUC = auc(roccurve2)),
                  Model3 = list(Accuracy = acc(tab3), Null_Deviance = model3$null.deviance, Residual_Deviance = model3$deviance, AIC = model3$aic, AUC = auc(roccurve3)),
                  Model4 = list(Accuracy = acc(tab4), Null_Deviance = model4$null.deviance, Residual_Deviance = model4$deviance, AIC = model4$aic, AUC = auc(roccurve4)),
                  Model5 = list(Accuracy = acc(tab5), Null_Deviance = model5$null.deviance, Residual_Deviance = model5$deviance, AIC = model5$aic, AUC = auc(roccurve5)),
                  Model6 = list(Accuracy = acc(tab6), Null_Deviance = model6$null.deviance, Residual_Deviance = model6$deviance, AIC = model6$aic, AUC = auc(roccurve6)),
                  Model7 = list(Accuracy = acc(tab7), Null_Deviance = model7$null.deviance, Residual_Deviance = model7$deviance, AIC = model7$aic, AUC = auc(roccurve7)))

info1 <- do.call(rbind, info_list)
info2  <- as.data.frame(info1)
info <- rownames_to_column(info2, "Models")
info$Accuracy <-  round(as.numeric(info$Accuracy),2)
info$Null_Deviance <-  round(as.numeric(info$Null_Deviance),2)
info$Residual_Deviance <-  round(as.numeric(info$Residual_Deviance),2)
info$AUC <-  round(as.numeric(info$AUC),3)
info$AIC <-  round(as.numeric(info$AIC),2)
info
##################################
## All ROC plotted on one graph 
 ### closer to the topleft the better the model
# library(ggplot2) for ggroc()
ggroc(list(model = roccurve, model_2 = roccurve2, model_3 = roccurve3,model_4 = roccurve4, model_5 = roccurve5, model_6 = roccurve6,
           model_7 = roccurve7)) + ggtitle("ROC Curve") + theme(plot.title = element_text(hjust = 0.5))
#### tables of models. 
# Looking for models with smallest false negative rate, that means predicited no but Actual is yes 
tables <- list(model1 = tab1,
               model2 = tab2,
               model3 = tab3,
               model4 = tab4,
               model5 = tab5,
               model6 = tab6, 
               model7 = tab7)
tables
################ Best Models 
##### Using Test Data
## Model 1
pred_test1 <- predict(model, test, type = 'response')
prediction_1 <- ifelse(pred_test1 > 0.5, 1, 0)
tab_1 <- table(Predicted = prediction_1, Actual = test$AHD)
roccurve_1 <- roc(test$AHD ~ pred_test1)
### Model 4
pred_test4 <- predict(model4, test, type = 'response')
prediction_4 <- ifelse(pred_test4 > 0.5, 1, 0)
tab_4 <- table(Predicted = prediction_4, Actual = test$AHD)
roccurve_4 <- roc(test$AHD ~ pred_test4)
#### Model 6
pred_test6 <- predict(model6, test, type = 'response')
prediction_6 <- ifelse(pred_test6 > 0.5, 1, 0)
tab_6 <- table(Predicted = prediction_6, Actual = test$AHD)
roccurve_6 <- roc(test$AHD ~ pred_test6)
##########
# ROC curve for Best Models 
ggroc(list(model_1 = roccurve_1, model_4 = roccurve_4, model_6 =  roccurve_6))
### AUC for best models 
test_info_list <- list(Model1 = list(Accuracy = acc(tab_1), AUC = auc(roccurve_1)),
                  Model4 = list(Accuracy = acc(tab_4), AUC = auc(roccurve_4)),
                  Model6 = list(Accuracy = acc(tab_6), AUC = auc(roccurve_6)))
test_info1 <- do.call(rbind, test_info_list)
test_info2 <- as.data.frame(test_info1)
test_info <- rownames_to_column(test_info2, "Models")
test_info

tables_test <- list(model1 = tab_1,
                    model4 = tab_4,
                    model6 = tab_6)
 
tables_test

### Support Vector Machines ####################################################################
#install.packages('e1071')
library(e1071)
svm1_lin <-  svm(AHD ~ ., data = train, kernel = "linear")
summary(svm1_lin)
pred1_lin <- predict(svm1_lin, train, type = "class")
tab_lin <- table(Predicited = pred1_lin, Actual = train$AHD)
tab_lin
plot(pred1_lin, main = "SVM 1 Linear")
######### Sigmoid ##########
svm1_sig <-  svm(AHD ~ ., data = train, kernel = "sigmoid")
summary(svm1_sig)
pred1_sig <- predict(svm1_sig, train, type = "class")
tab_sig <- table(Predicited = pred1_sig, Actual = train$AHD)
tab_sig
plot(pred1_sig, main = "SVM 1 Sigmoid")
########## Poly #########
svm1_poly <-  svm(AHD ~ ., data = train, kernel = "polynomial")
summary(svm1_poly)
pred1_poly <- predict(svm1_poly, train, type = "class")
tab_poly <- table(Predicited = pred1_poly, Actual = train$AHD)
tab_poly
acc(tab_poly)
plot(pred1_poly, main = "SVM 1 Polynomial")
########## Svm 2 ############
svm2 <- svm(AHD ~ Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca, data = train, kernel = "linear")
summary(svm2)
pred2 <- predict(svm2, train, type = "class")
plot(pred2, main = "SVM 2 Linear")
tab2 <- table(Predicited = pred2, Actual = train$AHD)
tab2
########### Sigmoid #####
svm2_sig <- svm(AHD ~ Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca, data = train, kernel = "sigmoid")
summary(svm2_sig)
pred2_sig <- predict(svm2_sig, train, type = "class")
plot(pred2_sig, main = "SVM 2 Sigmoid")
tab2_sig <- table(Predicited = pred2_sig, Actual = train$AHD)
tab2_sig
################ Svm 3 ################
svm3 <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal, data = train, kernel = "linear")
summary(svm3)
pred3 <- predict(svm3, train, type = "class")
plot(pred3, main = "SVM 3 Linear")
tab3 <- table(Predicited = pred3, Actual = train$AHD)
tab3
######## sigmoid ############
svm3_sig <- svm(AHD ~  Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal, data = train, kernel = "sigmoid")
summary(svm3_sig)
pred3_sig <- predict(svm3_sig, train, type = "class")
plot(pred3_sig, main = "SVM 3 Sigmoid")
tab3_sig <- table(Predicited = pred3_sig, Actual = train$AHD)
tab3_sig
############## SVM 4 ################
svm4 <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal + Chol, data = train, kernel = "linear")
summary(svm4)
pred4 <- predict(svm4, train, type = "class")
plot(pred4, main = "SVM 4 Linear")
tab4 <- table(Predicited = pred4, Actual = train$AHD)
tab4
######### Sigmoid #####
svm4_sig <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal + Chol, data = train, kernel = "sigmoid")
summary(svm4_sig)
pred4_sig <- predict(svm4_sig, train, type = "class")
plot(pred4_sig, main = "SVM 4 Sigmoid")
tab4_sig <- table(Predicited = pred4_sig, Actual = train$AHD)
tab4_sig
##################### SVM 5 ##############
svm5 <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal + ExAng, data = train, kernel = "linear")
summary(svm5)
pred5 <- predict(svm5, train, type = "class")
plot(pred5, main = "SVM 5 Linear")
tab5 <- table(Predicited = pred5, Actual = train$AHD)
tab5
######## Sigmoid #############
svm5_sig <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal + ExAng, data = train, kernel = "sigmoid")
summary(svm5_sig)
pred5_sig <- predict(svm5_sig, train, type = "class")
plot(pred5_sig, main = "SVM 5 Sigmoid")
tab5_sig <- table(Predicited = pred5_sig, Actual = train$AHD)
tab5_sig
##################### SVM 6 ##################
svm6 <- svm(AHD ~ Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal + ExAng + Chol, data = train, kernel = "linear")
summary(svm6)
pred6 <- predict(svm6, train, type = "class")
plot(pred6, main = "SVM 6 Linear")
tab6 <- table(Predicited = pred6, Actual = train$AHD)
tab6
######## Sigmoid #############
svm6_sig <- svm(AHD ~ Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal + ExAng + Chol, data = train, kernel = "sigmoid")
summary(svm6_sig)
pred6_sig <- predict(svm6_sig, train, type = "class")
plot(pred6_sig, main = "SVM 6 Sigmoid")
tab6_sig <- table(Predicited = pred6_sig, Actual = train$AHD)
tab6_sig
##################### SVM 7
svm7 <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal +  RestECG, data = train, kernel = "linear")
summary(svm7)
pred7 <- predict(svm7, train, type = "class")
plot(pred7, main = "SVM 7 Linear")
tab7 <- table(Predicited = pred7, Actual = train$AHD)
tab7
########## Sigmoid ####
svm7_sig <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal +  RestECG, data = train, kernel = "sigmoid")
summary(svm7_sig)
pred7_sig <- predict(svm7_sig, train, type = "class")
plot(pred7_sig, main = "SVM 7 Sigmoid")
tab7_sig <- table(Predicited = pred7_sig, Actual = train$AHD)
tab7_sig
###################### SVM 8 ######
svm8 <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal +  Fbs, data = train, kernel = "linear")
summary(svm8)
pred8 <- predict(svm8, train, type = "class")
plot(pred8,main = "SVM 8 Linear")
tab8 <- table(Predicited = pred8, Actual = train$AHD)
tab8
####### Sigmoid #######
svm8_sig <- svm(AHD ~ Age + Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal +  Fbs, data = train, kernel = "sigmoid")
summary(svm8_sig)
pred8_sig <- predict(svm8_sig, train, type = "class")
plot(pred8_sig,main = "SVM 8 Sigmoid")
tab8_sig <- table(Predicited = pred8_sig, Actual = train$AHD)
tab8_sig
###################### SVM 9
svm9 <- svm(AHD ~ Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal +  Fbs + RestECG + Chol + ExAng, data = train, kernel = "linear")
summary(svm9)
pred9 <- predict(svm9, train, type = "class")
plot(pred9, main = "SVM 9 Linear")
tab9 <- table(Predicited = pred9, Actual = train$AHD)
tab9
############## Sigmoid #######
svm9_sig <- svm(AHD ~ Sex + ChestPain +  RestBP + MaxHR + Oldpeak + Slope + Ca + Thal +  Fbs + RestECG + Chol + ExAng, data = train, kernel = "sigmoid")
summary(svm9_sig)
pred9_sig <- predict(svm9_sig, train, type = "class")
plot(pred9_sig, main = "SVM 9 Sigmoid")
tab9_sig <- table(Predicited = pred9_sig, Actual = train$AHD)
tab9_sig
###################
##### Number of support vectors, table for each model, Accuracy for each model
##For train 
svm_list <- list(svm1 = list(svm1_lin$tot.nSV,acc(tab_lin)),
     svm1_sig = list(svm1_sig$tot.nSV,acc(tab_sig)),
     svm2 = list(svm2$tot.nSV,acc(tab2)),
     svm2_sig = list(svm2_sig$tot.nSV,acc(tab2_sig)),
     svm3 = list(svm3$tot.nSV,acc(tab3)),
     svm3_sig = list(svm3_sig$tot.nSV,acc(tab3_sig)),
     svm4 = list(svm4$tot.nSV ,acc(tab4)),
     svm4_sig = list(svm4_sig$tot.nSV ,acc(tab4_sig)),
     svm5 = list(svm5$tot.nSV ,acc(tab5)),
     svm5_sig = list(svm5_sig$tot.nSV ,acc(tab5_sig)),
     svm6 = list(svm6$tot.nSV ,acc(tab6)),
     svm6_sig = list(svm6_sig$tot.nSV ,acc(tab6_sig)),
     svm7 = list(svm7$tot.nSV ,acc(tab7)),
     svm7_sig = list(svm7_sig$tot.nSV ,acc(tab7_sig)),
     svm8 = list(svm8$tot.nSV ,acc(tab8)),
     svm8_sig = list(svm8_sig$tot.nSV ,acc(tab8_sig)),
     svm9 = list(svm9$tot.nSV ,acc(tab9)),
    svm9_sig = list(svm9_sig$tot.nSV ,acc(tab9_sig)))

svm_info1 <- do.call(rbind, svm_list)
svm_info2  <- as.data.frame(svm_info1 )
svm_info <- rownames_to_column(svm_info2, "Models")
names(svm_info)[2] <- "Total Support Vectors"
names(svm_info)[3] <- "Accuracy"
svm_info$Accuracy <- round(as.numeric(svm_info$Accuracy), 2)
svm_info
#####
svm_tables <- list(svm1 = tab_lin,
                   svm2 = tab2,
                   svm3 = tab3,
                   svm4 = tab4,
                   svm5 = tab5,
                   svm6 = tab6,
                   svm7 = tab7,
                   svm8 = tab8,
                   svm9 = tab9)

svm_tables
########### Best Models 
### svm 4
pred_4 <- predict(svm4, test, type = "class")
plot(pred_4,main = "SVM 4 (Test Data)")
tab_4 <- table(Predicited = pred_4, Actual = test$AHD)
tab_4
acc(tab_4)
#### svm 7
pred_7 <- predict(svm7, test, type = "class")
plot(pred_7, main = "SVM 7 (Test Data)")
tab_7 <- table(Predicited = pred_7, Actual = test$AHD)
tab_7
acc(tab_7)
### svm 9 
pred_9 <- predict(svm9, test, type = "class")
plot(pred_9, main = "SVM 9 (Test Data)")
tab_9 <- table(Predicited = pred_9, Actual = test$AHD)
tab_9
acc(tab_9)
##############
##### Number of support vectors, table for each model, Accuracy for each model
### For test
list(svm4 = list(Total_Support_Vectors = svm4$tot.nSV, Table = tab_4 ,Accuracy = acc(tab_4)),
     svm7 = list(Total_Support_Vectors = svm7$tot.nSV,Table = tab_7 ,Accuracy = acc(tab_7)),
     svm9 = list(Total_Support_Vectors = svm9$tot.nSV,Table = tab_9 ,Accuracy = acc(tab_9)))

