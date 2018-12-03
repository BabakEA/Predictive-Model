setwd('C:/Scotia_test') # set the path

install.packages('lattice')
install.packages('ggplot2')
install.packages('randomForest')
install.packages('caret')
install.packages('ROSE')
install.packages("pROC")
install.packages('ROCR')
install.packages('xlsx')

library(dplyr)
library(lattice)
library(randomForest)
library(ROCR)
library(ggplot2)
library(caret)
library(pROC)

library(ROSE)
library(xlsx)


scoti = read.csv('cs-training-Copy.csv',  header = TRUE, sep= ',',  na.strings='NA')
newscoti = scoti
newscoti$age[newscoti$age <10 ]= NA
newscoti$RevolvingUtilizationOfUnsecuredLines[newscoti$RevolvingUtilizationOfUnsecuredLines >3 ]= NA
newscoti$MonthlyIncome[newscoti$MonthlyIncome <200 ]= NA
newscoti$DebtRatio[newscoti$DebtRatio >3 ]= NA
newscoti$DebtRatio[newscoti$DebtRatio ==0 ]= NA
newscoti$MonthlyIncome=log10(newscoti$MonthlyIncome)

scoti.cleand= na.omit(newscoti)

clolis=c('age','RevolvingUtilizationOfUnsecuredLines', 'NumberOfTime30.59DaysPastDueNotWorse','DebtRatio','MonthlyIncome','NumberOfOpenCreditLinesAndLoans',
         'NumberOfTimes90DaysLate',
         'NumberRealEstateLoansOrLines',
         'NumberOfTime60.89DaysPastDueNotWorse',
         'NumberOfDependents',
         'SeriousDlqin2yrs','X')
noiseds <- scoti.cleand[which( scoti.cleand$RevolvingUtilizationOfUnsecuredLines> 2 ),
                        names(scoti.cleand) %in% clolis]

normalds= scoti.cleand[ !(scoti.cleand$X %in% noiseds$X), ]
normalds =normalds %>% select(
  age,
  RevolvingUtilizationOfUnsecuredLines,
  NumberOfTime30.59DaysPastDueNotWorse,
  DebtRatio,	
  MonthlyIncome,
  NumberOfOpenCreditLinesAndLoans,
  NumberOfTimes90DaysLate,
  NumberRealEstateLoansOrLines,
  NumberOfTime60.89DaysPastDueNotWorse,
  NumberOfDependents,
  SeriousDlqin2yrs)



table(normalds$SeriousDlqin2yrs) # oversampleing next 


normalds.over <- ovun.sample(SeriousDlqin2yrs ~ ., data = normalds , method = "over",N = 200000)$data
prop.table(table(normalds.over$SeriousDlqin2yrs)) # oversampleing progcess



normalds.over$SeriousDlqin2yrs = factor(normalds.over$SeriousDlqin2yrs) # factorization

set.seed(1000)

sub.idx = createDataPartition(normalds.over$SeriousDlqin2yrs, p=0.7 , list = FALSE)## creat data partition or test and train 


mod = randomForest(x=normalds.over[sub.idx,1:10], y=normalds.over[sub.idx,11],
                   ntree = 100,  keep.forest =TRUE) # creat random forest model name mod

pred <- predict(mod, normalds.over[-sub.idx,])

tab=table(normalds.over[-sub.idx,"SeriousDlqin2yrs"],
          pred,
          dnn=c("Actual", 'Predicted'))
t= as.vector(tab)
acc= (t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
acc
print (acc) # accuracy of prediction on test partition from the train model 

predictions <- as.data.frame(predict(mod, normalds.over[-sub.idx,], type = "prob"))

normalds.over1= normalds.over # duplicat dataset for randomforest prediction 
normalds.over2= normalds.over # duplicat dataset for logestic reggression prediction 

temppredict<- as.data.frame(predict(mod, normalds.over1, type = "prob"))
normalds.over1$fail = temppredict$'0'
normalds.over1$pass = temppredict$'1'
normalds.over1$predict<- predict(mod, normalds.over, type="response") # creat the model from the training set 

prop.table(table(normalds.over1$predict))


normalds.over2$SeriousDlqin2yrs <- factor(normalds.over$SeriousDlqin2yrs, levels = c(0,1))# prepration for logit

logit <- glm(SeriousDlqin2yrs~., data=normalds.over2 , family = binomial )# creat logestic as logit


train.probab<- predict(logit, normalds.over2,  type="response")


train.logit.pr<- ifelse(train.probab > 0.4 & train.probab < 0.88 ,1, 0) # predict the class lable with logit

trainanalysis <- roc(response=normalds.over1$predict , predictor=train.probab)# analysis teh results 
trainanalysisorigianl <- roc(response=normalds.over$SeriousDlqin2yrs, predictor=train.probab)# analysis teh results 


traine <- cbind(trainanalysis$thresholds,trainanalysis$sensitivities+trainanalysis$specificities)
opt_train <- subset(traine,traine[,2]==max(traine[,2]))[,1]


etrain <- cbind(trainanalysisorigianl$thresholds,trainanalysisorigianl$sensitivities+trainanalysisorigianl$specificities)
opt_etrain <- subset(etrain,etrain[,2]==max(etrain[,2]))[,1]



plot(1-trainanalysis$specificities,trainanalysis$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="red",lwd=2,
     main = "ROC Curve for LOGIT and predicted randomForest")
abline(a=0,b=1)
abline(v = opt_test)


plot(1-trainanalysisorigianl$specificities,trainanalysisorigianl$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="blue",lwd=2,
     main = "ROC Curve for LOGIT and train data class")
abline(a=0,b=1)
abline(v = opt_test)
opt_train
opt_etrain



ts= read.csv('cs-test.csv')
tslabl=read.csv('sampleEntry.csv')


colnames(tslabl)[1] <- "X"  #### ad class to datafram 
ts <- merge(ts,tslabl,by="X")
ts$SeriousDlqin2yrs=0
ts$age[ts$age <10 ]= NA
ts$RevolvingUtilizationOfUnsecuredLines[ts$RevolvingUtilizationOfUnsecuredLines >20 ]= NA
ts$MonthlyIncome[ts$MonthlyIncome <100 ]= NA

ts$MonthlyIncome=log10(ts$MonthlyIncome)

ts= na.omit(ts) # remove recoords including NA's



tsnoiseds <- ts[which( ts$RevolvingUtilizationOfUnsecuredLines> 3 ),
names(ts) %in% c(clolis,'Probability')]

normats= ts[ !(ts$X %in% tsnoiseds$X), ] # noise reduction 

normats =normats %>% select( age,
                             RevolvingUtilizationOfUnsecuredLines,
                             NumberOfTime30.59DaysPastDueNotWorse,
                             DebtRatio,	
                             MonthlyIncome,
                             NumberOfOpenCreditLinesAndLoans,
                             NumberOfTimes90DaysLate,
                             NumberRealEstateLoansOrLines,
                             NumberOfTime60.89DaysPastDueNotWorse,
                             NumberOfDependents,
                             SeriousDlqin2yrs,
                             Probability
)

test = normats %>% select( age,
                           RevolvingUtilizationOfUnsecuredLines,
                           NumberOfTime30.59DaysPastDueNotWorse,
                           DebtRatio,	
                           MonthlyIncome,
                           NumberOfOpenCreditLinesAndLoans,
                           NumberOfTimes90DaysLate,
                           NumberRealEstateLoansOrLines,
                           NumberOfTime60.89DaysPastDueNotWorse,
                           NumberOfDependents
)
tsnoiseds= tsnoiseds %>% select (age,
                                 RevolvingUtilizationOfUnsecuredLines,
                                 NumberOfTime30.59DaysPastDueNotWorse,
                                 DebtRatio,	
                                 MonthlyIncome,
                                 NumberOfOpenCreditLinesAndLoans,
                                 NumberOfTimes90DaysLate,
                                 NumberRealEstateLoansOrLines,
                                 NumberOfTime60.89DaysPastDueNotWorse,
                                 NumberOfDependents
)


ts = ts %>% select (age,
                    RevolvingUtilizationOfUnsecuredLines,
                    NumberOfTime30.59DaysPastDueNotWorse,
                    DebtRatio,	
                    MonthlyIncome,
                    NumberOfOpenCreditLinesAndLoans,
                    NumberOfTimes90DaysLate,
                    NumberRealEstateLoansOrLines,
                    NumberOfTime60.89DaysPastDueNotWorse,
                    NumberOfDependents)


 

normats$SeriousDlqin2yrs <- predict(mod, test, type="response") #predict : The prediction model name is Mod

train.probab<- predict(logit, test,  type="response") # predict with logit
tp<- predict(logit, test,  type="response") # predict with logit
tt<- predict(logit, normalds.over2,  type="response")


testanalysis <- roc(response=normats$SeriousDlqin2yrs , predictor=normats$Probability)# analysis teh results 

teste <- cbind(trainanalysis$thresholds,trainanalysis$sensitivities+trainanalysis$specificities)
opt_test <- subset(teste,teste[,2]==max(teste[,2]))[,1]


etrain <- cbind(trainanalysisorigianl$thresholds,trainanalysisorigianl$sensitivities+trainanalysisorigianl$specificities)
opt_etrain <- subset(etrain,etrain[,2]==max(etrain[,2]))[,1]



plot(1-testanalysis$specificities,testanalysis$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="red",lwd=2,
     main = "ROC Curve for test probab and predicted randomForest")
abline(a=0,b=1)
abline(v = opt_test)#   ROC graph  from the given probability and predicted class using Randomforest


plot(1-trainanalysisorigianl$specificities,trainanalysisorigianl$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="blue",lwd=2,
     main = "ROC Curve for LOGIT and train data class")
abline(a=0,b=1)
abline(v = opt_test) # ROC graph from the caluslated probability using Logistic regression and predicted class using RF
opt_train
opt_etrain



ggplot(data=normats, aes(train.probab)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2)  
  #labs(title=..................., x=....., y=.......)













write.csv(normats,'Scotia_Banak.csv')


