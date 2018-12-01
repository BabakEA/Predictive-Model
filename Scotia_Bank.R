setwd('C:/Scotia_test') # set the path
install.packages(c('lattice','ggplot2','randomForest','caret', 'ROSE', "pROC", 'ROCR'))

library(dplyr)
library(lattice)
library(randomForest)
library(ggplot2)
library(caret)
library(pROC)
library(ROCR)
scoti = read.csv('cs-training-Copy.csv',  header = TRUE, sep= ',',  na.strings='NA')

newscoti = scoti

newscoti$age[newscoti$age <10 ]= NA
newscoti$RevolvingUtilizationOfUnsecuredLines[newscoti$RevolvingUtilizationOfUnsecuredLines >3 ]= NA
newscoti$MonthlyIncome[newscoti$MonthlyIncome <200 ]= NA
newscoti$DebtRatio[newscoti$DebtRatio >3 ]= NA
newscoti$DebtRatio[newscoti$DebtRatio ==0 ]= NA


newscoti$MonthlyIncome=log10(newscoti$MonthlyIncome)







scoti.cleand= na.omit(newscoti)


summary(scoti.cleand)

clolis=c('age','RevolvingUtilizationOfUnsecuredLines', 'NumberOfTime30.59DaysPastDueNotWorse','DebtRatio','MonthlyIncome','NumberOfOpenCreditLinesAndLoans',
         'NumberOfTimes90DaysLate',
         'NumberRealEstateLoansOrLines',
         'NumberOfTime60.89DaysPastDueNotWorse',
         'NumberOfDependents',
         'SeriousDlqin2yrs','X'
)


for (i in clolis){
  
  print(summary(scoti.cleand[i]))   
  
}



noiseds <- scoti.cleand[which( scoti.cleand$RevolvingUtilizationOfUnsecuredLines> 2 ),
names(scoti.cleand) %in% clolis]





for (i in clolis){
  b = i
  print(summary(noiseds[b])  ) 
  
}






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




table(scoti.cleand$SeriousDlqin2yrs)
table(noiseds$SeriousDlqin2yrs)
prop.table(table(normalds$SeriousDlqin2yrs))


ggplot(normalds, aes(x = age, y = MonthlyIncome, fill = SeriousDlqin2yrs, label = 'Frequency')) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))







for (i in clolis){
  print(summary(normalds[i]))   
  
}

#creat oversamplening from ROSE package

library(ROSE)


#test for Population 
table(normalds$SeriousDlqin2yrs)
#     0     1 
#0          1 
#0.93072405 0.06927595 

#creat over sample dataset
normalds.over <- ovun.sample(SeriousDlqin2yrs ~ ., data = normalds , method = "over",N = 200000)$data
prop.table(table(normalds.over$SeriousDlqin2yrs))

#creat under sample dataset
normalds.under <- ovun.sample(SeriousDlqin2yrs ~ ., data = normalds , method = "under",N = 40000 , seed =50)$data
prop.table(table(normalds.under$SeriousDlqin2yrs))

#0      1 
#0.6312 0.3688 




#################### Undersampeling 


normalds.under$SeriousDlqin2yrs = factor(normalds.under$SeriousDlqin2yrs)


set.seed(1000)

## creat data partition or test and train 
sub.idx = createDataPartition(normalds.under$SeriousDlqin2yrs, p=0.7 , list = FALSE)

### build the random forest model ":
mod = randomForest(x=normalds.under[sub.idx,1:10], y=normalds.under[sub.idx,11],
                   ntree = 100,  keep.forest =TRUE
)

pred <- predict(mod, normalds.under[-sub.idx,])

tab=table(normalds.under[-sub.idx,"SeriousDlqin2yrs"],
          pred,
          dnn=c("Actual", 'Predicted'))
t= as.vector(tab)
acc= (t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
acc




predictions <- as.data.frame(predict(mod, normalds.under[-sub.idx,], type = "prob"))
normalds.under1= normalds.under
temppredict<- as.data.frame(predict(mod, normalds.under1, type = "prob"))
normalds.under1$fail = temppredict$'0'
normalds.under1$pass = temppredict$'1'
normalds.under1$predict<- predict(mod, normalds.under, type="response")

##### creat graph




#calculate the probability of model 
#probs = predict(mod,normalds[-sub.idx,],type = 'prob')

library(ROCR)

predfaile = prediction(normalds.under1$fail, normalds.under1$SeriousDlqin2yrs)
predpass = prediction(normalds.under1$pass, normalds.under1$SeriousDlqin2yrs)
# creat performance object from ROC function 

perfpass = performance(predpass,'tpr','acc' )
peffail=performance(predfaile,'tpr','acc')
#perfpass1 = performance(predpass,'tpr','acc' )

# plot the performance

plot (perfpass ,type="b", pch=19, col="red")
par(new=TRUE)
plot (peffail , col='blue')
par(new=TRUE)
#plot (perfpass1, col='blue')
legend(1, 95, legend=c("Line 1", "Line 0"),
       col=c("green", "blue"), lty=1:2, cex=0.8)

###############################  end of undersampling ############3

############################# Over Sampleing #####################

normalds.over$SeriousDlqin2yrs = factor(normalds.over$SeriousDlqin2yrs)

set.seed(1000)

## creat data partition or test and train 
sub.idx = createDataPartition(normalds.over$SeriousDlqin2yrs, p=0.7 , list = FALSE)

### build the random forest model ":
mod = randomForest(x=normalds.over[sub.idx,1:10], y=normalds.over[sub.idx,11],
                   ntree = 100,  keep.forest =TRUE)

pred <- predict(mod, normalds.over[-sub.idx,])

tab=table(normalds.over[-sub.idx,"SeriousDlqin2yrs"],
          pred,
          dnn=c("Actual", 'Predicted'))
t= as.vector(tab)
acc= (t[1]+t[4])/(t[1]+t[2]+t[3]+t[4])
acc

predictions <- as.data.frame(predict(mod, normalds.over[-sub.idx,], type = "prob"))
normalds.over1= normalds.over
temppredict<- as.data.frame(predict(mod, normalds.over1, type = "prob"))
normalds.over1$fail = temppredict$'0'
normalds.over1$pass = temppredict$'1'
normalds.over1$predict<- predict(mod, normalds.over, type="response")

##### creat graph

predfaile = prediction(normalds.over1$fail, normalds.over1$SeriousDlqin2yrs)
predpass = prediction(normalds.over1$pass, normalds.over1$SeriousDlqin2yrs)
# creat performance object from ROC function 

perfpass = performance(predpass,'tpr','acc' )
peffail=performance(predfaile,'tpr','acc')
#perfpass1 = performance(predpass,'tpr','acc' )

# plot the performance
plot (perfpass ,type="b", pch=19, col="red")
par(new=TRUE)
plot (peffail , col='blue')

#plot (perfpass1, col='blue')
legend(1, 95, legend=c("Line 1", "Line 0"),
       col=c("green", "blue"), lty=1:2, cex=0.8)





############################## End of over sampleing #########################3


plot (Agetab , col="red")

plot (table(normalds.over1$SeriousDlqin2yrs)  , col="green" )

plot (table(normalds.under1$SeriousDlqin2yrs))



ts= read.csv('cs-test.csv')
tslabl=read.csv('sampleEntry.csv')

#### ad class to datafram 
colnames(tslabl)[1] <- "X"
ts <- merge(ts,tslabl,by="X")

ts$SeriousDlqin2yrs=0


ts$age[ts$age <10 ]= NA
#ts$RevolvingUtilizationOfUnsecuredLines[ts$RevolvingUtilizationOfUnsecuredLines >20 ]= NA
ts$MonthlyIncome[ts$MonthlyIncome <100 ]= NA
#ts$DebtRatio[ts$DebtRatio >2 ]= NA

ts$MonthlyIncome=log10(ts$MonthlyIncome)




ts= na.omit(ts)



tsnoiseds <- ts[which( ts$RevolvingUtilizationOfUnsecuredLines> 2
),
names(ts) %in% c(clolis,'Probability')]

for (i in clolis){
  i
  print(summary(tsnoiseds[i])  ) 
  
}

normats= ts[ !(ts$X %in% tsnoiseds$X), ]



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


#predict : The prediction model name is Mod 

normats$SeriousDlqin2yrs <- predict(mod, test, type="response")

tsnoiseds$SeriousDlqin2yrs <- predict(mod, tsnoiseds, type="response")

ts$SeriousDlqin2yrs<- predict(mod, ts, type="response")

prop.table(table(ts$SeriousDlqin2yrs))


predictions <- as.data.frame(predict(mod, ts, type = "prob"))

testtabel=prop.table(table(normats$SeriousDlqin2yrs))
traintabel=prop.table(table(normalds$SeriousDlqin2yrs))

resulttab= bind_rows(testtabel,traintabel)






ts$fail = predictions$'0'
ts$pass = predictions$'1'
ts$predict<- predict(mod, ts, type="response")


predictions <- as.data.frame(predict(mod, normats, type = "prob"))
normats$fail = predictions$'0'
normats$pass = predictions$'1'
normats$predict<- predict(mod, normats, type="response")
normats$observed[normats$Probability <= 0.4 ]= 0
normats$observed[normats$Probability > 0.4 ]= 1

write.csv(normats,'Scotia_Banak.csv')










