#Predictive-Model

Create a recommender system to predict class label - R Proggramming , random forest classifier, oversampling

############################################################################################################

Project File : Scotia_Bnak_New.r  # new updated 
               Scotia_Bank.r      # old version including all reporting and monitoring lines

#############################################################################################################
Scotia Bank Prediction Model:
Create a recommender system to predict class label
Installing the python libraries
•	install.packages(c('lattice','ggplot2','randomForest','caret', 'ROSE’)

Call the library : 

•	library(dplyr)
•	library(lattice)
•	library(randomForest)
•	library(ggplot2)
•	library(caret)
•	library(ROSE)

Objectives:

	Data Normalization :
o	Noise reduction: To reduce the complexity of the Model 
o	Data Balancing :Use the balancing technique to produce relevant models and data results
	Create the model:
o	Use Random Forest 
	Test the model on test data:
	Future Works

Data Normalization:
Scaling: Data columns must be normalized to increase the linearity of the model or increase the accuracy of prediction. To address this issue, the MonthlyIncome will be replaced by Log (MonthlyIncome,10) and, age has replaced by (age/10)
Following Code, shows the scaling step on this project
train$MonthlyIncome=log10(train$MonthlyIncome)
train$age= (train$age)/10

Noise reduction : Hence, classification problems which contain noise are complex problems, accurate solutions are often difficult to achieve. The presence of noise in the data may affect the intrinsic characteristics of a classification problem, as these corruptions could introduce new properties in the problem domain. 
In this case I eliminated outlier recorded to the separated Database named Noise
Data Normalization :

NumberOfTimes90DaysLate

 Min.   : 0.0000        
 1st Qu.: 0.0000        
 Median : 0.0000        
 Mean   : 0.1181        
 3rd Qu.: 0.0000        
 Max.   :98.0000 
       
       
 
NumberRealEstateLoansOrLines

 Min.   : 0.000              
 1st Qu.: 0.000              
 Median : 1.000              
 Mean   : 1.072              
 3rd Qu.: 2.000              
 Max.   :32.000  
       
       
 
NumberOfTime60.89DaysPastDueNotWorse

 Min.   : 0.00000                    
 1st Qu.: 0.00000                    
 Median : 0.00000                    
 Mean   : 0.09573                    
 3rd Qu.: 0.00000                    
 Max.   :98.00000 
        
        


 Data Balancing :
Balancing the data" in data mining is similar to the concept of weighting the data in traditional statistics, but there are a number of important differences


Under sampling VS Over Sampling:


normalds.over <- ovun.sample(SeriousDlqin2yrs ~ ., data = normalds , method = "over",N = 200000)$data
prop.table(table(normalds.over$SeriousDlqin2yrs))



normalds.under <- ovun.sample(SeriousDlqin2yrs ~ ., data = normalds , method = "under",N = 40000 , seed =50)$data
prop.table(table(normalds.under$SeriousDlqin2yrs))



Test data preparation       


All steps which have been done for the Train data, needs to be done on the test data as well. 
In this step we are going to use the generated model against the unlabelled dataset or stream vectors  to predict the appropriated class label 
ts= read.csv('cs-test.csv’) # create dataset name TS as test data
tslabl=read.csv('sampleEntry.csv’) # probability dataset
Prediction : 
Nomats$SeriousDlqin2yrs <- predict(mod, ts, type="response")
prop.table(table(ts$SeriousDlqin2yrs))  



Output:
Scotia_Bank.Exl 
predictions <- as.data.frame(predict(mod, normats, type = "prob"))
normats$fail = predictions$'0'
normats$pass = predictions$'1'
normats$predict<- predict(mod, normats, type="response")
normats$observed[normats$Probability <= 0.4 ]= 0
normats$observed[normats$Probability > 0.4 ]= 1

write.csv(normats,'Scotia_Banak.csv')
write.csv(normats,'Scotia_Banak.csv')

fail	pass	predict	observed

0.88	0.12	0	        0

0.92	0.08	0	        0

1   	0   	0	        0

0.97	0.03	0	        0

0.56	0.44	0	        0

0.93	0.07	0	        0

0.83	0.17	0	        0


Pass, Fail: The probability of 1 = pass and fail = o,  predicted by the model 

Predict: the class label, predicted by the model

Observed: created from the given probability list 



 Future work:
In this project I omitted all NA values to reduce the complexity of model. 
To deal with NA we can replace the NA with:
zero , mead, average, most frequent , or predict the possible value using linear regression. In future work I will complete the model using multi linear regression to predict the missing values and compare with this results.


