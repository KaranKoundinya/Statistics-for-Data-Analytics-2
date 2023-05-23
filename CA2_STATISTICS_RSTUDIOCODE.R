#CA2

#Name : Karan Koundinya Janakiram
#Student Number : 10602768
#Course: MSc in Data Analytics
#Module: Statistics for Data Analytics
#Module Code: B9DA101
#Group : B
#Question 1

install.packages('gmodels')
library(gmodels)
Real.estate.valuation.data.set<-read.csv(file.choose(),header=TRUE)
#Uploading the dataset on the RStudio ecosystem
View(Real.estate.valuation.data.set)
#Viewing the dataset on the Rstudio setup
dataset <- Real.estate.valuation.data.set
#Equating the real estate to the word dataset for the further use in the code
set.seed(32)
#set seed is used  to specify the initial value of the random number seed, over here 32 is set as the random number value
n=nrow(dataset)
#This is used to give the number of rows present in the dataset and we equate it to n
indexes = sample(n,n*(80/100)) # the ratio of trainset is 80% and testset is 20%
trainset = dataset[indexes,]
testset = dataset[-indexes,]
fit = glm(Y.house.price.of.unit.area~X6.longitude+X5.latitude+X1.transaction.date+X2.house.age+X4.number.of.convenience.stores+X3.distance.to.the.nearest.MRT.station
          ,dataset, family='gaussian') #Fitting the Model
#Where the dependent variable is Y.house.price.of.unit.area and the rest are independent variables 
coef(fit)#Co-efficient's of model
summary(fit)# show results
pred=predict(fit,testset)#This is used to predict the predicted value according to the linear regression setup
actual=testset$Y.house.price.of.unit.area #This is useful in bringing out the actual values present in the dataset
mse=sum((pred-actual)^2)/nrow(testset) #This is used for getting the Mean Squared Error
rmse=sqrt(mse) #This is the formula for obtaining the root mean squared error
rmse
Rsquar=1-mse/var(actual) #This is used to obtain the R squared which signifies the performance of the model
Rsquar
trainset.glm <- glm(trainset$Y.house.price.of.unit.area ~.,trainset, family="gaussian") #The following steps are  used to fit the test dataset using the trained model
phat=predict(trainset.glm,testset, type="response")
length(actual)
predictedvalues=rep(0,length(phat))
predictedvalues[phat>0.5]=1 
#The below steps are used give us the confusion matrix 
CrossTable(actual,predictedvalues)
table(actual,predictedvalues)


#Question 3

# A)State the Hypothesis
#For this we have to consider the Null Hypothesis and the Alternative Hypothesis
# H0 = Null Hypothesis
# H1 = Alternative Hypothesis
# H0: Gender and Opinion on Women Reservation are independent
# H1: Gender and Opinion on Women Reservation are not independent

# B)	Find the statistic and critical values
# alpha=0.05
# Since 0.05 is the level of significance, i have opted for Chi square test,as it is according to me the best method to analyse the fluctuations in the same set of variables

#To move ahead with the chi squared test, we have to calculate the degree of freedom(DF) 

#DF = (R-1) * (C-1),where R is the no of rows and C is the no of columns in the table specified
# In our table we have R = 2 and C = 3
DF <- (2-1)*(3-1)
DF
#So,from the above formula it is clear that the value of DF is 2
#After computing the value of DF, we need to calculate expected frequency count
#Expected frequency count is basically a probability count used in chi square test while tackling table calculations
#To calculate the EFC(Expected Frequency count) we have to sum up the total of the 'a'th row and the total in the 'b'th column, then divide the result by the total

#c)Explain your decision and Interpret results
#EFC = (aR * bC)/n where n represents the total population, 'a' represents row and 'b' represents column
# n = 1000
EFC1.1 = (400*450)/1000
EFC1.2 = (400*450)/1000
EFC1.3 = (400*100)/1000
EFC2.1 = (600*450)/1000
EFC2.2 = (600*450)/1000
EFC2.3 = (600*100)/1000
EFC1.1
# The value is 180
EFC1.2
# The value is 180
EFC1.3
# The value is 40
EFC2.1
# The value is 270
EFC2.2
# The value is 270
EFC2.3
# The value is 60
# Now we have to calculate Chi square statistic, for that the formula is the summation of the EFC minus the the value under consideration divided by the EFC
xSQR1 =((200-180**2)/EFC1.1) 
xSQR2 =((150-180**2)/EFC1.2)  
xSQR3 =((50-40**2)/EFC1.3)  
xSQR4 =((250-270**2)/EFC2.1) 
xSQR5 =((300-270**2)/EFC2.2) 
xSQR6 =((50-60**2)/EFC2.3)
xSQR1
xSQR2
xSQR3
xSQR4
xSQR5
xSQR6
xSQRZ = xSQR1 + xSQR2 + xSQR3 + xSQR4 + xSQR5 + xSQR6
#The above formula gives us chi square statistic value 
xSQRZ
# The chi square value is 16.2
#Now we have to use the degree of freedom and the chi square value to calculate the p square value
pchisq(16.2, df=2, lower.tail=FALSE)
#The p square value is 0.0003

#The p square value of 0.0003 is lesser than the value of significance level of 0.05, because of this we have to reject the null hypothesis
# So after performing the chi square test, i have come to the conclusion that there is a relationship between opinion and gender on women reservation