salary <- read.csv(file.choose())
View(salary)

#split into training set and test set
library(caTools)
set.seed(123)
split=sample.split(salary$Salary_hike ,SplitRatio = 2/3)
training_set=subset(salary,split==TRUE)
test_set=subset(salary,split==FALSE)

#fitting slrt training set
regressor=lm(formula =  Salary_hike~Churn_out_rate,
             data=training_set)

#predicting test set results
y_pred=predict(regressor,newdata=test_set)

#visual training of the data set
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$Churn_out_rate,y=training_set$Salary_hike),
             colour='red')+
  geom_line(aes(x=training_set$Churn_out_rate,y=predict(regressor,newdata=training_set)),
            colour='blue')+
  ggtitle('Salary_hike vs Churn_out_rate(Test set)')+
  xlab('Churn_out_rate')+
  ylab('Salary_hike')


#visualising test set data
library(ggplot2)
ggplot()+
  geom_point(aes(x=test_set$Churn_out_rate,y=test_set$Salary_hike),
             colour='red')+
  geom_line(aes(x=training_set$Churn_out_rate,y=predict(regressor,newdata=training_set)),
            colour='blue')+
  ggtitle('Salary_hike vs Churn_out_rate(Test set)')+
  xlab('Churn_out_rate')+
  ylab('Salary_hike')










