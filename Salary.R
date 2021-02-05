#simple linear regression
salary_years <- read.csv(file.choose())
View(salary_years)


#split dataset into training and test set
library(caTools)
set.seed(123)
split=sample.split(salary_years$Salary,SplitRatio = 2/3)
training_set=subset(salary_years,split==TRUE)
test_set=subset(salary_years,split==FALSE)

#fitting slr to training set
regressor=lm(formula = Salary~YearsExperience,
             data=training_set)


#predicting test set results
y_pred=predict(regressor,newdata=test_set)


#visualizing training set data
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),
             colour='red')+
  geom_line(aes(x=training_set$YearsExperience,y=predict(regressor,newdata = training_set)),
            colour='blue')+
  ggtitle('Salary vs Experience(Test set)')+
  xlab('years of experience')+
  ylab('salary')


#visualising test set results

library(ggplot2)
ggplot()+
  geom_point(aes(x=test_set$YearsExperience,y=test_set$Salary),
             colour='red')+
  geom_line(aes(x=training_set$YearsExperience,y=predict(regressor,newdata = training_set)),
            colour='blue')+
  ggtitle('Salary vs Experience(Test set)')+
  xlab('years of experience')+
  ylab('salary')
  