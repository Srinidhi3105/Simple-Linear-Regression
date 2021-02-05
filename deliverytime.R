delivery_time <- read.csv(file.choose())
View(delivery_time)

#split into training set and test set
library(caTools)
set.seed(123)
split=sample.split(delivery_time$Delivery.Time,SplitRatio = 2/3)
training_set=subset(delivery_time,split==TRUE)
test_set=subset(delivery_time,split==FALSE)

#fitting slrt training set
regressor=lm(formula = Delivery.Time~Sorting.Time,
             data=training_set)

#predicting test set results
y_pred=predict(regressor,newdata=test_set)

#visual training of the data set
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$Sorting.Time,y=training_set$Delivery.Time),
             colour='red')+
  geom_line(aes(x=training_set$Sorting.Time,y=predict(regressor,newdata=training_set)),
            colour='blue')+
  ggtitle('Delivery.Time vs Sorting.Time(Test set)')+
  xlab('Sorting.Time')+
  ylab('Delivery.Time')


#visualising test set data

library(ggplot2)
ggplot()+
  geom_point(aes(x=test_set$Sorting.Time,y=test_set$Delivery.Time),
             colour='red')+
  geom_line(aes(x=training_set$Sorting.Time,y=predict(regressor,newdata=training_set)),
            colour='blue')+
  ggtitle('Delivery.Time vs Sorting.Time(Test set)')+
  xlab('Sorting.Time')+
  ylab('Delivery.Time')








