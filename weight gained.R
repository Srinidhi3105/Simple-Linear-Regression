weight_gained <- read.csv(file.choose())
View(weight_gained)

#split into training set and test set
library(caTools)
set.seed(123)
split=sample.split(weight_gained$Weight.gained..grams. ,SplitRatio = 2/3)
training_set=subset(weight_gained,split==TRUE)
test_set=subset(weight_gained,split==FALSE)

#fitting slrt training set
regressor=lm(formula = Weight.gained..grams.~Calories.Consumed,
             data=training_set)

#predicting test set results
y_pred=predict(regressor,newdata=test_set)

#visual training of the data set
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$Calories.Consumed,y=training_set$Weight.gained..grams.),
             colour='red')+
  geom_line(aes(x=training_set$Calories.Consumed,y=predict(regressor,newdata=training_set)),
            colour='blue')+
  ggtitle('Weight.gained..grams. vs Calories.Consumed(Test set)')+
  xlab('Calories.Consumed')+
  ylab('Weight.gained..grams.')


#visualising test set data

library(ggplot2)
ggplot()+
  geom_point(aes(x=test_set$Calories.Consumed,y=test_set$Weight.gained..grams.),
             colour='red')+
  geom_line(aes(x=training_set$Calories.Consumed,y=predict(regressor,newdata=training_set)),
            colour='blue')+
  ggtitle('Weight.gained..grams. vs Calories.ConsumedTest set)')+
  xlab('Calories.Consumed')+
  ylab('weight.gained..grams.')








