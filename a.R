#Installing the needed packages
install.packages('readr')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.packages('plotly')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')
library(readr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)
#Input the dataset
data(Housing)
housing <- Housing

#Expolartory Data Analysis - using ggplot2 and corrplot
#Correlation
corrplot(cor(select(housing,-chas)))
#medv Desnsity plot using ggplot2
#visualizing the distribution of the target variable 
housing %>% 
  ggplot(aes(medv)) +
  stat_density() + 
  theme_bw()
#medv density using ploty
ggplotly(housing %>% 
           ggplot(aes(medv)) +
           stat_density() + 
           theme_bw())
#using medv
housing %>%
  select(c(crim, rm, age, rad, tax, lstat, medv,indus,nox,ptratio,zn)) %>%
  melt(id.vars = "medv") %>%
  ggplot(aes(x = value, y = medv, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()

#Model Building and Prediction
#Train and Test Data
#set a seed 
set.seed(123)
#Split the data , `split()` assigns a booleans to a new column based on the SplitRatio specified.
split <- sample.split(housing,SplitRatio =0.75)
train <- subset(housing,split==TRUE)
test <- subset(housing,split==FALSE)
# train <- select(train,-b)
# test <- select(test,-b)


#Training the model
model <- lm(medv ~ crim + rm + tax + lstat , data = train)
summary(model)

#Visualizing the model
res <- residuals(model)
# Convert residuals to a DataFrame 
res <- as.data.frame(res)
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)

#Predictions
test$predicted.medv <- predict(model,test)
pl1 <-test %>% 
  ggplot(aes(medv,predicted.medv)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv')+
  theme_bw()
ggplotly(pl1)

#Assessing the model 
error <- test$medv-test$predicted.medv
rmse <- sqrt(mean(error)^2)
