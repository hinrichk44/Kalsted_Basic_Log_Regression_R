#Here is a logistic regression project where we will study the effect of ads on behavior
#This dataset contains information on users of a certain social network
#A car company is seeing if their ads on this social network led to customers purchasing a car

#Here we are reading in the dataset
social.ads = read.csv("Social_Network_Ads.csv")

#Here is a brief look at the dataset
#Purchased is the dependent variable (1-0)
#We have User ID, Gender, Age, Estimated Salary as the independent variables
head(social.ads)


#We are going to simplify our dataset and just look at Age, Salary, and Purchased
#In the future, Gender is a good category to use as an independent variable
social.ads = social.ads[, 3:5]


#We are going immediately into splitting the training and test set
#Here we are importing the caTools library, which is useful for splitting the data
library(caTools)


#Here we are setting the seed to 123. This is just to get consistent results
set.seed(123)

#Here we are setting the split to 80-20
split = sample.split(social.ads$Purchased, SplitRatio = 0.8)

#Here we are creating our training and test sets
training_set = subset(social.ads, split == TRUE)
test_set = subset(social.ads, split == FALSE)


#Here we are implementing Feature Scaling. Feature Scaling is almost always necessary for logistic regression
#Remember that you are scaling the X variables, not the Y-variable
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])


#Now we will fit our logistic regression model to the training set
#The classifier is the model!
classifier = glm(formula = Purchased ~ . , family = binomial, data = training_set)

#The model has been fitted, so it is time for predicting the test set results
#Type = response will give us the probabilities in a nice vector
#We have to identify the new observations we want to predict, in this case the test set
#We are removing the last column (Purchased) because that's what we are predicting
#We want to measure our predictions vs. actual results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])

#As you can see, you now have a bunch of probabilities (ranging from 0 to 1)
prob_pred

#We want to convert all the probabilities close to 0 as 0, and close to 1 as 1
#We are going to create a vector of converted results
#We will set the threshold at 0.5. Anything over that is considered a 1
#The 1 after 0.5 is indicating we want values over 0.5 to be 1
#The last argument is the value you want if the condition is false. So if prob is lower than 0.5, we want 0
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Indeed now we have only 1s and 0s
y_pred


#Now we will make the confusion matrix and evaluate our results
#So we first take the real results (from the test set)
#Then we enter in the predicted values of y_pred
cm = table(test_set[, 3], y_pred)

#Here is the full confusion matrix
#Correct predictions are 44 + 20 = 64
#Incorrect are 9 + 17 = 26
cm


#Now we will visualize the training test results
#We installed ElemStatLearn the old fashioned way since it was removed from CRAN
#Here we are importing the ElemStatLearn library
library(ElemStatLearn)


#All the points you see on this graph are actual observations in the training set
#Age is on the X-axis while Estimated Salary is on the Y-axis
#The red data points are where the dependent variable Purchased = 0
#The green data points are where the dependent variable Purchased = 1
#In general it looks like those who were older and had more money were more likely to purchase the car. Not surprising
#What exactly are we trying to make this classifier do for our business problem?
#The goal is to classify the right users into the right categories
#The points on this graph are the truth, the colored regions are the predictions
#If there's a green dot in the red region, that is an incorrect prediction
#The straight line that divides the green and red regions is called the "prediction boundary"

#Here we are making set = training set because that makes code easier to copy
set = training_set
#Here we are building our grid. We take the min and max values and increase each by one in their direction
#This allows the graph to not look "squeezed"
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
#We basically created a matrix with X1 and X2. 
grid_set = expand.grid(X1, X2)
#Here we are giving names to the columns in the newly created matrix
colnames(grid_set) = c('Age', 'EstimatedSalary')
#Here we are creating the predictions from our classifier
prob_set = predict(classifier, type = 'response', newdata = grid_set)
#Here we are setting the predictions into official 1-0 categories
y_grid = ifelse(prob_set > 0.5, 1, 0)
#Here is the actual part where we plot the graph
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
#Here we are coloring the regions
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
#Here we are coloring the points
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))





#Here we are visualizing the Test set results
#Overall looks like we captured most of the observations correctly
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
