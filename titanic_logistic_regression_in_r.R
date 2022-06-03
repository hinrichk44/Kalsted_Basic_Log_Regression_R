#To understand the actual math behind logistic regression, you can read Introduction to Statistical Learning by Gareth James
#Specifically, Sections 4-4.3

#We like Logistic Regression for Classification
#Spam emails, approving loans (Y/N), cancer diagnosis
#Logistic = discrete, while Linear = continuous

#The Sigmoid Function takes in any value and outputs it to be between 0 and 1
#This 0 to 1 is a PROBABILITY. Threshold usually at 0.5
#We can actually take the linear model and place it into the denominator of the Sigmoid Function

#You can evaluate the logistic model's performance using a Confusion Matrix
#Predicted vs. Actual. True Positives/Negatives vs. False Positives/Negatives
#Accuracy: (TP + TN) / Total
#Error Rate: (FP + FN) / Total
#Type I Error: False Positive. You say you're pregnant when you're not
#Type II Error: False Negative. You say you're not pregnant when you really are


#For our first exercise, we will be using the classic Titanic dataset
#Who survived? Who died?
#Here we are importing the training dataset. Usually you would have to split the dataset yourself
#There are 891 observations of 12 variables in this dataset
titanic_train <- read.csv('titanic_train.csv')

#Here we are checking out the first few observations
print(head(titanic_train))

#Here we are checking out the basic structure of the training set
#PassengerID
#Survived: A logical value of 0 or 1. 1 = Survived, 0 = Died
#PClass: Class on the Titanic. 1st class 2nd class, etc.
#Name
#Sex
#Age
#SibSp: Siblings or spouses on board
#Parch: Parents or children on board
#Ticket
#Fare
#Cabin
#Embarked
print(str(titanic_train))


#Here we are installing the Amelia package
#We are then importing the Amelia library
#This package can be used to find missing values
#Specifically a "missingness map"
#Named after Amelia Earhart who famously went missing
#install.packages('Amelia')
library(Amelia)

#Here we are creating our missingness map 
#First value is the object you are lookign at, in this case our Titanic training set
#Second value (main) is the title of the missingness map
#Third value (col) is the color of our cells. First is the color of our missing cells.Second color is for observed cells
#We clearly have a lot of missing values in the Age column
missmap(titanic_train, main = "Titanic Missing Map", col = c('yellow', 'black'), legend = FALSE)


#Here we are installing the ggplot2 package
#We are then importing the ggplot2 library
#install.packages('ggplot2')
library(ggplot2)


#Here we will use ggplot2 to build a bar chart
#We will take a look at the Survived column
#aes = "aesthetic" 
ggplot(titanic_train, aes(Survived)) + geom_bar()


#Here we will use ggplot2 to build a bar chart
#We will take a look at the Pclass column
#We are coloring the bar charts according to the factors in the Pclass column
#3rd class passengers are the largest contingent
ggplot(titanic_train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))


#Here we will use ggplot2 to build a bar chart
#We will take a look at the Sex column
#We are coloring the bar charts according to the factors in the Sex column
#Male passengers are the largest contingent
ggplot(titanic_train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))



#Here we will use ggplot2 to build a histogram
#We will take a look at the Age column
#We get a warning message that the missing values were removed before producing the histogram
#The ages skew young, especially in the early 20s
ggplot(titanic_train, aes(Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')



#Here we will use ggplot2 to build a bar chart
#We will take a look at the SibSp column
#Most passengers had 0 siblings with them on board
ggplot(titanic_train, aes(SibSp)) + geom_bar()



#Here we will use ggplot2 to build a histogram
#We will take a look at the Fare column
#Looks like most people did not pay a fare to get on board
ggplot(titanic_train, aes(Fare)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'green', color = 'black')



#Here we are building a box plot with ggplot2
#We will examine the Pclass and Age columns
#So we are sorting by Pclass, and then the age of each passenger within that class
#scale_y_continuous allows you to pick which numbers you want on the y-axis
#We will go from 0 to 80 in increments of 2
#First class passengers are the oldest, followed by second class, then third class
new_plot <- ggplot(titanic_train, aes(Pclass, Age))
new_plot <- new_plot + geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.4))
new_plot + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))




#As we discovered above, there are many missing values in the Age column
#One way to adjust for this is to impute data into the column
#We will do imputation of age based on class
#We will name our function simply "impute_age"
#We will first take in age and class
impute_age <- function(age,class){
  #The output will be the age
  out <- age
  #"For every element in the age column"
  for (i in 1:length(age)){
    #"I will check if that element is an NA value"
    #If it is NA, I continue down the iteration
    if (is.na(age[i])){
      #If class = 1, then we will impute with 37, which is the average age of the first class passenger
      if (class[i] == 1){
        out[i] <- 37
       #If not class 1, but class 2, then we will impute with 29, which is the average age of the 2nd class passenger 
      }else if (class[i] == 2){
        out[i] <- 29
       #If not first or second class, we will impute with 24, which is the average age of the 3rd class passenger 
      }else{
        out[i] <- 24
      }
      #If not NA, then we will just output the age that is already in the column
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}


#Here we will now apply our impute_age function to our Age column
fixed_ages <- impute_age(titanic_train$Age, titanic_train$Pclass)
titanic_train$Age <- fixed_ages

#We are creating a missingness map again to check if our function worked properly
#The map is entirely black, no yellow for missing values
#This indeed suggests that our function worked properly
missmap(titanic_train, main = 'Imputated Titanic Map', col = c('yellow', 'black'), legend = FALSE)


#In order to move forward with our analysis, we need to decide which variables are useful
#We will not use Name, PassengerID, Ticket, and Cabin because there are way too many levels/factors
#To be advanced you could hypothetically use feature engineering to make these variables useful


#Here we are installing the dplyr package
#Then importing the dplyr library
#install.packages('dplyr')
library(dplyr)

#Here we are going to select the columns we want for analysis from the training set
#You can either say which columns you want, or use a negative sign to REMOVE the columns you don't want
titanic_train <- select(titanic_train, -PassengerId, -Name, -Ticket, -Cabin)

#Here we are checking our dataset after removing the unwanted columns
#Indeed we successfully removed the columns
print(head(titanic_train))


#Some of the columns we have would be better if they were converted to factors instead of integers
#You could argue that the amount of siblings or parents should be left as continuous integers
titanic_train$Survived <- factor(titanic_train$Survived)
titanic_train$Pclass <- factor(titanic_train$Pclass)
titanic_train$Parch <- factor(titanic_train$Parch)
titanic_train$SibSp <- factor(titanic_train$SibSp)


#Here we will begin to actually train our logistic regression model
#Must start with glm, which stands for "generalized linear model"
#You then add your dependent variable first, in this case Survived
#The period after ~ means "every column"
log.model <- glm(Survived ~ . , family = binomial(link = 'logit'), data = titanic_train)

#Here we are looking at a summary of our model
#The stars indicate significance
#Three stars > two stars > one star > no stars
#Second and third class passengers mattered when it came to who survived. NEGATIVE CORRELATION
#Being male was important as to who survived. NEGATIVE CORRELATION
#Age mattered as to who survived. NEGATIVE CORRELATION
#Having 3-4 siblings or spouses also mattered. NEGATIVE CORRELATION
summary(log.model)


#We are going to make a test set out of our training set

#Here we are installing the caTools package
#Then importing the caTools library
#install.packages('caTools')
library(caTools)


#Here we are setting the seed to 101
#The number itself doesn't matter much, we just want the splitting to be consistent each time
set.seed(101)

#Now we will split the dataset
#We will split it 70-30
split <- sample.split(titanic_train$Survived, SplitRatio = 0.7)

#Here are our training and test sets
#Training set has 623 observations
#Test set has 268 observations
final.train <- subset(titanic_train, split == TRUE)
final.test <- subset(titanic_train, split == FALSE)

#We are going to re-run our logistic regression model using the training set
#Then we will test the model on the test set
final.log.model <- glm(Survived ~ . , family = binomial(link = 'logit'), data = final.train)

#Here's a summary of the model trained on the training set
summary(final.log.model)


#Now we will use our model to predict outcomes
fitted.probabilities <- predict(final.log.model, final.test, type = 'response')
#If our fitted probability is greater than 0.5, set it to 1. Otherwise set it to 0
fitted.results <- ifelse(fitted.probabilities>0.5,1,0)


#Here we are calculating our error rate
#We are seeing how many predictions do not equal the actual results (from our test set)
#We are then taking the average of that number
mis.class.error <- mean(fitted.results != final.test$Survived)
#Our accuracy was 0.798 (1 - error rate)
print(1 - mis.class.error)


#Here we are going to create an actual confusion matrix
#This will be a better way to evaluate our predictions
table(final.test$Survived, fitted.probabilities > 0.5)
