#Here is another project using logistic regression modeling
#We will use the UCI adult dataset
#The dataset has many features, we will use these features to see which predict salary
#Our threshold will be adults who make 50K or less vs. more than 50K


#Here we are reading in the dataset and naming the dataframe "adult"
#There are 32,561 observations of 16 variables
adult <- read.csv('adult_sal.csv')

#Here we are looking at the first few rows of data
#We seem to have a double index. The "X" column is repetitive 
print(head(adult))

#Here we are importing the dplyr library
library(dplyr)

#Here we are removing the "X" column since it's repetitive and not needed
adult <- select(adult, -X)


#Here we are looking at the structure of the dataset
str(adult)


#Here is a summary of the dataset
#Country has 42 levels and occupation has 15 levels. Education has 16 levels
#We need to decide on whether to remove these columns or use feature engineering
summary(adult)


#Here we are checking out just the type_employer column
#There are 1836 "?" in this column, basically NA values
#Maybe we should combine "Never-Worked" with "Without-Pay"
table(adult$type_employer)


#Here we are going to create a function to combine Never Worked with Without Pay
#We will name the function "unemployment"
unemployment <- function(job){
  #We're making sure job is a character so we can compare strings
  job <- as.character(job)
  #Now we are ensuring that job = never-worked or without-pay for our filtering
  #The pipe operator | is for "or"
  if (job=='Never-worked' | job=='Without-pay'){
    #If the job titles do match those criteria, we are converting them to "unemployed"
    return('Unemployed')
  }else{
    #If the job titles do not match those criteria, then we simply return their current job title
    return(job)
  }
}


#Here we are actually applying our newly created function to the type_employer column
#Basic feature engineering/cleaning
adult$type_employer <- sapply(adult$type_employer, unemployment)
print(table(adult$type_employer))


#We also have Self-emp-inc and Self-emp-not-inc
#So basically two categories of self-employment, but incorporated vs. not-incorporated
#It would make sense to simply combine those two columns


#In addition we are going to combine Local-gov and State-gov.
#I don't personally agree with this, but it's part of the exercise


#Here we are going to create a function to combine Self-emp-inc and Self-emp-not-inc along with Local-gov and State-gov
#We are basically copying the function we created above for unemployment
#We don't need to include job <- as.character(job) because that was taken care of the first time around
#We will name the function "group_emp"
group_emp <- function(job){
  #Now we are ensuring that job = never-worked or without-pay for our filtering
  #The pipe operator | is for "or"
  if (job=='Local-gov' | job=='State-gov'){
    #If the job titles do match those criteria, we are converting them to "SL-gov" for State and Local Gov
    return('SL-gov')
    #Now we are ensuring that job = Self-emp-inc or Self-emp-not-inc for our filtering
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    #If the job titles do match those criteria, we are converting them to "self-emp" for self-employed
    return('self-emp')
  }else{
    #If the job titles do not match the criteria we made above, then we simply return their current job title
    return(job)
  }
}



#Now we are going to apply the group-emp function to the type_employer column again
#We have reduced the factors in the type_employer column. Makes it much easier to do analysis
adult$type_employer <- sapply(adult$type_employer, group_emp)
print(table(adult$type_employer))



#Just like we did with the various employment categories, we should also check in on the marital status categories
#Which categories can be combined so we can reduce factors?
#We're going to simplify them into three categories: Married, Not-Married, and Never-Married


#Here we are creating our actual function to convert the marital categories
#We will call the function "group_marital"
group_marital <- function(mar){
  #Like with job above, we are transforming the marital status into a character
  mar <- as.character(mar)
  #Here we are setting the criteria for Not-Married
  #If you are separated, divorced, or widowed then that's how you will be designated
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    #If you're Never-Married, then we will leave you as such
  }else if(mar=='Never-married'){
    return(mar)
    #If you are Married, then we will leave you as such
  }else{
    return('Married')
  }
}



#Now we will apply our group_marital function
#We now just have Married, Never-married, and Not-married as our categories
adult$marital <- sapply(adult$marital, group_marital)
print(table(adult$marital))



#So far we've examined both the type_employment and marital columns
#Now we are going to investigate the country column
#There are lots of levels in the country column. This makes it difficult to analyze
#As you can see there are lots of countries included in this dataset
table(adult$country)



#In order to account for all of the different countries, it is easier to group them in regions
#Here is the Asia region
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

#Here is the North America region
North.America <- c('Canada','United-States','Puerto-Rico' )

#Here is the Europe region
Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

#Here is the Latin and South America Region
Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')

#We created an "Other" region to account for the generic level of "South
Other <- c('South')



#Now that we've grouped the countries into regions, we actually have to convert the country column to those regions
#We will create a function to actually do this. The name will simply be "group_country"
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}


#Here we are applying the function to the country column
#We do indeed have Asia, Europe, Latin.and.South.America, North.America, and Other
#Much simpler to read and understand now
#There are 671 values in Asia, 521 for Europe, 1301 for L/S America, 29405 for North.America, and 663 for Other
adult$country <- sapply(adult$country, group_country)
print(table(adult$country))


#Here we are checking the structure of the dataframe again 
#As you can see, type_employer, marital, and country are not all characters (chr)
#We will have to re-factor these columns in order to do regression analysis
str(adult)

#Here we are officially converting those three columns into factors
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)

#The changes officially worked
#type_employer is a factor with six levels
#marital is a factor with three levels
#country is a factor with five levels
#There are still many factors in education and occupation. It's up to you if you want to change those
str(adult)


#Now we will move on and check to see if there are missing values
#First we will import the Amelia library
library(Amelia)


#We noticed near the beginning that there were many "?" as values in our dataset
#We should convert those question marks to NA
#Here we are doing the actual conversion
adult[adult == '?'] <- NA

#Here we are looking at the type_employer column and there are 0 values for the "?"
print(table(adult$type_employer))


#Here we are going to re-factorize the type_employer column
#This way we eliminate the "?" as a factor in our analysis
#As you can see, the factors went from 6 to 5 because of the question mark being eliminated
adult$type_employer <- sapply(adult$type_employer,factor)
str(adult)

#Here is a basic missmap for the adult dataframe
#This is a tool from the Amelia package
#Says 1% of the data is missing, with 99% observed
missmap(adult)

#Here is a cleaner looking missmap
#Looks like we have missing data both in occupation and in type_employer
#We can't really impute the average/median in these columns because they are not continuous numerical values
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


#We are simply going to drop missing data from our dataset
adult <- na.omit(adult)

#Now we are creating the same map. There are no longer yellow lines, meaning there are no longer missing values
#We did what we needed to do
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


#Here we are importing the ggplot2 and dplyr libraries
#They are both very useful for data visualization
library(ggplot2)
library(dplyr)


#Here we are creating a histogram for the age column
#We are going to colorize the ages by income
#Most people in this dataset make less than or equal to 50K a year, especially if they are younger
ggplot(adult, aes(age)) + geom_histogram(aes(fill=income), color = 'black',
                                         binwidth = 1) + theme_bw()


#Here we will build another histogram, but with hours worked per week
#40 hours per week clearly dwarfs the other ranges
ggplot(adult, aes(hr_per_week)) + geom_histogram() + theme_bw()


#Here we are simply renaming the country column to region
#This makes more sense since we grouped all the disparate countries into regions
adult <- rename(adult, region = country)

#Here we are just checking that worked and yes the country column is now called "region"
print(head(adult))


#Here we are creating a bar plot visualizing the region data
#We are going to colorize the data by income
#North America has more of everything
bplot <- ggplot(adult, aes(region)) + geom_bar(aes(fill = income), color = 'black') + theme_bw()
bplot


#We've done our cleaning, now it is time to actually build our logistic regression model
#First we will import the caTools library. This allows us to split the data
library(caTools)

#Then we set the seed to a specific number just so we get consistent results
set.seed(101)

#Here we are splitting the sample. You can choose any column, but it's good practice to choose the dependent variable
sample <- sample.split(adult$income, SplitRatio = 0.7)

#Here we are creating our training set
train <- subset(adult, sample == TRUE)

#Here we are creating our test set
test <- subset(adult, sample == FALSE)

#Here we are making the model for training
#We are predicting income based on all features (done with the period)
#glm = generalized linear model
#We had to put as.factor for income. In general it should have already been factorized
model <-glm(as.factor(income) ~ . , family = binomial(link = 'logit'), data = train)

#Here is a summary of our model
#Lots of significant variables based on age, education, and type of occupation
summary(model)

#Here we are applying the step function to our model
#The step function tries to remove variables that do not contribute to the model
#It tries a different combination of variables and then tries to keep the ones that are truly significant
#It uses AIC
#The Akaike information criterion is an estimator of prediction error and thereby relative quality of statistical models for a given set of data
#We can compare different logistic regression models with the step function
#We get different AIC values for each independent variable
new.step.model <-step(model)


#Here we are viewing a summary of the new step model
summary(new.step.model)


#Here we are creating the predicted values from our model
#We are creating a new column called predicted.income that is added to the test set
test$predicted.income <- predict(model, newdata = test, type = 'response')


#Here is our confusion matrix
table(test$income, test$predicted.income > 0.5)


#We have an 85% accuracy from our model
accuracy <- (6428+1395) / (6428+492+900+1395)
print(accuracy)


#Here is the recall at 93%
print(6428/(6428+492))


#Here is precision at 88%
print(6428/(6428+900))
