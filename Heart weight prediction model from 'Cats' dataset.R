# Heart weight prediction model for Cats Dataset
## using Cat dataset available in MASS package.
install.packages("MASS")
library(MASS)

## Loading dataset
data("cats")

## using head() to view content of data
head(cats)

## to see the complete dataset
View(cats)

## As the data is inbuilt so we don't need to read it separately , we will analyze the data set and then we can split it in two parts as train and test.

## Step1: Analyse the data set!

### identifying number of rows and columns
dim(cats)
nrow(cats)
ncol(cats)

### Checking for missing values
sum(is.na(cats))
which(is.na(cats))

### Checking structure and summary of the dataset
str(cats)
summary(cats)

### Checking class of variables
sapply(cats,class)

### Finding the correlation between Body 'weight' and 'Heart weight'
plot(cats$Bwt ,cats$Hwt)

### Replacing blanks or spaces with NA, even-though we have none!
cats[cats==" "] <- NA

## Step 2: visualizing the data
## Finding correlations between 'Heart weight' and 'sex' of cats using visual plots

### Installing package ggplot2
install.packages("ggplot2")
### Load the ggplot2 library for plotting
library(ggplot2)

## Create a box plot and violin plot to visualize the relationship between 'Hwt' and 'Sex'
### Box plot
ggplot(cats, aes(x = Sex, y = Hwt)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Heart Weight")

### Violin plot
ggplot(cats, aes(x = Sex, y = Hwt)) +
  geom_violin() +
  labs(x = "Sex", y = "Heart Weight")


### creating a new data set cats2 so we don’t alter the original cats dataset
cats2 <- cats
dim(cats2)
View(cats2)

## Step 3: Splitting the dataset into Test and Train data
### Generating a random sample of indices representing the training set from the dataset 'cats2'
train_ind <- sample.int(n = nrow(cats2), size = floor(0.75 * nrow(cats2)), replace = FALSE)

### splitting the dataset cats2 into a training set and a test set based on the indices generated earlier
train <- cat_c[train_ind,]
test <- cat_c[-train_ind,]

### Observing the test and train data
head(train,10)
head(test,10)
dim(train)
dim(test)

## Step 3: Finding Correlation between variables

### Attaching train data
attach(train)

### plotting the relationship between 'Hwt' and 'Bwt' and then perform a correlation test between the two variables
### Plotting Heart Weight against Body Weight
plot(cat_c$Hwt, cat_c$Bwt, xlab = "Heart Weight", ylab = "Body Weight", main = "Heart vs Body")

### Perform a correlation test
correlation_test <- cor.test(cat_c$Hwt, cat_c$Bwt)

### Print the correlation test result
print(correlation_test)
### There is strong positive correlation of ~ 80%

## Step 4: Performing regression analysis
### Conducting linear regression analysis
linear_model1 <- with(train, lm(Hwt ~ Bwt + Sex))
### As the variable sex is not significant lets drop it
linear_model1 <- with(train, lm(Hwt ~ Bwt))
summary(linear_model1)

### plot of linear_model ,it helps to understand the accuracy and to identify the hidden behaviour of data
plot(linear_model1) 
# Analyzing all 4 plots

## Step 5: Yay! Let's predict
### Predicting values of 'Hwt' using the linear regression mode
Hwt_Predicted <- predict(linear_model1, test, method = "class")

### Inspecting the predicted and actual values of 'Hwt' side by side using a data frame view
View(data.frame(Hwt_Predicted, test$Hwt))

### Adding the Predicted Values to Test dataset
test <- cbind(test, Hwt_Predicted)

View(test)



