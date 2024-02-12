# Heart weight prediction model for Cats Dataset
## using Cat dataset available in MASS package.
install.packages("MASS")
library(MASS)

## Loading dataset
data("cats")

## using head() to view content of data
head(cats,10)

## As the data is inbuilt we don't need to read it separately, we will analyze the data set and then we can split it into two parts train and test.

## Step1: Analyse the data set!
### Identifying the number of rows and columns
dim(cats)
nrow(cats)
ncol(cats)

### Checking for missing values
sum(is.na(cats))
which(is.na(cats))

### Checking the structure and summary of the dataset
str(cats)
summary(cats)

### Checking class of variables
sapply(cats,class)

### Finding the correlation between Body 'weight' and 'Heart weight'
plot(cats$Bwt ,cats$Hwt)

### Replacing blanks or spaces with NA, even though we have none!
cats[cats==" "] <- NA

## Step 2: visualizing the data
### Finding correlations between 'Heart weight' and 'sex' of cats using visual plots
### Installing package ggplot2
install.packages("ggplot2")
### Load the ggplot2 library for plotting
library(ggplot2)

### Create a box plot and violin plot to visualize the relationship between 'Hwt' and 'Sex'
### Box plot
ggplot(cats, aes(x = Sex, y = Hwt)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Heart Weight")

### Violin plot
ggplot(cats, aes(x = Sex, y = Hwt)) +
  geom_violin() +
  labs(x = "Sex", y = "Heart Weight")


### creating a new data set cats2 so we don’t alter the original cats' dataset
cats2 <- cats
dim(cats2)
head(cats2,10)

## Step 3: Splitting the dataset into Test and Train data
### Generating a random sample of indices representing the training set from the dataset 'cats2'
train_ind <- sample.int(n = nrow(cats2), size = floor(0.75 * nrow(cats2)), replace = FALSE)

### Splitting the dataset cats2 into a training set and a test set based on the indices generated earlier
train <- cats2[train_ind,]
test <- cats2[-train_ind,]

### Observing the test and train data
head(train,10)
head(test,10)
dim(train)
dim(test)

## Step 4: Finding Correlation between variables
### Attaching train data
attach(train)

### plotting the relationship between 'Hwt' and 'Bwt' and then perform a correlation test between the two variables
### Plotting Heart Weight against Body Weight
plot(cats2$Hwt, cats2$Bwt, xlab = "Heart Weight", ylab = "Body Weight", main = "Heart vs Body")

### Perform a correlation test
correlation_test <- cor.test(cats2$Hwt, cats2$Bwt)

### Print the correlation test result
print(correlation_test)
### There is a strong positive correlation of ~ 80%

## Step 5: Performing regression analysis
### Conducting linear regression analysis
linear_model1 <- with(train, lm(Hwt ~ Bwt + Sex))
### As the variable sex is not significant let's drop it
linear_model1 <- with(train, lm(Hwt ~ Bwt))
summary(linear_model1)

### plot of linear_model,it helps to understand the accuracy and to identify the hidden behavior of data and analyzing all 4 plots
plot(linear_model1) 


## Step 6: Yay! Let's predict
### Predicting values of 'Hwt' using the linear regression mode
Hwt_Predicted <- predict(linear_model1, test, method = "class")

### Inspecting the predicted and actual values of 'Hwt' side by side using a data frame view
head(data.frame(Hwt_Predicted, test$Hwt),10)

### Adding the Predicted Values to the Test dataset
test <- cbind(test, Hwt_Predicted)
head(test,10)

## Step 7: Let's proceed with calculating the prediction accuracy
### Extract Predicted and Actual Values
predicted_values <- test$Hwt_Predicted
actual_values <- test$Hwt

### Calculating Mean Absolute Error (MAE)
mae <- mean(abs(predicted_values - actual_values))

### Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predicted_values - actual_values)^2))

### Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predicted_values - actual_values)^2))

### Calculating R-squared (R²)
rsquared <- cor(predicted_values, actual_values)^2

### Printing the evaluation metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R²):", rsquared, "\n")





