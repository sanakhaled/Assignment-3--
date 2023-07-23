## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
# Install tidyverse
install.packages('tidyverse')

# Import the tidyverse library
library(tidyverse)

# Import the data set.
sales <- read.csv(file.choose(), header = TRUE)

# Print the data frame.
# View the data frame
view(sales)

# View the structure of the data frame 
str(sales)

# View the attributes of the data frame
attributes(sales)

# Check the type of the data frame.
typeof(sales)

# Check the class of the data frame.
class(sales)

# Check the dimensions of the data frame
dim(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_1 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
view(sales_1)

# View the descriptive statistics.
dim(sales_1)
summary(sales_1)
################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, Global_Sales, colour = Platform, data = sales_1)
ggplot(sales_1, mapping=aes(x=NA_Sales,y= Global_Sales), color = 'red') + geom_point(alpha=0.5,size=1.5) +
  geom_smooth(method = 'lm',se= FALSE,size=1.5)

ggplot(sales_1, mapping=aes(x=NA_Sales,y= EU_Sales), color = 'red') + geom_point(alpha=0.5,size=1.5) +
  geom_smooth(method = 'lm',se= FALSE,size=1.5)

ggplot(sales_1, mapping=aes(x=EU_Sales,y= Global_Sales), color = 'red') + geom_point(alpha=0.5,size=1.5) +
  geom_smooth(method = 'lm',se= FALSE,size=1.5)

## 2b) Histograms
# Create histograms.
ggplot(sales_1, aes(Global_Sales)) + geom_histogram(bins=10)

## 2c) Boxplots
# Create boxplots.
qplot(Global_Sales, NA_Sales, data = sales_1, geom = 'boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
#The Scatterplot displaying the relationship between NA_Sales and EU_Sales is heavily dense at the tail indicating that the data is positively skewed. There is an extreme outlier which can be observed in all the plots. The relation between Global and NA Sales however is more linear and follows a positive direction as shown in the diagrams below. The boxplot and histogram for NA and Global Sales also indicate a very skewed relation with a few outliers above 10M which should be observed as well. 



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales_1)

# Check output: Determine the min, max, and mean values.
# min, max and mean values of NA_Sales 
min(sales_1$NA_Sales)
max(sales_1$NA_Sales)
mean(sales_1$NA_Sales)

# min, max and mean values of EU_Sales 
min(sales_1$EU_Sales)
max(sales_1$EU_Sales)
mean(sales_1$EU_Sales)

# min, max and mean values of Global_Sales
min(sales_1$Global_Sales)
max(sales_1$Global_Sales)
mean(sales_1$Global_Sales)

# View the descriptive statistics.
summary(sales_1$NA_Sales)
summary(sales_1$EU_Sales)
summary(sales_1$Global_Sales)

IQR(sales_1$NA_Sales)
IQR(sales_1$EU_Sales)
IQR(sales_1$Global_Sales)

# Determine the measures of dispersion. Variance and Standard Deviation
var(sales_1$NA_Sales)
sd(sales_1$NA_Sales)

var(sales_1$EU_Sales)
sd(sales_1$EU_Sales)

var(sales_1$Global_Sales)
sd(sales_1$Global_Sales)
###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_2 <- aggregate(Global_Sales~Product,sales_1, sum)
sales_3 <- aggregate(NA_Sales~Product,sales_1,sum)
sales_4 <- aggregate(EU_Sales~Product,sales_1,sum)

sales_5 <- cbind(sales_2, sales_3, sales_4)
sales_6 <- sales_5[, -c(3,5)]

sales_7 <- sales_1 %>% group_by(Product) %>% summarise(sum_Global = sum(Global_Sales),
                                                       sum_NA = sum(NA_Sales),
                                                       sum_EA = sum(EU_Sales))
  
# View the data frame. 
View(sales_2)
View(sales_3)
View(sales_4)
View(sales_5)
View(sales_6)
View(sales_7)

# Explore the data frame.
summary(sales_6)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
ggplot(sales_6, aes(x = Product, y = Global_Sales)) + geom_point(alpha=0.5, size=1.5, colour = 'red')+
  geom_smooth(se = FALSE)+labs(x = "Product", y = "Global_Sales", title= "Relation between Product and Global Sales")

ggplot(sales_6, aes(x = Product, y = NA_Sales)) + geom_point(alpha=0.5, size=1.5, colour = 'red')+
  geom_smooth(se = FALSE)+labs(x = "Product", y = "NA_Sales", title= "Relation between Product and NA Sales")

ggplot(sales_6, aes(x = Product, y = EU_Sales)) + geom_point(alpha=0.5, size=1.5, colour = 'red')+
  geom_smooth(se = FALSE)+labs(x = "Product", y = "EU_Sales", title= "Relation between Product and EU Sales")


# Create histograms.
hist(sales_6$Global_Sales)
hist(sales_6$NA_Sales)
hist(sales_6$EU_Sales)

# Create boxplots.
boxplot(sales_6$Global_Sales, color = 'blue')
boxplot(sales_6$NA_Sales)
boxplot(sales_6$EU_Sales)

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_6$Global_Sales, col="blue", xlab= "z value", ylab ="Global Sales")
qqline(sales_6$Global_Sales, col='red', lwd =2)

qqnorm(sales_6$NA_Sales, col="blue", xlab= "z value", ylab ="NA Sales")
qqline(sales_6$NA_Sales, col='red', lwd =2)

qqnorm(sales_6$EU_Sales, col="blue", xlab= "z value", ylab ="EU Sales")
qqline(sales_6$EU_Sales, col='red', lwd =2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_6$Global_Sales)
shapiro.test(sales_6$NA_Sales)
shapiro.test(sales_6$EU_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(sales_6$Global_Sales)
kurtosis(sales_6$Global_Sales)

skewness(sales_6$NA_Sales)
kurtosis(sales_6$NA_Sales)

skewness(sales_6$EU_Sales)
kurtosis(sales_6$EU_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(sales_6$NA_Sales, sales_6$Global_Sales)
cor(sales_6$EU_Sales, sales_6$Global_Sales)
cor(sales_6$NA_Sales, sales_6$EU_Sales)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Scatterplot to identify the relation between NA and Global Sales with
# a smoothing line 

ggplot(sales_6, aes(jitter, x= NA_Sales, y= Global_Sales)) +
  geom_point( color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  scale_x_continuous(breaks = seq(0,60,5), 'NA Sales') +
  scale_y_continuous(breaks = seq(0,60,10), 'Global Sales') +
  labs(title = 'Relationship between NA and Global Sales',
       subtitle = 'Turtle Games') +
  theme_classic() 

# Scatterplot to identify the relation between EU and Global Sales

ggplot(sales_6, aes(jitter, x=EU_Sales, y= Global_Sales)) +
  geom_point( color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  scale_x_continuous(breaks = seq(0,60,5), 'EU Sales') +
  scale_y_continuous(breaks = seq(0,60,10), 'Global Sales') +
  labs(title = 'Relationship between EU and Global Sales',
       subtitle = 'Turtle Games') +
  theme_classic() 

# Scatterplot to identify the relation between EU and NA Sales

ggplot(sales_6, aes(jitter, x=EU_Sales, y= NA_Sales)) +
  geom_point( color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  scale_x_continuous(breaks = seq(0,60,5), 'EU Sales') +
  scale_y_continuous(breaks = seq(0,60,10), 'NA Sales') +
  labs(title = 'Relationship between EU and NA Sales',
       subtitle = 'Turtle Games') +
  theme_classic() 
  

# Histogram of EU Sales
ggplot(sales_6, aes(x=EU_Sales)) + geom_histogram(bins = 20)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#I believe the data could have been more comprehensive in terms of Sales. 
#There could have been more information regarding Sales from regions within NA and the EU. It is useful for Turtle Games to know which region within NA or EU is making the least and most amount of sales and hence target customers accordingly through their marketing campaigns as the data extracted is quite broad.
#The extreme tail end where most of the values are concentrated between 0.01M and 1M should be focused on. It can be observed that the products published by Nintendo make the most sales especially in NA. The best selling products belong to the platform of Wii. 
#There are some very prominent outliers below the linear regression line in the plot comparing the relation between NA and Global Sales and above the line in the plot comparing relation between EU and Global Sales. 




###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales_6)

# Determine a summary of the data frame.
summary(sales_6)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(sales_6$NA_Sales,sales_6$Global_Sales)
cor(sales_6$EU_Sales,sales_6$Global_Sales)
cor(sales_6$NA_Sales,sales_6$EU_Sales)

plot(sales_6$NA_Sales,sales_6$Global_Sales)
plot(sales_6$EU_Sales,sales_6$Global_Sales)
plot(sales_6$NA_Sales,sales_6$EU_Sales)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
salesA <- lm(Global_Sales ~ NA_Sales, data = sales_6)
salesA
summary(salesA)
coefficients(salesA)

# NA Sale values are highly significant explaining a variability of 83.95%
# Expect Global Sales to go up by 1.63 with each increase of NA Sales. 
plot(sales_6$NA_Sales, sales_6$Global_Sales)
abline(coefficients(salesA))

salesB <- lm(Global_Sales ~ EU_Sales, data = sales_6)
salesB
summary(salesB)
coefficients(salesB)

plot(sales_6$EU_Sales, sales_6$Global_Sales)
abline(coefficients(salesB))

salesC <- lm(EU_Sales ~ NA_Sales, data = sales_6)
salesC
summary(salesC)
coefficients(salesC)

plot(sales_6$NA_Sales, sales_6$EU_Sales)
abline(coefficients(salesC))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
cor(sales_6)

# plot the correlation
corPlot(sales_6, cex=2)


# Multiple linear regression model.
modela = lm(Global_Sales~NA_Sales+EU_Sales, data=sales_6)
summary(modela)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
# Create a new object and specify the predict function.
predictTest = predict(modela, newdata=sales_6,
                      interval='confidence')
# Print the object.
head(predictTest)
head(sales_6)

View(predictTest)
View(sales_6)
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# strong correlation between global and NA sales
# data to be more granular- available for different regions and diff months


###############################################################################
###############################################################################




