
library(plyr)
library(readr)
# Loading dataset
property_da <- read.csv('C:/Users/Saketh Vanam/Downloads/Data Science/Property_Price_Register.csv')
#displaying the structure of dataset 
str(property_da)

#displaying number of rows and columns
nrow(property_da)
ncol(property_da)

#data frame is created
property_df <- data.frame(property_da)

#displaying total number of null values
sum(is.na(property_df))
 
library(mice)
library(VIM)

#plotting null values
md.pattern(property_df)

#changing column names
colnames(property_df) <- c("Date", "Address", "Postal_code", "County", "Price", "Market_price", "VAT_Exclusive", "Description")
str(property_df)

#Removing € from price column
property_df$Price = as.numeric(gsub("[\\€,]", "", property_df$Price))
property_df$Price

#changing date field from factor to date and extracting only year from it.
date_field <- as.character(property_df$Date)
str(date_field)

year_field <- format(as.Date(property_df$Date, format="%d/%m/%Y"),"%Y")
property_df$Date <- year_field
str(property_df)
property_df$Date <- as.numeric(property_df$Date)
str(property_df)
summary(property_df)
barplot(table(property_df$Date)) #bar plot
densityplot(property_df$Date)


library(ggplot2)
#checking the number of houses registered as per year
property_yearwise <- property_df$Date
hist(property_yearwise) 
plot(x=property_df$Price, y=property_df$Date)

#creating new data frame with only year and price
new_df <- data.frame(property_df$Date, property_df$Price)
str(new_df)

#normality test
ks.test(property_df$Date, property_df$Price) #Data size is more than 3 lakhs.
qqnorm(new_df$property_df.Price) #Q-Q plot for price variable
qqline(new_df$property_df.Price, col = 'green')


#pca analysis for price and date data.
data_numeric_variables <- sapply(property_df, is.numeric)
data_numeric_variables #we can see only date and price are displayed as true as I'm having only two numeric values.

data_adj <- property_df[, data_numeric_variables]
#my dataframe consists of only price and date, which are in numeric format
pca <- prcomp(data_adj, center = TRUE, scale. = TRUE)
summary(pca)  #we can see 2 pca are obtained pc1 and pc2.
str(pca) #standard deviation, center and scale can be seen by running the structure
#install.packages("factoextra")
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values #we can see variance of 52.01% and 47.9% for dim1 and dim2 and eigen values are displayed for both the dimensions.

library("FactoMineR")
pca2 <- PCA(new_df, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) #variances are plotted with respect to dimensions.

pca_for_variables <- get_pca_var(pca)
pca_for_variables

library("corrplot")
fviz_cos2(pca, choice = "var", axes = 1,2) #cos2 is used for quality of representation.




fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "blue"), repel =TRUE
) 

library(factoextra)
#contributions to each dimension.
fviz_contrib(pca, choice = "var", axes = 1, top = 100)
fviz_contrib(pca, choice = "var", axes = 2, top = 100)


#spearman model to check the corelation b/w two variables
res <- cor.test(new_df$property_df.Date, new_df$property_df.Price,
                method = "spearman")
res

#install.packages("corrplot")
#Plotting corelation graph.
library(corrplot)
M <- cor(new_df)
corrplot(M, method = "number") #We can clearly see value is 1 for date and 0.04 for price.



#Checking Relation between county and price

#new dataframe is created with only county and price properties
another_df <- data.frame(property_df$County, property_df$Price)
str(another_df) #structure of data frame

summary(another_df)
#displaying counties with the number of houses in ascending order
sort(table(another_df$property_df.County), increasing = TRUE)
 

#plotting the data points
barplot(table(another_df$property_df.County))

plot(x=another_df$property_df.Price, y=another_df$property_df.County) 

#normality test
ks.test(another_df$property_df.County, new_df$property_df.Price)


#install.packages("pwr")
library(pwr)

#calculating the effective size
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

#Considering effective size and alpha as 5% ,Power analysis is calculated. #pwr.t.test for corelation.
power_analysis <-pwr.t.test(d=0.5,n=NULL,sig.level=0.05,  power=0.95, type="one.sample",alternative="two.sided")
power_analysis
#plotting power analysis
plot(power_analysis)

#as the data consists of numerical and factoral data, chisquare test is used.
chisq.test(another_df$property_df.County, new_df$property_df.Price, correct=FALSE)
#based on result, we can clearly see p value is less than 0.05. so H0 is rejected.




