##Andre King
##Exercise 4: Indication, determination, and inference
## DATA 306
## 3/31/2025

getwd()

##Analysis of inspection scores across NYC

##1 ########################################################################################

setwd("~/Desktop/DATA306/") # modify as needed
## My directory
#setwd("/Users/senorking/Desktop/Data306/data/")

insp.full <- read.csv("data/NYC_restaurant_inspections_r1.csv")

#insp.full <- read.csv("/Users/senorking/Desktop/Data306/data/NYC_restaurant_inspections_r1.csv")
insp <- insp.full[ , 11:16] # select columns 11 thru 16
str(insp.full[ , 1:10])

##2########################################################################################


summary(insp[ , c("mean_initial_score", "mean_reinspection_score")])

##3 **********************************************************************************

par(mfrow = c(1, 2))
hist(insp.full$mean_initial_score, 
     main = "Histogram of Initial Scores", 
     xlab = "Mean Initial Score",  # x axis label
     col = "yellow", 
     #breaks = "scott", # more bars
     border = "red")
hist(insp.full$mean_reinspection_score,
	main = "Histogram of Initial Scores", 
     xlab = "Mean Reinspection Score",  # x axis label
     col = "blue", 
     #breaks = "scott", # more bars
     border = "red")
)     

##4########################################################################################


#The distribution of of initial score and reinspection scores of resturant in NYC show several differences. While both charts seems to be skewed to the left, the initial scores have a higher mean (23.26) compared to the reinspection scores mean (14.26). This indicates that when the resturant get reinspected their scores tend to be lower than the first time. 

## The theoretical distribution that might fit the "data generating mechanism" producing these distributions of scores could be the chi-squared distribution distribution.  

##5########################################################################################

library(tidyr)
insp.1 <- pivot_longer(insp, cols = c("mean_initial_score", "mean_reinspection_score"), values_to = "score", names_to = "type")

str(insp) 
str(insp.1)

## The structure for insp is 6771 observation of  6 variables meaning there are 6771 rows and 6 colums while the structure of insp.1 is 13542 rows and 6 colums. the structure of insp.1 has type for the column of initial and reinspection and another column for the score of each. 

##6########################################################################################


#Raw counts table
counts_table <- table(insp.1$BORO, insp.1$year)

counts_with_margins <- addmargins(counts_table)# add margin

proportions_table <- prop.table(counts_table)
rounded_proportions <- round(proportions_table, 4)

print(counts_with_margins)
print(rounded_proportions)

# Plot side-by-side using par
par(mfrow = c(1, 2))  # 1 row, 2 columns for plots

# Plot raw counts table
hist(counts_with_margins, 
     main = "Counts Table", 
     col = "red", 
     xlab = "Year", ylab = "Count")

# Plot proportions table
hist(rounded_proportions, 
     main = "Proportions", 
     col = "blue", 
     xlab = "Year", ylab = "Proportion")

##7########################################################################################


par(mar = c(18, 3, 2, 1)) # adjust plot margins
## c(bottom, left, top, right)
## box plots by score type and year
boxplot(score ~ type + BORO, data = insp.1,
xlab = "", # suppress x-axis label
las = 2) # rotate x-axis tick labels

tapply(insp$mean_initial_score, insp$BORO, mean)
tapply(insp.1$score, insp.1$BORO, mean)
## try different functions and see what happen. eg mean, summary...

##8########################################################################################


#Pr(2020) = 0.0372 or 3.73%
round(504/13542, 4)

#Pr(2022) = 0.5996 or 59.96%
round(8120/13542, 4)

#Pr(Manhattan) = 0.3799 or 37.99%
round(5144/13542, 4)

#Pr(Manhattan|2021) = 0.2383 or 23.83%
round(112/470, 4)

##9 #######################################################################################

# Using a boxplot the initial scores can be shown to be higher than the reinspection score in each borough of NYC. This can be confirmed using taaply and the mean function which shows that each borough has a lower reinspection score. The mean initial score was 23.26 while the mean reinspection score was 14.26, meaning on average there is a 9 point lower score after reinspection. 

#One difficulty in the distribution of inspections across year and borough is understanding how inspections might be done over time. 

##10 ########################################################################################


insp$diffs <- insp$mean_initial_score - insp$mean_reinspection_score
## vector math in action: subtracts every element in mean_initial_score by the corresponding elements in mean_reinspection_score

summary(insp$diffs)

par(mfrow = c(1, 2), mar = c(6, 4, 1, 1)) # c(bottom, left, top, right)
hist(insp$diffs,
	main = "Difference Plot", # suppress the main title using ""
	ylab = "", # ditto for y axis label
	sub = "\nTheoretical density distribution in red",
	adj = 0.5, # left-justify label and title text
	cex.sub = 0.75, # subtitle size
	xlab = "Initial score minus reinspection score",
	breaks = "scott", # more bars
	freq = FALSE, # density histogram
	col = "lightblue")

sstats <- paste("mean = ", round(mean(insp$diffs), 4),
"\nsd = ", round(sd(insp$diffs), 4), 
"\nmedian =", median(insp$diffs)) ## \n makes a new line between mean and sd

text(100, 0.025, sstats)## insert the text at location 100 (x), 0.025 (y) in the hist

## Compute a random normal distribution with the same stats as ours
theoretical_dist <- rnorm(nrow(insp), mean(insp$diffs), sd(insp$diffs))

lines(density(theoretical_dist), col = "red", lwd = 2)## overlay the density of this random distribution on our hist

## include a separate qqnorm plot
qqnorm(insp$diffs, main = "Normal Distribution", col = "blue")
qqline(insp$diffs, col = "red")

 ##########################################################################################


#1: What do you think? Is this distribution approximately normal, or does it look like some other theoretical distribution? Do you think we can assume normality? Write a sentence or two deliberating on this question.

#answer: The distribution of the differences between initial and reinspection scores seems to be approximately normal except for the ends of the distribution. There is an overall normal shape between the difference. We can assume normality since the reason it is not normal is because of the skewness so without it there is a possiblitiy is can be normal

#2
t.test(insp$diffs)
Pr(µc = 0 | x,sd) = 0.00000000000000022
#the output from t.test() indicates that the estimated probability of observing a mean difference of nearly 9 points, given the contemplated mean (µc) is zero, is approximately zero

t.test(insp$diffs, mu = 8.5)
#The p-value is no longer approximately zero. The output above indicates the probability of observing a mean score difference of about nine given the true mean difference is 8.5 is about 0.02 or 2%. So there is a 2% chance of getting a score difference as extreme as 9 given a true mean difference of 8.5.

testobj <- t.test(insp$diffs, mu = 8.5, alternative = "greater")
## alternative as in "alternative hypothesis"
round(testobj$p.value, 2)
#So the probability of observing an average difference of about 9 points, given the "true mean"is 8.5 or less, is about 0.01, or 1%. This last test gives strong evidence (a p-value of 0.01) in favor of rejecting the idea that the true mean is less than 8.5. thus true men above 8.5

#3
wilcox.test(score ~ type, data = insp.1, alternative = "greater",
## use alternative = "two.sided" to get confidence intervals (CI)
conf.int = FALSE) # change to TRUE for CI estimates
#The probability reinspection scores are not lower than initial scores is almost 0 (p≈0).

##########################################################################################
#1
#insp$my.boro <- insp$BORO == "Brooklyn"
## for multiple boroughs, try:
insp$my.boros <- insp$BORO == "Brooklyn" | insp$BORO == "Queens"
## We need to pivot longer again.
## Make sure your boro is defined in insp first!
insp.12 <- pivot_longer(insp, cols = c("mean_initial_score", "mean_reinspection_score"),values_to = "score", names_to = "type")

#2
par(mfrow = c(1, 2))
hist(insp$diff[insp$my.boro == TRUE], main = "Difference Distribution", 	breaks = "scott", # more bars
col = "yellow", )
qqnorm(insp$diff[insp$my.boro == TRUE], col = "blue")

#The distribution of the differences between initial and reinspection scores of Brooklyn and Queens seems to be a bell curve with the majority of the score in the middle with some outliers at the ends of the distribution. There is an overall normal shape between the difference but it exceeds the normal distribution. The is no skewness. the histogram seem to be very similar to the previous histogram.

#3
insp$diff <- insp$mean_initial_score - insp$mean_reinspection_score

# Create a new factor variable: "My Borough" vs "Other"
insp$otherboro <- ifelse(insp$my.boro == TRUE, "My Borough", "Other")

# Create the boxplots
boxplot(diff ~ otherboro, data = insp,
        main = "Score Differences: My Borough vs Others",
        xlab = "Borough Group",
        ylab = "Initial - Reinspection Score",
        col = c("blue", "green"))

#4
t.test(insp$diff, mu = 0)

#the p-value is so this indicates that the probability that Brooklyn and Queens difference of the mean of 8.99 is very small approximately zero. the true mean difference is not zero.

#5

wilcox.test(diffs ~ type, data = insp.12, subset = (my.boros == TRUE), conf.int = TRUE)
## subset selects rows defined
### on the right-hand side
#### of the equals sign.

#The Wilcoxon Test with shows that there is no significant difference in the distributions of initial and reinspection scores within your borough. The p-value of 1 shows we fail to reject the idea that their distributions are the same.\