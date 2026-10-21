##Andre King
## DATA 306
##Exercise 8: From bivariate to multivariate linear modeling


#***************************************************************************#
###Preparatory research###
#***************************************************************************#

#1. What is the job title of the "Notice of Exam" document(s) you looked 
## Locksmith (NYC H+H)

#2. What is the minimum score for passing the test?
## A score of 70 is needed for passing the

#3. Give an example of one kind of "knowledge," "ability," or "experience" the exam you selected is intended to measure.
# maintenance and repair of locks and locking devices

#4. Irrespective of the candidate’s "true" knowledge/ability/experience, is there anything about the way exams are administered that you expect might change scores or access to exams for some people but not others? Why or why not?
#The use of OASys online submission system can disqualify qualified candidates who lack internet access, or is not computer literate. Applicants who are older or people with disabilities may struggle to complete or submit the online exam by the deadline.

#***************************************************************************#
###Data preparation###
#***************************************************************************#

##5
job <- read.csv("/Users/senorking/Desktop/Data306/data/NYC_civil_service_jobs2023.csv")
str(job)

##6
job$deadline <- as.Date(job$deadline)
str(job$deadline)

#we move the List.Title.Desc column to the dataframe’s rownames()
rownames(job) <- job$List.Title.Desc # assign row names
job$List.Title.Desc <- NULL # CAUTION! This deletes the column

#***************************************************************************#
###Exploring the score variables###
#***************************************************************************#

# from the pairs documentation page:
#This function adds histograms to the diagonal panels of a scatterplot matrix
panel.hist <- function(x, ...){ usr <- par("usr")
par(usr = c(usr[1:2], 0, 1.5) )
h <- hist(x, plot = FALSE)
breaks <- h$breaks; nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col = "lightblue", ...)
}

#This function displays correlation coefficients in the upper or lower panels of the pairs() plot
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use = "complete.obs"))
txt <- format(c(r, 0.123456789), digits = digits)[1]
txt <- paste0(prefix, txt)
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex.cor * r)
}

#***************************************************************************#

pairs(job[ , c(2:5)],
lower.panel = panel.smooth,
upper.panel = panel.cor,
# ^^custom function from above
diag.panel = panel.hist)
# ^^custom function from above

#***************************************************************************#
###The coefficient of variation, mean, and median in bounded distributions###
#***************************************************************************#

##7
#The relationship between CV and both mean and median scores forms an inverted U-shape. since the CV is 92 it shows that there is more inequality and that the mean and median are between 80 and 90, since the values are approximately 95 for median and mean.

##8
#Excluding jobs with a mean of 100 and a CV of 0 removes cases where all candidates received perfect scores, this might be good because having all perfect score might mean the exam is very easy. this will allow for analysis of exam scores where there is a challenge that shows if they actually qualified. However it may overlook very good candidates

#***************************************************************************#
###Find the percentage of veterans and of candidates with family members who died in 9/11###
#***************************************************************************#

job$pct_family_911 <- with(job, family_911/candidates)*100
# job$pct_family_911 <- (job$family_911/job$candidates)*100 # equivalent to above

#9
job$pct_veteran <- with(job, veterans/candidates)*100
str(job)

#***************************************************************************#
###Summarize the predictor variables###
#***************************************************************************#

#10
summary(job[ , !names(job) %in% c("mean_score", "median_score", "CV_score", "lower_10_score")])
#extracts only the columns named  
job_ext <- summary(job[ , !names(job) %in% c("mean_score", "median_score", "CV_score", "lower_10_score")])
job_ext #this summarize the remaining variables

#11
The variable hierarchy_code seems to vary the least because the median and the mean is very similar and the difference between the minimum and maximun is very small, additionally the 1st and 3rd quartile is the same.

#12 
#Yes we should be concerned about how well some of these variables represent the collection of jobs because variables that barely vary or only apply to a small subset don’t fully represent jobs. Also there is a number of NA's which might mean there is some mistakes made by candidates that may affect their job results.

#***************************************************************************#
###Linear modeling with categorical predictors###
#***************************************************************************#

reg1 <- lm(mean_score ~ pct_family_911, data = job)
summary(reg1)

plot(mean_score ~ pct_family_911,
xlim = c(-1, 6), # so we can see the labels
col = as.factor(hierarchy_code),
data = job)
abline(reg1, col = "red")
text(mean_score ~ pct_family_911,
labels = rownames(job),
data = job,
subset = pct_family_911 > 1 | mean_score < 75)
# subset so we are not swamped with labels
legend("topright", c("Upper management", "Management",
"Staff", "Junior Staff"),
pch = 1, col = 1:4)

reg2 <- lm(mean_score ~ pct_family_911 + as.factor(hierarchy_code), data = job)
summary(reg2)

coefs <- coef(reg2) # saves our coefficients in coefs
cbind(round(coefs, 3), 1:5)
# result
#[,1] [,2]
#(Intercept) 96.744 1
#pct_family_911 -3.690 2
#as.factor(hierarchy_code)2 -4.009 3
#as.factor(hierarchy_code)3 -5.983 4
#as.factor(hierarchy_code)4 -7.006 5


##Plot regression lines
# replace the first line below with a jittered plot to see more points (many are overplotted):
plot(mean_score ~ jitter(pct_family_911, 5000),
#plot(mean_score ~ pct_family_911, xlim = c(0, 6),
col = as.factor(hierarchy_code), # ditto
data = job)
abline(a = coefs[1], b = coefs[2], col = 1)
# the reported intercept is the intercept for the top-level supervisors group (it is the "reference group" and does not need to change)

abline(a = coefs[1] + coefs[3], b = coefs[2], col = 2)
# it is as simple as adding the coefficient for hierarchy code 2 to the intercept!
abline(a = coefs[1] + coefs[4], b = coefs[2], col = 3)
# same thing for hierarchy code 3
abline(a = coefs[1] + coefs[5], b = coefs[2], col = 4)
# same thing for hierarchy code 4
legend("topright", c("Upper management", "Management", "Staff", "Junior Staff"),
pch = 1, lwd = 1, col = 1:4)
#


tapply(job$mean_score, job$hierarchy_code, mean)

reg_cats <- lm(mean_score ~ as.factor(hierarchy_code), data = job)
coefs <- coef(reg_cats)
coefs

#***************************************************************************#
###The woes of regression coefficients###
#***************************************************************************#

#13
reg3 <- lm(mean_score ~ pct_veteran, data = job)
reg3

#14 Write a one-sentence interpretation of the coefficient for pct_veteran.
#With every percentage point increase in veterans, a job’s mean score is expected to decreases by approximately 1.89 points

#15 
##Plot regression lines of pct_veteran.

#plot(mean_score ~ pct_family_911, xlim = c(0, 6),
plot(mean_score ~ jitter(pct_veteran, 5000),
col = as.factor(hierarchy_code),  data = job)
abline(a = reg3[1], b = reg3[2], col = "red")
with(subset(job, pct_veteran > 4), text(pct_veteran, mean_score, labels = List.Title.Code, pos = 3, cex = 0.7))

#16
reg4 <- lm(mean_score ~ pct_veteran + pct_family_911 + as.factor(hierarchy_code), data = job)
reg4

#17
This change shows that the relationship between veteran representation and mean score is still negative, but slightly weaker once pct_family_911 is added

table(job$mean_score == 100, job$CV_score == 0)

job.sub <- job[job$CV_score > 0 & job$mean_score < 100, ]

reg_noouts <- lm(mean_score ~ pct_veteran, data = job.sub)
summary(reg_noouts)

#
plot(mean_score ~ pct_veteran,
xlim = c(-1, 6), # so we can see the labels
col = as.factor(hierarchy_code), data = job.sub)
abline(reg_noouts, col = "red")
text(mean_score ~ pct_veteran,
labels = rownames(job), data = job,
subset = pct_veteran > 4 | mean_score < 75)
legend("topright", c("Upper management", "Management",
"Staff", "Junior Staff"),
pch = 1, col = 1:4)
#

18. Modify the code block above so that it reflects the model you constructed in question 16.

# Scatterplot: mean_score vs pct_veteran (still on x-axis for clarity)
plot(mean_score ~ pct_veteran + pct_family_911,
xlim = c(-1, 6),  # extend x-axis
col = as.factor(hierarchy_code),
data = job,
main = "Mean Score vs. Veteran and 911 Family")

abline(reg4, col = "red")
	
text(mean_score ~ pct_veteran,
labels = rownames(job), # Label extreme points
data = job, subset = pct_veteran > 4 | mean_score < 75)
legend("topright",  c("Upper management", "Management", "Staff", "Junior Staff"),
pch = 1, col = 1:4)
	
#19
# The regression had a flat regression line but after removing the anchor the line became a decreasing line showing a negative relationship. 

#***************************************************************************#
###Post script: the consequences of limited variation in the predictor###
#***************************************************************************#


# adding random variation to the pct_family_911 variable
experiment <- job$pct_family_911 + rpois(nrow(job),
0.5 * mean(job$pct_family_911))
# the experimental regression
reg.experiment <- lm(job$mean_score ~ experiment,
data = job,
sub = job$mean_score < 100 & experiment < 2)
# the plot
plot(job$mean_score[experiment < 2] ~ experiment[experiment < 2])
abline(reg.experiment, col = "red")














