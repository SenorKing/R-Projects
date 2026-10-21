##Andre King
##Exercise 5: Data analysis and modeling of group-level characteristics
## DATA 306


getwd()
#setwd("~/Desktop/DATA306/") # modify as needed
setwd("/Users/senorking/Desktop/Data306/data/")#my directory

#1
#A possible source of error is taking the first batch of water when it was not stagnant for 6 hours. Also some neighbourhood is likely to participate more than others. A neighbour that is wealthier will likely participate more since wealthier families are more informed and will care more. 
#I think neighborhoods who participate in the program will likely have similar amount of lead as others but will think they have more since more people in that community will find lead in the water while other neighborhood will not be aware. 

pb.cu <- read.csv("/Users/senorking/Desktop/Data306/data/NYC_pb_cu_residentialTap.csv")
str(pb.cu)

pb.cu$Date.Collected <- as.Date(pb.cu$Date.Collected, format = "%Y-%m-%d") 
pb.cu$Date.Received <- as.Date(pb.cu$Date.Received, format = "%Y-%m-%d")

pb.cu$Date.Collected[1]; pb.cu$Date.Collected[1] + 365

install.packages("lubridate")
library(lubridate)

x <- pb.cu$Date.Collected[sample(1:nrow(pb.cu), 1)] # a random sample; rerun for more examples
x; year(x); month(x); day(x); quarter(x)

library(dplyr)
pb <- select(pb.cu, !starts_with("Copper")) ## ! means "everything but"

zipcounts <- pb %>% count(Zipcode) ## str(zipcounts) # if you want to see the resulting 191 x 2 data.frame
summary(zipcounts$n); stem(zipcounts$n)

zipcounts[which.max(zipcounts$n), ]

table(year(pb$Date.Collected))

#2
table(pb$borough)

#3
The geographic distribution of samples shows that Brooklyn (11524) and Manhattan  (8691) contributed the most, while Bronx (1901) and Staten Island (987) contributed the least. This may be becaue of the population of these area and the people of these neighborhood are more aware compared to bronx and staten island. 

#4
table(month(pb$Date.Collected, label = TRUE))

#5
#There seems to be a seasonal variation in tap water sampling among New Yorkers, The first four months of the year is much heigher than the rest of the year with the peak month being march. The summer months seems to be much lowers as well with july being the second lowest month. 

#6
spacetime.tab <- table(year(pb$Date.Collected), pb$borough)
addmargins(spacetime.tab)

#7 
# Pr(2020|Queens)= 0.1276
870/6820

#8
Pr(2020) = 0.1127
3372 / 29923

#9 
I think it is dependent because the probability a measurement is from 2020 is around 0.11 average and this average is similar in all borough in the year 2020. However there seems to be a slight independence since some boroughs have a little different measurements. 

#10
chisq.test(spacetime.tab)

# The earlier idea was correct since the p-value is extremely small so you can confidently reject the idea that borough and year are independent so this means there is a statistically significant association between the borough where a sample was collected and the year it was collected.

## lay out the plots in the graphics window
layout(matrix(c(1, 1, 1, 1, 1, # the first plot
	2, 3, 4, 5, 6), # all the others
	2, 5, byrow = TRUE))
par(mar = c(6, 4, 2, 1), bty = "n")

hist(pb$Date.Collected, breaks = "month", # try "quarter" or "year" ## the first plot
	freq = TRUE,
	las = 2,
	lwd. = 1,
	cex.axis = 0.75,
	cex.main = 1.5,
	ylab = "Number of Samples per Month",
	xlab = "",
	main = "\nNew York City, 2014 \u2013 2022") ## \u2013 is unicode for en dash

## The for loop
blevels <- levels(as.factor(pb$borough))## initializing blevels
### start loop
for(boro in blevels){
hist(pb$Date.Collected[pb$borough == boro],
	breaks = "month",
	freq = FALSE,
	lwd. = "asd",
	las = 2,
	ylab = "", xlab = "", main = boro,
	ylim = c(0, 0.0018))
}

#12
#The top plot shows that tap water sampling across NYC peaked in 2016 and remained relatively high through 2022, and has increases in sampling spikes in warmer months. The sampling density of each borough seems to be similar with most density around 0.005 across all borough and with ocasional spikes 

#13
#Brooklyn and Manhattan contribute the most to our understanding of lead content in NYC tap water as shown by their consistently high sampling numbers. Brooklyn alone had for over 11,000 samples and shows sustained sampling activity across multiple years. Manhattan follows closely with similarly high participation. Boroughs like the Bronx, Queens, and especially Staten Island contribute fewer samples and display more irregular or seasonally clustered activity. This suggests that our knowledge about lead exposure is strongest in areas with high sample contributions, while lower-participating boroughs may be underrepresented in the data, potentially masking localized issues such as higher lead in underrepresented areas. 

quantile(pb$Lead.First_draw, p = 0.9, na.rm = TRUE)#The 90th percentile for the whole city is 0.004 mg of lead per liter of water

nineties.zc <- tapply(pb$Lead.First_draw, # apply this measure...
	pb$Zipcode, # across these groups...
	quantile, # to this function.
	p = 0.9, # quantile() argument
	na.rm = TRUE # quantile() argument
)
summary(nineties.zc); stem(nineties.zc)

nineties.zc[which.max(nineties.zc)]

nineties.boro <- tapply(
	pb$Lead.First_draw,   # lead measurements
  	pb$borough,           # group by borough
  	quantile,             # apply quantile function
  	p = 0.9,              # 90th percentile
  	na.rm = TRUE          # ignore NAs
)
nineties.boro

#Interpret the 90th percentile lead concentrations by borough. How do they compare with the same statistic for whole city? For zip codes?

#The 90th percentile lead levels differs across boroughs, with the Bronx showing the highest value, exceeding the citywide 90th percentile of 0.004 mg/L. This shows a higher lead exposure in some borough. Manhattan and Staten Island have lower 90th percentile values, meaning fewer high-lead samples. Compared to ZIP codes, the borough lead values are more stable, while ZIP codes analysis show more variation, this shows that lead contamination can vare greatly even within a single borough.

# First draw
exceedances_firstDraw <- sum(pb$Lead.First_draw >= 0.015, na.rm = TRUE)
total_firstDraw <- length(na.omit(pb$Lead.First_draw))
exceedances_firstDraw/total_firstDraw

#Second draw
exceedances_thirdDraw <- sum(pb$Lead.5_min_flush >= 0.015, na.rm = TRUE)
total_thirdDraw <- length(na.omit(pb$Lead.5_min_flush))
exceedances_thirdDraw/total_thirdDraw

# for loop
crude.rates <- NA # initialize a vector to store the loop’s output
names <- c("Lead.First_draw", "Lead.1_2_min_flush", "Lead.5_min_flush")
for(i in 1:3){
	crude.rates[i] <- sum(pb[ , names[i]] >= 0.015, na.rm = TRUE) /
	length(na.omit(pb[ , names[i]]))
}
## tidy up the output vector by labeling with names
names(crude.rates) <- names
crude.rates

library(tidyr)
pb.l <- pivot_longer(pb,
	cols = starts_with("Lead"),
	values_to = "lead_conc",
	names_to = "sample")

nrow(pb)*3 == nrow(pb.l) #gut check
str
(pb.l )

pb.l$sample <- factor(pb.l$sample,
	levels = c("Lead.First_draw",
		"Lead.1_2_min_flush",
		"Lead.5_min_flush"))

pb.l$lead_exceedance <- pb.l$lead_conc >= 0.015
summary(pb.l$lead_exceedance)

# Three-way Table
(leadtab <- table(pb.l$borough, pb.l$sample, pb.l$lead_exceedance))
#The first table has counts of all samples that did not meet or exceed the limit value of 0.015 mg/liter. The second table has counts of samples that did meet or exceed the limit


## leadtab[ , , 1] is the FALSE table above,
## leadtab[ , , 2] is the TRUE table
exposed <- leadtab[ , , 1] + leadtab[ , , 2]
k <- leadtab[ , , 2]
(rates <- round(k/exposed, 4))


##16

#a: Why are exceedance rates so much higher in the 5-minute flush samples?
#the ex eedance rates is much higher in the 5 minutes flush sample because the lead is in the entire water supply and flushing the water for a few minutes will not help in reducing the lead to a high extent. 

#b: Write about what you think is going on in Queens. What more do you want to know about lead contamination in Queens tap water? Is there something different about Queens that explains the similar (if not higher) value in the 1-2 minute flush exceedance rate?

## The fact that 1–2 minute flush samples in Queens show no improvement and even slightly worse lead exceedance rates suggests the lead problem originates outside of the household plumbing and into the main pipes of the entire neighborhood. 

#17
#This table complicate our evaluation of the efficacy of flushing because it shows that flushing the water for 1–2 minutes does not consistently reduce lead levels across NYC. In some areas like Queens, it may not help at all so flushing is not the best solution.

# rates comparison
rates[ , 1]/rates[ , 2]

std.rates <- rowSums(k)/rowSums(exposed)
ref.rates <- NA
for(i in 1:3){
ref.rates[i] <- sum(std.rates*exposed[1:5, i]/sum(exposed[1:5, i]))
}
ref.rates

crude.rates/ref.rates

#18
#The evidence suggests that flushing taps for 1 to 2 minutes does reduce lead exposure, as exceedance rates are lower than expected after a short flush. However, flushing for 5 minutes appears to increase lead exceedances, maybe as a result of taking in lead from main lines. The conclusion is that DEP’s recommendation to flush briefly is effective but flushing longer than 5 minutes may actually worsen lead concentration.
