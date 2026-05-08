##Andre King 
## Tabular Demo

nv <- read.csv("/Users/senorking/Desktop/Data306/data/Tabulation demo.csv", strip.white = TRUE)
nv
str(nameviz)

par(mfrow = c(3, 1))# 2 row 1 col
hist(nv$Regular) 
hist(nv$Crooked)
qqplot(nv$Regular, nv$Crooked)

#tapply means table apply 
rollstab<- tapply(nv$Regular, nv$Name, sum)
rollstab

crookedstab<- tapply(nv$Crooked, nv$Name, sum)
# sum can be change to any other calculations. eg. mean, standard deviation

# Sister function is Array2DF()
rollsdf <- array2DF(rollstab)
rollscrooked <- array2DF(crookedstab)

str(rollsdf)#expose structure
str(rollscrooked)#expose structure

rollsdf
rollscrooked

#changing column names 
ccolnames(rollsdf) <- c("Names", "Regular")
colnames(rollscrooked) <- c("Names", "Crooked")

#sample(1:6, 20, replace = TRUE)
#sum(sample(1:6, 20, replace = TRUE))# sum 20 rolls
#rollsums <- sum(sample(1:6, 20, replace = TRUE))# store sum of 20 rolls in vector

rollsums <- NA
for(roll in c(1:1000000)){
	rollsums[roll] <- sum(sample(1:6, 20, replace = TRUE))
}
str(rollsums)
hist(rollsums)
