#################################################
###       Biostats 1 VSC113                   ###
#################################################

BeefAge_data = c(2, 3, 4, 4, 5, 3, 3, 3, 2, 3)

mean(BeefAge_data)



### Types of variables --------------------------------------------
# Rectal temperature  = quantitative, continuous, interval
# Semen motility score = qualitative, ordinal
# Litter size = quantitative, discrete
# Coat colour = qualitative, nominal
# Serum CK = quantitative, continuous, ratio 
# Lameness score = qualitative, ordinal



### Sample variation ----------------------------------------------
EggsWeek = c(4, 3, 7, 6)

sample(EggsWeek, 2, replace=TRUE)


#Make this into a loop and store the sample data---------------- you don't need to understand this code
Results = list()
Means = c()

set.seed(1)

for (i in 1:10) {
 res = sample(EggsWeek, 2, replace=TRUE)
 meanRes = mean(res)
 Results = append(Results, list(res))
 Means = c(Means, meanRes)
}

hist(Means, col = 'red', main = "", xlim = c(0, 8))
abline(v = mean(Means), lty = 2, lwd = 3)

###--------------------------------------------------------------
library(readr)

urlfile="https://raw.githubusercontent.com/vbrookes/Biostats_VSC113/main/Dairy2.csv"
CowData<-read.csv(urlfile)

str(CowData)
summary(CowData)

# Change qualitative variables to 'factor'
CowData$breed = as.factor(CowData$breed)
CowData$parity = as.factor(CowData$parity)
CowData$cowid = as.factor(CowData$cowid)
CowData$InCalf = as.factor(CowData$InCalf)

str(CowData)
summary(CowData)

## Visualizing Data --------------------------------------------------------
## Frequency tables for Qualitative data------------------------------------

table(CowData$breed) #table of counts in each category
prop.table(table(CowData$breed)) #table of percentage in each category

# Now, make a table of counts and proportions for parity and pregnancy 
table(CowData$parity)
prop.table(table(CowData$parity))

table(CowData$InCalf)
prop.table(table(CowData$InCalf))

## Plots for Qualitative data------------------------------------
barplot(table(CowData$breed), xlab = 'Breed', ylab = 'Frequency', ylim = c(0, 600))

# Now, make barplots for parity and pregnancy











barplot(table(CowData$parity), xlab = 'Parity', ylab = 'Frequency', col = 'green', ylim = c(0, 500))
barplot(table(CowData$InCalf), xlab = 'Pregnant', ylab = 'Frequency', col = 'red', names = c('No', 'Yes'))

pie(table(CowData$InCalf), main="")

# Frequency tables and plots for 2 variables (2x2 tables; Contingency tables)---------------------------------------
# Let's investigate breed and pregnancy together
table(CowData$breed, CowData$InCalf) #table of counts in each category
prop.table(table(CowData$breed, CowData$InCalf), 1) # adding '1' means that the proportions are calculated by row

#Barplot side by side
barplot(table(CowData$InCalf, CowData$breed), beside = T, xlab = 'Parity', ylab = 'Frequency', col = c('orange2', 'blue2'))
legend("topright", legend= c('no', 'Yes'), title="Pregnant", fill = c('orange2', 'blue2'))

#Plots for Quantitative data ------------------------------------------

hist(CowData$kgmilk, main = '', xlab = 'Milk (kg)', col = 'pink1', ylim = c(0, 400))

# Now make a histogram of cell count








hist(CowData$cellcount, main = '', xlab = 'Cell count', col = 'yellow1', ylim = c(0, 1000))


## Boxplots - useful for data that is not Normally distributed
boxplot(CowData$cellcount, main = '', xlab = '', ylab = 'Cell count (cells/ml)', col = 'yellow1')
boxplot(CowData$cellcount ~ CowData$parity, main = '', xlab = 'Parity', ylab = 'Cell count', col = 'yellow2') # Very useful for comparing categories

#See if you can plot boxplots of milk (kg) by parity








boxplot(CowData$kgmilk ~ CowData$parity, main = '', xlab = 'Parity', ylab = 'Milk (kg)', col = 'pink1') # Very useful for comparing categories


## Scatter plots if 2 quantitative variables
plot(CowData$cellcount ~ CowData$kgmilk, ylab= 'Cell count', xlab = 'Milk (kg)')

## Describing natural variation using summary statistics-------------------------------------------------------
## Measures of central tendency--------------------------------------------------------------------------------

summary(CowData)
str(CowData)

## Ordinal data
median(CowData$parity, na.rm = T)

CowData$parityNum = as.numeric(CowData$parity)
median(CowData$parityNum, na.rm = T)

summary(CowData$parityNum)

barplot(table(CowData$parity), xlab = 'Parity', ylab = 'Frequency', col = 'green', ylim = c(0, 500))
abline(v = median(CowData$parityNum, na.rm = T), lwd = 3)
abline(v = mean(CowData$parityNum, na.rm = T), lwd = 3, col = 'red')

## Quantitative symmetric data-------------------

summary(CowData$kgmilk)

quantile(CowData$kgmilk, c(0.025, 0.975))

sd(CowData$kgmilk)
var(CowData$kgmilk)

hist(CowData$kgmilk, main = '', xlab = 'Cell count', col = 'pink1', ylim = c(0,400))
abline(v = median(CowData$kgmilk, na.rm = T), lwd = 3)
abline(v = mean(CowData$kgmilk, na.rm = T), lwd = 3, col = 'red')
abline(v = quantile(CowData$kgmilk, c(0.025, 0.975)), lwd = 3, col = 'blue2')

library(epiR)
epi.descriptives(CowData$kgmilk)

## Quantitative asymmetric data-------------------

summary(CowData$cellcount); quantile(CowData$cellcount, c(0.025, 0.975))

hist(CowData$cellcount, main = '', xlab = 'Cells/ml', col = 'yellow1', ylim = c(0, 1000))
abline(v = median(CowData$cellcount, na.rm = T), lwd = 3)
abline(v = mean(CowData$cellcount, na.rm = T), lwd = 3, col = 'red')
abline(v = quantile(CowData$cellcount, c(0.025, 0.975)), lwd = 3, col = 'blue2')


### What if I don't have a dataset and I just want to describe a list of numbers?-------------------------------------------
#For example, my list:
#  2.3, 5.6, 3.7, 9.2, 10, 1, 6.7, 2.3, 4.5

# Name your list!

MyList = c(2.3, 5.6, 3.7, 9.2, 10, 1, 6.7, 2.3, 4.5)

summary(MyList)
hist(MyList)
mean(MyList)
median(MyList)
sd(MyList)
var(MyList)
quantile(MyList, c(0.025, 0.975))
epi.descriptives(MyList)

### Sampling variation -------------------------------------------

# Let's make a big list of numbers

BigList = runif(10000, 0, 1000)
BigList
hist(BigList, col = 'orange1', main = '', xlab = 'Dragon wingspan (m)')

# What is the mean of the big list of numbers?
# The true mean (i.e. the population parameter) is:
  mean(BigList)
  sd(BigList)

# What if we have to take a sample from BigList to estimate the mean (i.e. statistic)?
# If we take a sample of 5 numbers (we don't have much time or money)...
set.seed(1)
mean(sample(BigList, 5, replace=TRUE))  # keep clicking on this line and watch how the mean varies
hist(sample(BigList, 5, replace=TRUE), col = 'orange1', main = '', xlab = 'Dragon wingspan (m)', breaks = 5, xlim = c(0, 1000), ylim = c(0, 15))

epi.descriptives(sample(BigList, 5, replace=TRUE))
sd(sample(BigList, 5, replace=TRUE), na.rm = T)/sqrt(5)
# If we take a sample of 50 numbers (we now have a bit more time and money)...

mean(sample(BigList, 50, replace=TRUE))  # keep clicking on this line and watch how the mean varies
hist(sample(BigList, 50, replace=TRUE), col = 'orange1', main = '', xlab = 'Dragon wingspan (m)', breaks = 5,  xlim = c(0, 1000), ylim = c(0, 15))

epi.descriptives(sample(BigList, 50, replace=TRUE))
sd(sample(BigList, 50, replace=TRUE), na.rm = T)/sqrt(50)

## To see this effect a bit more closely, lets re-sample 1000s of times 
# and plot all the means that we collect for sample of 5 and samples of 50



# means from 5 samples ---------------- you don't need to understand this code
Means5 = c()

set.seed(1)

for (i in 1:1000) {
  res = sample(BigList, 5, replace=TRUE)
  meanRes = mean(res)
  Means5 = c(Means5, meanRes)
}

par(mfrow = c(1,2))
hist(Means5, col = 'orange2', main = "Sampling distribution of the mean (Sample size = 5)", xlab = 'Mean (m)', xlim = c(0,1000))
abline(v = mean(Means5), col = 'blue', lwd = 3)
abline(v = quantile(Means5, c(0.025, 0.975)), lty = 2, lwd = 3)


Means50 = c()

set.seed(1)

for (i in 1:1000) {
  res = sample(BigList, 50, replace=TRUE)
  meanRes = mean(res)
  Means50 = c(Means50, meanRes)
}

hist(Means50, col = 'orange2', main = "Sampling distribution of the mean (Sample size = 50)", xlab = 'Mean (m)', xlim = c(0,1000))
abline(v = mean(Means50), col = 'blue', lwd = 3)
abline(v = quantile(Means50, c(0.025, 0.975)), lty = 2, lwd = 3)

# Standard error of the mean ------------------------------------------

# An estimate of precision of the mean and reflects the sample size
sd(CowData$kgmilk, na.rm = T)/sqrt(length(CowData$kgmilk))











