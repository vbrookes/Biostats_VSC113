#################################################
###       Biostats 1 VSC113                   ###
#################################################

# libraries that you need:
library(epiR)

# Reminder ## Types of variables --------------------------------------------
# Rectal temperature  = quantitative, continuous, interval
# Semen motility score = qualitative, ordinal
# Litter size = quantitative, discrete
# Coat colour = qualitative, nominal
# Serum CK = quantitative, continuous, ratio 
# Lameness score = qualitative, ordinal



### Data creation
set.seed(1)

## ALP in cats. Ref range i 5-50 U/L and Enalor was 33 U/L
# Create 'high cats'
ALP_levels = as.data.frame(rnorm(125, 45, 10))
colnames(ALP_levels) = c('ALP_level')
hist(ALP_levels$ALP_level)
epi.descriptives(ALP_levels$ALP_level)
write.csv(ALP_levels, 'D:/Users/vbrookes/OneDrive - Charles Sturt University/CSU_Teaching/VSC226/ALP_cats.csv')


## time recovery with sevo and iso
options(scipen=999)
Iso = rlnorm(50, 2, 1)
Iso = as.data.frame(cbind(Iso, cat = 'Iso'))
colnames(Iso) = c('time', 'agent')
Sev = rlnorm(50, 1, 1)
Sev = as.data.frame(cbind(Sev, cat = 'Sev'))
colnames(Sev) = c('time', 'agent')
Anaes_data = as.data.frame(rbind(Sev, Iso))
write.csv(Anaes_data, 'D:/Users/vbrookes/OneDrive - Charles Sturt University/CSU_Teaching/VSC226/Anaes_data.csv')


#### 1-SAMPLE TESTS ------------------------------------------------------------------------------------

### ------------------------ Compare sample MEAN to population level -------------------------------------------------------------

# Use the 'ALP in cats' example
# First, load your data (you need to download the data from I2 to a location on your computer)

ALP_levels = read.csv('D:/Users/vbrookes/OneDrive - Charles Sturt University/CSU_Teaching/VSC226/ALP_cats.csv')

# Visualise and describe your data:
hist(ALP_levels$ALP_level, main = '', xlab = 'ALP (U/L)', col = 'blue1')
epi.descriptives(ALP_levels$ALP_level)

# ---- Step 1: State your hypotheses
# In cats in my practice, is the mean ALP level higher than cats in general?
# H0 = there is no difference in the mean ALP level between the cats in the practice and cats in the broader population.
# Ha: there is a difference in the in the mean ALP level between the cats in the practice and cats in the broader population (ALP higher in cats in practice).


# ---- Step 2: 
## Check whether the sample values are Normally distributed----------------------------
# Visual checks
hist(ALP_levels$ALP_level, breaks = 'fd') # Should be a Normal shape
qqnorm(ALP_levels$ALP_level) # Should be a straight 45 degree line
qqline(ALP_levels$ALP_level) # Adds a line to check further

# Skewness (wonky tail), kurtosis (too pointy or too flat)
# For both, if n<200 then the values need to be < +/-1.96, and if n>=200 < +/-2.58
Shape = epi.descriptives(ALP_levels$ALP_level) # collect the values that you need in an object called Shape

Shape$symmetry$skewness/(Shape$arithmetic$sd/Shape$arithmetic$n) # Check the skewness
 
Shape$symmetry$kurtosis/(Shape$arithmetic$sd/Shape$arithmetic$n) # Check the kurtosis

# Kolmogorv-Smirnoff test n>50, Shapiro-Wilk test if n<=50
# H0 is Normality, so we our data are considered consistent with Normality (the null) if P > 0.05
ks.test(jitter(ALP_levels$ALP_level), "pnorm", mean=mean(ALP_levels$ALP_level), sd=sd(ALP_levels$ALP_level))
shapiro.test(ALP_levels$ALP_level)

## The data are consistent with a Normal distribution. Therefore, we select a 1-sample t-test.

# ---- Step 3: 
### State significance level:
# Alpha = 0.05

# ---- Step 4: 
### Conduct test:
t.test(ALP_levels$ALP_level, mu =33)

# Step 5:
### State whether data are consistent with the null hypothesis

# No they are not.
# The P value is < alpha and the 95% confidence interval does not contain the population mean.
# Either:
# There is a very small chance that this sample is from a similar population to the general population
# Or, and we suspect more likely, the sample population has a higher mean ALP than the general population.
# I wonder why?
# Note that statistics doesn't tell you why... you need to apply your knowledge of the context to figure that out.


### ------------------------ Compare sample MEDIAN to population level -------------------------------------------------------------

# What if the data were not Normally distributed (i.e. were not parametric)?
# Then we compare the median nof the sample to a population median, using a non-parametric test!
# Use the 'Dairy2' data
# First, load your data (you need to download the data from I2 to a location on your computer)

Dairy2 = read.csv('D:/Users/vbrookes/OneDrive - Charles Sturt University/CSU_Teaching/VSC226/Dairy2.csv')
summary(Dairy2)
str(Dairy2)

# let's re-code breed and parity to factors (because the are categorical data and 'factor' is the term for this in R)
# We don't really need to do this for this example, but it's useful to know how to do this in R.
Dairy2$breed = as.factor(Dairy2$breed)
Dairy2$parity = as.factor(Dairy2$parity)

# We are interested in the cell count data. So... 

# Visualise and describe your data:
hist(Dairy2$cellcount, main = '', xlab = 'ALP (U/L)', col = 'blue1') # It's a bit skewed!
boxplot(Dairy2$cellcount, ylab = 'ALP (U/L)', col = 'yellow')
epi.descriptives(Dairy2$cellcount)

# ---- Step 1: State your hypotheses
# In cows on this farm, the somatic cell count is higher than cows in general
# H0 = there is no difference in the somatic cell count between the cows on this farm and cows in the broader population.
# Ha: there is a difference in the somatic cell count between the cows on this farm and cows in the broader population.

# ---- Step 2: 
## Check whether the sample values are Normally distributed----------------------------
# Visual checks
hist(Dairy2$cellcount, breaks = 'fd') # Should be a Normal shape
qqnorm(Dairy2$cellcount) # Should be a straight 45 degree line
qqline(Dairy2$cellcount) # Adds a line to check further

# Skewness (wonky tail), kurtosis (too pointy or too flat)
# For both, if n<200 then the values need to be < +/-1.96, and if n>=200 < +/-2.58
Shape = epi.descriptives(Dairy2$cellcount) # collect the values that you need in an object called Shape

Shape$symmetry$skewness/(Shape$arithmetic$sd/Shape$arithmetic$n) # Check the skewness

Shape$symmetry$kurtosis/(Shape$arithmetic$sd/Shape$arithmetic$n) # Check the kurtosis

# Kolmogorv-Smirnoff test n>50, Shapiro-Wilk test if n<=50
# H0 is Normality, so we our data are considered consistent with Normality (the null) if P > 0.05
ks.test(jitter(Dairy2$cellcount), "pnorm", mean=mean(Dairy2$cellcount), sd=sd(Dairy2$cellcount))
shapiro.test(Dairy2$cellcount)

## The data are NOT consistent with a Normal distribution. Therefore, we select a 1-sample Wilcoxon test.
# Assumptions:
# observations are independent of one another

# ---- Step 3: 
### State significance level:
# Alpha = 0.05

# ---- Step 4: 
### Conduct test:
# We read some literature and find that the population median is 175 cells/ml
wilcox.test(Dairy2$cellcount, mu =175)

# Step 5:
### State whether data are consistent with the null hypothesis

# No they are not.
# The P value is < alpha.
# Either:
# There is a very small chance that this sample is from a similar population to the general population
# Or, and we suspect more likely, the sample population has a different median ALP than the general population.
# I wonder why???? (Again)
# And yes, we already know that statistics doesn't tell you why... you need to apply your knowledge of the context to figure that out.


### ------------------------ Compare sample FREQUENCY (proportion) to population level -------------------------------------------------------------

# What if the data were categorical?
# Then we compare the frequency of the condition in the sample to the frequency in a population.

Mastitis_data <-read.table(header = TRUE, text = "
                   HeiferID  result
                   1      0
                   2      0
                   3      0
                   4      0
                   5      1
                   6      0
                   7      1
                   8      0
                   9      0
                   10      0
                   11      1
                   12      0
                   13      1
                   14      0
                   15      0
                   16      0
                   17      1
                   18      0
                   19      1
                   20      0
                   21      0
                   22      0
                   23      1
                   24      0
                   25      1
                   ")
table(Mastitis_data$result)
prop.table(table(Mastitis_data$result))

# Visualise and describe your data:
barplot(table(Mastitis_data$result), main = '', xlab = 'Mastitis', col = 'orange', names = c('No', 'Yes'), ylim = c(0, 20)) 

# ---- Step 1: State your hypotheses
# In heifers in this herd, the proportion of cows with mastitis higher than the general population
# H0 = there is no difference in the proportion of heifers with mastitis between the herd and the general population.
# Ha: there is a difference in the proportion of heifers with mastitis between the herd and the general population (higher in heifers).


# ---- Step 2: 
## These data are categorical, so we do not check for Normality.

# We select a Chi-squared test of independence
# Assumptions:
# Observations are independent
# Expected frequencies are > 5


# ---- Step 3: 
### State significance level:
# Alpha = 0.05

# ---- Step 4: 
### Conduct test:
# We read some literature and find that the expected population proportion of cows with mastitis is 0.08
binom.test(table(Mastitis_data$result), p = 0.08)

# A problem!! Successes = heifers without mastitis. Need to adjust the probability to 1 - 0.08
binom.test(table(Mastitis_data$result), p = 1 - 0.08)

# Step 5:
### State whether data are consistent with the null hypothesis

# No they are not.
# The P value is < alpha.
# Either:
# There is a very small chance that this sample is from a similar population to the general population
# Or, and we suspect more likely, the sample population has a different proportion of heifers with mastitis to the general population.
# I wonder why???? (Again)
# And yes, we already know that statistics doesn't tell you why... you need to apply your knowledge of the context to figure that out.

#-------------------------------------------------------------------------------------------------------
#### 2-SAMPLE TESTS ------------------------------------------------------------------------------------

### ------------------------ Compare 2 sample MEDIANS -------------------------------------------------------------

# First, load your data (you need to download the data from I2 to a location on your computer)

Dairy2 = read.csv('D:/Users/vbrookes/OneDrive - Charles Sturt University/CSU_Teaching/VSC226/Dairy2.csv')
summary(Anaes_data)
str(Anaes_data)


# Visualise and describe your data:
hist(Anaes_data$time[Anaes_data$agent == 'Sev'], main = '', xlab = 'Time (minutes)', col = 'pink', breaks = 'fd')
hist(Anaes_data$time[Anaes_data$agent == 'Iso'], main = '', xlab = 'Time (minutes)', col = 'purple1', breaks = 'fd')
boxplot(Anaes_data$time ~ Anaes_data$agent, xlab = 'Agent', ylab = 'Time (minutes)')
epi.descriptives(Anaes_data$time[Anaes_data$agent == 'Sev'])
epi.descriptives(Anaes_data$time[Anaes_data$agent == 'Iso'])

# ---- Step 1: State your hypotheses
# In dogs undergoing surgery for pyometra, anaesthesia with isoflurane results in quicker recovery than anaesthesia with sevoflurane
# H0 = there is no difference in the time to recovery between anaesthetic agents.
# Ha: There is a difference in the time to recovery between anaesthestic agents (isoflurane < sevoflurane).


# ---- Step 2: 
## Choose test, Check assumptions ----------------------------
# Observations are independent


# ---- Step 3: 
### State significance level:
# Alpha = 0.05

# ---- Step 4: 
### Conduct test:
wilcox.test(Anaes_data$time[Anaes_data$agent == 'Sev'], Anaes_data$time[Anaes_data$agent == 'Iso'])

# Step 5:
### State whether data are consistent with the null hypothesis

# No the data are not consistent with the null hypothesis. 
# The P value is < alpha.
# There is a significant difference between the time to recovery for each group.

# Either:
# There is a very small chance that these samples are from similar source populations
# Or, and we suspect more likely, the populations' time to recoveries are different.
# I wonder why?
# Note that statistics doesn't tell you why... you need to apply your knowledge of the context to figure that out.


### ------------------------ Compare 2 sample MEANS -------------------------------------------------------------

# First, load your data (you need to download the data from I2 to a location on your computer)
# we will use Dairy2 again

Dairy2 = read.csv('D:/Users/vbrookes/OneDrive - Charles Sturt University/CSU_Teaching/VSC226/Dairy2.csv')
summary(Dairy2)
str(Dairy2)

# let's re-code breed and parity to factors (because the are categorical data and 'factor' is the term for this in R)
Dairy2$breed = as.factor(Dairy2$breed)
Dairy2$parity = as.factor(Dairy2$parity)

# we are going to comapre milk yield between breeds: Friesian and Jersey

# Visualise and describe your data:
hist(Dairy2$kgmilk[Dairy2$breed == 'Friesian'], main = '', xlab = 'Milk (kg)', col = 'red1', breaks = 'fd')
hist(Dairy2$kgmilk[Dairy2$breed == 'Jersey'], main = '', xlab = 'Milk (kg)', col = 'green1', breaks = 'fd')
boxplot(Dairy2$kgmilk ~ Dairy2$breed, xlab = 'Agent', ylab = 'Milk (kg)')
epi.descriptives(Dairy2$kgmilk[Dairy2$breed == 'Friesian'])
epi.descriptives(Dairy2$kgmilk[Dairy2$breed == 'Jersey'])

# ---- Step 1: State your hypotheses
# In milking cows, milk yield in Friesians is greater than that in Jerseys
# H0 = there is no difference in milk yield between breeds.
# Ha: There is a difference in milk yield between breeds (Friesians < Jerseys).


# ---- Step 2: 
## Choose test, Check assumptions ----------------------------
# Observations are independent
# Data are Normally distributed

## Check whether the sample values are Normally distributed----------------------------
# Visual checks
# already looked at histograms
qqnorm(Dairy2$kgmilk[Dairy2$breed == 'Friesian']) # Should be a straight 45 degree line
qqline(Dairy2$kgmilk[Dairy2$breed == 'Friesian']) # Adds a line to check further
qqnorm(Dairy2$kgmilk[Dairy2$breed == 'Jersey']) # Should be a straight 45 degree line
qqline(Dairy2$kgmilk[Dairy2$breed == 'Jersey']) # Adds a line to check further

# Skewness (wonky tail), kurtosis (too pointy or too flat)
# For both, if n<200 then the values need to be < +/-1.96, and if n>=200 < +/-2.58
Shape = epi.descriptives(Dairy2$kgmilk[Dairy2$breed == 'Friesian']) # collect the values that you need in an object called Shape

Shape$symmetry$skewness/(Shape$arithmetic$sd/sqrt(Shape$arithmetic$n)) # Check the skewness

Shape$symmetry$kurtosis/(Shape$arithmetic$sd/sqrt(SShape$arithmetic$n)) # Check the kurtosis


Shape = epi.descriptives(Dairy2$kgmilk[Dairy2$breed == 'Jersey']) # collect the values that you need in an object called Shape

Shape$symmetry$skewness/(Shape$arithmetic$sd/sqrt(Shape$arithmetic$n)) # Check the skewness

Shape$symmetry$kurtosis/(Shape$arithmetic$sd/sqrt(Shape$arithmetic$n)) # Check the kurtosis

# Kolmogorv-Smirnoff test n>50, Shapiro-Wilk test if n<=50
# H0 is Normality, so we our data are considered consistent with Normality (the null) if P > 0.05
ks.test(jitter(Dairy2$kgmilk[Dairy2$breed == 'Jersey']), "pnorm", mean=mean(Dairy2$kgmilk[Dairy2$breed == 'Jersey']), sd=sd(Dairy2$kgmilk[Dairy2$breed == 'Jersey']))
shapiro.test(Dairy2$kgmilk[Dairy2$breed == 'Jersey'])
ks.test(jitter(Dairy2$kgmilk[Dairy2$breed == 'Friesian']), "pnorm", mean=mean(Dairy2$kgmilk[Dairy2$breed == 'Friesian']), sd=sd(Dairy2$kgmilk[Dairy2$breed == 'Friesian']))
shapiro.test(Dairy2$kgmilk[Dairy2$breed == 'Friesian'])

## The data are consistent with a Normal distribution. Therefore, we select a 2-sample t-test.

# ---- Step 3: 
### State significance level:
# Alpha = 0.05

# ---- Step 4: 
### Conduct test:
t.test(Dairy2$kgmilk[Dairy2$breed == 'Friesian'], Dairy2$kgmilk[Dairy2$breed == 'Jersey'])

# Step 5:
### State whether data are consistent with the null hypothesis

# No the data are not consistent with the null hypothesis. 
# The P value is < alpha.
# There is a significant difference between the milk yield for each group.

# Either:
# There is a very small chance that these samples are from similar source populations
# Or, and we suspect more likely, the populations'milk yields are different.
# I wonder why?
# Note that statistics doesn't tell you why... you need to apply your knowledge of the context to figure that out.

### ------------------------ Compare FREQUENCY (proportion) between two groups -------------------------------------------------------------

# Fish data!!

Fish_data <-read.table(header = TRUE, text = "
                   FishID  ulcer Size
                   1      0     S
                   2      0    S
                   3      0    S
                   4      0    S
                   5      1    S
                   6      0    S
                   7      1    S
                   8      0    S
                   9      0    L
                   10      0    L
                   11      1    L
                   12      0    L
                   13      1    L
                   14      0    L
                   15      0    L
                   16      0    L
                   17      1    L
                   18      0    L
                   19      1    L
                   20      0    L
                   21      0    L
                   22      0    L
                   23      1    L
                   24      0   S
                   25      1   S
                   ")
table(Fish_data$ulcer, Fish_data$Size)
prop.table(table(Fish_data$ulcer, Fish_data$Size))

# Visualise and describe your data:
barplot(table(Fish_data$ulcer, Fish_data$Size), beside = T, main = '', xlab = 'Mastitis', col = c('orange', 'green1'),  ylim = c(0, 10)) 
legend("topright", legend= c('No', 'Yes'), title="Ulcer", fill = c('orange2', 'green1'))

# ---- Step 1: State your hypotheses
# In koi carp kept in zoological exhibits, fish <500g liveweight have more ulcers than fish ≥500g in a 3 month period over summer
# H0 = there is no difference in the proportion of small or large fish with ulcers
# Ha: There is a difference in the proportion of small or large fish with ulcers (small > large)


# ---- Step 2: 
## These data are categorical, so we do not check for Normality.

# We select a Chi-squared test of independence
# Assumptions:
# Observations are independent
# Expected frequencies are > 5


# ---- Step 3: 
### State significance level:
# Alpha = 0.05

# ---- Step 4: 
### Conduct test:
# We read some literature and find that the expected population proportion of cows with mastitis is 0.08
Test = chisq.test(table(Fish_data$ulcer, Fish_data$Size))

Test$expected

# A problem!! Fewer than 5 in expected cells
Test = fisher.test(table(Fish_data$ulcer, Fish_data$Size))

# Step 5:
### State whether data are consistent with the null hypothesis

# Yes they are!
# The P value is > alpha, and the 95% confidence interval ranges across 1 (this is odds ratios).
# It seems that ulcers occur as frequently in big fish and little fish.
# I wonder why???? (Again)
# And yes, we already know that statistics doesn't tell you why... you need to apply your knowledge of the context to figure that out.













