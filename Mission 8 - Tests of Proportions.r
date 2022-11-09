# STAT E-100
# 2022 Fall - Mission 8
#    One-sample proportion z-tests
#    One-sample proportion z-intervals
#    Two-sample proportion z-tests
#    Two-sample proportion z-intervals

# For this Mission, load the following Excel spreadsheet
# Employed_ACS_recoded.xlsx
# For convenience, the code below refers to the dataframe in the
# same name as the Excel spreadsheet's file name (Employed_ACS_recoded)
# Please check to ensure that the names of data frames and columns or variables
# are correct for the analyses on the Mission.

# The 'mosaic' library is needed for the tally() function
# install.packages("mosaic") # Install the 'mosaic' package
library("mosaic") # Load the 'mosaic' package
# The 'mosaic' library should load after running the line above but if it does
# not, then un-comment (remove the hashtag) from the line above it to install
# the 'mosaic' library again.

# Generate a table of counts by marital status
# Approach 1
counts <- table(Employed_ACS_recoded$Is_Married)
counts
barplot(counts, xlab="Marital Status", ylim =c(0,500))
# Approach 2
tally(~Is_Married,
      format = "count",
      data = Employed_ACS_recoded, margins=TRUE)

# Generate a sampling distribution of sample proportions
# Define the number of samples
n = 1000

#create empty vector of length n
sample_proportions = rep(NA, n)

#fill empty vector with means
for(i in 1:n){
  sample_proportions[i] = mean(sample(x=Employed_ACS_recoded$Is_Married,
                                      size=30, # Set the size of each sample to a certain value
                                      replace=TRUE))
}
sample_proportions[1] # What is the sample proportion for sample 1 out of n?

sd(Employed_ACS_recoded$Is_Married) # Population Standard Deviation
mean(sample_proportions) # What is the average of the sampling distribution of sample proportions?

# View first six sample proportions
head(sample_proportions)

# Generate a histogram of the sampling distribution of sample proportions
hist(sample_proportions, main = "", xlab = "Sample Proportions", col = "steelblue")

### One-sample proportion z-test (example only, replace numerical values)
prop.test(x = 20, n = 30, p = 0.3, correct = FALSE)
binom.test(x = 20, n = 30, p = 0.3, alternative = "two.sided")

# The code immediately below will generate a two-way table for counts to
# help calculate the sample proportions for the two groups:
#    Out of the people who are unmarried, how many have health insurance?
#    Out of the people who are married, how many have health insurance?
tally(~Is_Married + Has_Health_Insurance,
      format = "count",
      data = Employed_ACS_recoded,
      margins=TRUE)

### Two-proportion z-test and two-proportion z-confidence intervals (example only, replace numerical values)
prop.test(x = c(123, 456), n = c(789, 1011))
prop.test(x = c(490, 400), n = c(500, 500), alternative = "less")
prop.test(x = c(490, 400), n = c(500, 500), alternative = "greater")