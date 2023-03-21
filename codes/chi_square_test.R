# Packages ----------------------------------------------------------------
library(tidyverse)

# Data -------------------------------------------------------------------
Data <- readRDS("data/table3_aggregatedData.rds")
View(Data)
names(Data)
str(Data)


# Wrangling/Preparation ---------------------------------------------------

Data <- Data %>%
  mutate(
    # If fragility_index_mrp is > than 0.5, fragile will
    # take a value of 1. If not, it will take a value of 0.
    fragile = ifelse(fragility_index_mrp >= 0.5, 1, 0),
    # Same thing with campaign volatility
    volatile = ifelse(volatility >= 0.5, 1, 0))


# Hypothesis testing ------------------------------------------------------

## Quick cross-tabulation to explore the data
descr::crosstab(Data$volatile, Data$fragile)

### Is there a significative relation in those 4 quadrants?
#### H0: No relationship
#### H1: There is a relationship

crosstab <- descr::crosstab(Data$volatile, Data$fragile,
                     expected=T, #Add expected frequency to each cell
                     prop.chisq = T, #Total contribution of each cell
                     chisq = T) #Results of the chi-squared test will be printed

crosstab

## Save tab into a variable to use the number of rows and columns later
tab <- crosstab$tab

# degrees of freedom: (n_rows - 1)(n_cols - 1)
dfr <- (nrow(tab)-1)*(ncol(tab)-1) ## 1 degree of freedom

# Critical value of chi-square, p=.1, df=1
cv1 <- qchisq(.1, dfr, lower.tail=F)
cv1

# Critical value of chi-square, p=.05, df=1
cv05 <- qchisq(.05, dfr, lower.tail=F)
cv05

# Critical value of chi-square, p=.01, df=1
cv01 <- qchisq(.01, dfr, lower.tail=F)
cv01

chisq.test(Data$volatile, Data$fragile)
### Significative at the 0.05 threshold, not at the 0.01
