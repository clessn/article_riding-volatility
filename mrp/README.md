# Replicable code: Multi-level regression and post-stratification (MRP)

This folder contains the data and the code files to reproduce the MRPs that was done in the article.

# Ethical constraints
<!--- The surveys used in this article were exclusive, and access to the data was conditional upon signing an ethics form. As a result, the `table1_respondentsRCI` dataset presented here is a shortened version of the full dataset used in this study. If you are interested in accessing additional variables or data, please contact us at <hubert.cadieux.1@ulaval.ca> to discuss the possibility of obtaining access. --->



# Datasets

## census_data
This file contains 2016 Quebec census data by riding.

## census_age
https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=F&GENDERlist=1&STATISTIClist=1&HEADERlist=0&DGUIDlist=2021S0503421&SearchText=Qu%E9bec

## simulated_survey_data
This file contains simulated survey data generated using census_data. 

** vote_solidity instead of vote_fragility

# Code files

## simulate_survey_data
This file is used to simulate survey data based on `census_data` and `table1_respondentsRCI`.

## find_interactions.R
This file is used to explore survey data to find interactions between SES variables with vote fragility as a dependent variable. It takes data from `mrp/data/simulated_survey_data.rds`. The file is divided into 3 steps.
- 1. Examine interactions between the 5 SES variables.
    - At this step, a choice is made of only keeping **age, education and income** for the following steps. Given the volume of the data, multi-level regression will work better with less variables. Theoretical expectations and the data exploration made in step 1 allow us to select those 3 variables.
- 2. Examine interactions between the selected variables.
    - 


It starts by finding SES variables that interact together (gender, age, education, income and language spoken) and then analyses how those variables interact with the `region` variable.

## generate_postStrat_table.R
