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


# Code files

## simulate_survey_data
This file is used to simulate survey data based on `census_data` and `table1_respondentsRCI`.

## find_interactions.R
This file is used to explore survey data to find interactions between SES variables with vote fragility as a dependent variable. It takes data from `table1_`

## generate_postStrat_table.R
