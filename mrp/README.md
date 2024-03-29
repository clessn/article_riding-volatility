# Replicable code: Multi-level regression and post-stratification (MRP)

This folder contains the data and the code files to reproduce the MRPs that was done in the article.

# Ethical constraints
<!--- The surveys used in this article were exclusive, and access to the data was conditional upon signing an ethics form. As a result, the `table1_respondentsRCI` dataset presented here is a shortened version of the full dataset used in this study. If you are interested in accessing additional variables or data, please contact us at <hubert.cadieux.1@ulaval.ca> to discuss the possibility of obtaining access. --->

# Datasets
Information about the datasets are in the `mrp/mrp_codebook.md` file.

# Code files

## 1_simulate_survey_data.R
This file is used to simulate survey data based on `census_data` and `table1_respondentsRCI`.

## 2_find_interactions.R
This file is used to explore survey data to find interactions between SES variables with vote fragility as a dependent variable. It takes data from `mrp/data/simulated_survey_data.rds`. The file is divided into 3 steps.
- **Step 1** Examine interactions between the 5 SES variables.
    - At this step, a choice is made of only keeping **age, education and income** for the following steps. Given the volume of the data, multi-level regression will work better with less variables. Theoretical expectations and the data exploration made in **step 1** allow us to select those 3 variables.
    - The graph generated by **step 1** is located at `mrp/graphs/find_interactions_5ses.png`. The 1st plot of the second row shows the mean of the vote solidity of each subgroup shown in the graph (ageC and educ). Each line is a category of educ (educHSB, educColl, educUniv) while each point on the x-axis is an age category. The same logic is applied to each square of the grid. 
- **Step 2** Examine interactions between the 3 selected variables.
    - The graph generated by **step 2** is located at `mrp/graphs/find_interactions_3ses.png`. It can be read as the same way as the graph from **step 1**.
    - Based on this graph, interactions between all 3 variables would be justified. **Step 3** will offer more information
- **Step 3** Examine interactions between the 3 variables and the regions
    - The graph generated by **step 3** is located at `mrp/graphs/find_interactions_regions.png`. Each line is a region.

## 3_generate_postStrat_table.R

** citer les articles de post-strat

## 4_merge_riding_projections.R
This file is used to attach the riding projections around the start of the campaign to the survey data. It takes riding projections from the `data/table2_duringCampaign.rds` file and joins them on `mrp/data/simulated_survey_data.rds`. It generates the dataset `mrp/data/simulated_survey_with_riding_projections.rds`.

## 5_testing_models.R
This file contains the code to test different models. It takes data from the `mrp/data/simulated_survey_data_with_projections.rds` file. Since no model was performant enough, a pretty simple linear regression will be used in the next file, `6_predicting.R`.

## 6_predicting.R
This file contains the code to predict the model on the newdata from the synthetic post-stratification table. It imports the `mrp/data/post_strat_table.rds` dataset and the `mrp/data/simulated_survey_data_with_riding_projections.rds` to create the model. Since no model was performant enough, the model is a pretty simple linear regression. It generates the `mrp/data/table_post_strat_fragility.rds` file which is used 
