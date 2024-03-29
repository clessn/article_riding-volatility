# Replicable code: On Volatility and Parties’ Potential for Growth in Multiparty Systems: The Case of the 2022 Quebec Election

This README follows the guidelines for data replication of The Journal of Politics of the University of Chicago Press. These guidelines can be found here: https://www.journals.uchicago.edu/journals/jop/data-replication

This article can be replicated using R version 4.1.3. We consider opening the `article_riding-volatility.Rproj` to work in this repo.

# Packages to install
`devtools::install_github("clessn/clessnverse")`

# Ethical constraints
The surveys used in this article were exclusive, and access to the data was conditional upon signing an ethics form. As a result, the `table1_respondentsRCI` dataset presented here is a shortened version of the full dataset used in this study. If you are interested in accessing additional variables or data, please contact us at <hubert.cadieux.1@ulaval.ca> to discuss the possibility of obtaining access.

In the `mrp` folder where basic socio-demographic variables are necessary, the `mrp/codes/1_simulate_survey_data.R` allows the replicator to simulate the individual socio-demographic variables of respondents.

# Datasets
Information about the datasets are in the `codebook.md` file.

# Code files

## functions.R
This script contains relevant functions to the analysis. It can be sourced at the start of coding files.

## generate_table3.R
This script merges data from `data/table1_respondentsRCI`, `data/table2_duringCampaign` and `mrp/data/table_post_strat_fragility` to generate `table3_aggregatedData`. It aggregates data from `table1_respondentsRCI` and `table2_duringCampaign` at the riding-level to calculate the vote fragility index (with and without MRP) and campaign volatility for each riding. **It is not needed to run this script as `table3_aggregatedData` is already in the `data` folder**.

## figure1_rciDistribution.R
This script takes data from `table1_respondentsRCI` to generate `graphs/figure1_rciDistribution.png`.

## figure2_RCI_prob_voteInt.R
This script takes data from `data/table1_respondentsRCI.rds`, `mrp/data/simulated_survey_data.rds` and `mrp/data/post_strat_table.rds` to generate `figure2_prob_voteInt.png`.

## figure3_fragilityIndexDistribution.R
This script takes data from `table3_aggregatedData` to generate `graphs/figure3_fragilityIndexDistribution.png`.

## figure4_campaignVolatilityDistribution.R
This script takes data from `table3_aggregatedData` to generate `graphs/figure4_campaignVolatilityDistribution.png`.

## figure5_fragilityVSvolatility.R
This script takes data from `table3_aggregatedData` to generate `graphs/figure5_fragilityVSvolatility.png`.

## chi_square_test.R
This script takes data from `table3_aggregatedData` to do a chi-squared test. You can read [this section of *An Introduction to Political and Social Data Analysis Using R* of Thomas M. Holbrook](https://bookdown.org/tomholbrook12/bookdown-demo/hypothesis-testing-with-crosstabs.html#hypothesis-testing-with-crosstabs-1) for more documentation about the chi-squared test.

## regressions.R
This script takes data from `data/table3_aggregatedData.rds` and `mrp/data/census_data.rds` to generate the regression table presented in this article.

