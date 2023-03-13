# Replicable code: On Volatility and Parties’ Potential for Growth in Multiparty Systems: The Case of the 2022 Quebec Election

This README follows the guidelines for data replication of The Journal of Politics of the University of Chicago Press. These guidelines can be found here: https://www.journals.uchicago.edu/journals/jop/data-replication

This article can be replicated using R version 4.1.3.

# Ethical constraints
The surveys used in this article were exclusive, and access to the data was conditional upon signing an ethics form. As a result, the `table1_respondentsRCI` dataset presented here is a shortened version of the full dataset used in this study. If you are interested in accessing additional variables or data, please contact us at <hubert.cadieux.1@ulaval.ca> to discuss the possibility of obtaining access.

# Datasets
The two datasets needed to reproduce this article are included in the `data` folder.
- `table1_respondentsRCI`: contains RCI of each party for 9135 respondents surveyed through monthly surveys conducted from January to August 2022. The electoral riding of each respondent is also included.
- `table2_duringCampaign`: contains the predicted vote shares of the parties for each electoral riding during the campaign.

These two datasets are then agregated into a third dataset, `table3_aggregatedData` which is generated by the script located at `codes/generate_table3.R`.

## table1_respondentsRCI
Each observation of this dataset represents one respondent. It contains 9135 observations and 9 variables.
- **respondent_id**: unique number identifying respondents across the different surveys.
- **month**: the month in which the respondent was surveyed.
- **riding_name**: the name of the respondent's electoral riding.
- **riding_id**: the id of the respondent's electoral riding. Each riding has its own numbered id.
- **rci_CAQ**: the respondent's RCI score for the CAQ, which falls within a range of -1 to 1.
- **rci_PLQ**: the respondent's RCI score for the PLQ, which falls within a range of -1 to 1.
- **rci_QS**: the respondent's RCI score for the QS, which falls within a range of -1 to 1.
- **rci_PQ**: the respondent's RCI score for the PQ, which falls within a range of -1 to 1.
- **rci_PCQ**: the respondent's RCI score for the PCQ, which falls within a range of -1 to 1.

## table2_duringCampaign
Each observation of this dataset represents a single day's *Qc125.com* prediction of a riding's vote share of a political party. It contains 52388 observations and 6 variables.
- **date**: the date of the observation.
- **riding_name**: the name of the respondent's electoral riding.
- **riding_id**: the id of the respondent's electoral riding. Each riding has its own numbered id.
- **party**: the political party associated to this prediction. Options are CAQ, PLQ, QS, PQ and PCQ.
- **pred**: the predicted vote share of the party.
- **moes**: the margin of error of this prediction (as reported by *Qc125.com*).

## table3_aggregatedData
This dataset is generated by the file located at `codes/generate_table3.R`. Each observation of this dataset represents a riding with its vote fragility index (before the campaign) and its campaign volatility (during the campaign). It contains 125 observations and 4 variables.
- **riding_id**: the numerical id of the riding.
- **riding_name**: the name of the riding.
- **fragility_index**: vote fragility of the riding before the campaign generated from survey data. From 0 - *not fragile* to 1 - *fragile*. 
- **volatility**: campaign volatility of the riding generated from *Qc125.com* data. From 0 - *not volatile* to 1 - *volatile*.



# Code files

## functions.R
This script contains relevant functions to the analysis. It can be sourced at the start of coding files.

## generate_table3.R
This script aggregates data from `table1_respondentsRCI` and `table2_duringCampaign` at the riding-level to calculate the vote fragility index and campaign volatility for each riding. The dataset generated by this file is `data/table3_aggregatedData`.

## figure1_rciDistribution.R
This script takes data from `table1_respondentsRCI` to generate `graphs/figure1_rciDistribution.png`.

## figure2_fragilityIndexDistribution.R
This script takes data from `table3_aggregatedData` to generate `graphs/figure2_fragilityIndexDistribution.png`.

## figure3_campaignVolatilityDistribution.R
This script takes data from `table3_aggregatedData` to generate `graphs/figure3_campaignVolatilityDistribution.png`.

## figure4_fragilityVSvolatility.R
This script takes data from `table3_aggregatedData` to generate `graphs/figure4_fragilityVSvolatility.png`.

## chi_square_test.R
This script takes data from `table3_aggregatedData` to do a chi-squared test. You can read [this section](https://bookdown.org/tomholbrook12/bookdown-demo/hypothesis-testing-with-crosstabs.html#hypothesis-testing-with-crosstabs-1) for more documentation about the chi-squared test.