# Replicable code: On Volatility and Parties’ Potential for Growth in Multiparty Systems: The Case of the 2022 Quebec Election

This README follows the guidelines for data replication of The Journal of Politics of the University of Chicago Press. These guidelines can be found here: https://www.journals.uchicago.edu/journals/jop/data-replication

This article can be replicated using R version 4.1.3.

# Ethical constraints
The surveys used in this article were exclusive, and access to the data was conditional upon signing an ethics form. As a result, the `table1_respondentsRCI` dataset presented here is a shortened version of the full dataset used in this study. If you are interested in accessing additional variables or data, please contact us at <hubert.cadieux.1@ulaval.ca> to discuss the possibility of obtaining access.

# Datasets
The two datasets needed to reproduce this article are included in the `data` folder.
- `table1_respondentsRCI`: contains RCI of each party for 9135 respondents surveyed through monthly surveys conducted from January to August 2022. The electoral riding of each respondent is also included.
- `table2_duringCampaign`: contains the predicted vote shares of the parties for each electoral riding during the campaign.

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

# Code files

## figure1_rciDistribution
This script takes data from `table1_respondentsRCI` to generate `graphs/figure1_rciDistribution`.


