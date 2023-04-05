# riding-volatility

## Metadata of Data/marts/riding_volatility_preCampaign.rds
- **riding**: name of the riding
- **mean_irc**: mean RCI of the leader for the riding's respondents
- **n_below3**: number of respondents who have a below 0.3 RCI for their first party
- **n_riding**: number of respondents in the riding
- **mean_irc_rev**: `mean_irc*-1`. This operation is done to reverse the variable since a low RCI --> bigger volatility
- **prop_below3**: `n_below3/n_riding`. The proportion of the riding's respondents who have a below 0.3 RCI for their first party
- **prop_below3_scaled**: `scale(prop_below3)`. prop_below3 with a SD of 1
- **mean_irc_scaled**: `scale(mean_irc_rev)`. mean_irc_rev with a SD of 1
- **volatility_index**: `prop_below3_scaled + mean_irc_scaled` transformed so that the maximum becomes 1 and the minimum becomes 0

## Metadata of Data/marts/riding_volatility_duringCampaign.rds
- **riding**: name of the riding
- **volatility**: sum of the qc125 predictions' variance for the 5 main parties during the campaign
- **volatility_scale**: `scale(volatility)`. volatility with a SD of 1
- **volatility_log**: `log(volatility)`. logarithmic transformation of volatility
- **volatility_log_scale**: `scale(volatility_log)`. volatility_log with a SD of 1
- **real_volatility**: volatility_log_scale transformed so that the maximum becomes 1 and the minimum becomes 0