#aller chercher le package dans le système
library(dplyr)

minmaxNormalization <- function(x) {
  return((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
}

#importe les données
articles <- read.csv("/Users/arnaud/Dropbox/CLESSN/riding-volatility/_SharedFolder_riding-volatility/Snowballing Party ID/SnowballingPartyID.csv")

#pour modifier les noms de tes colonnes de données
names(articles) <- c("author", "title", "year", "n_cites", "pertinence", "survey", "done")

#runner les hisrogrammes
hist(articles$year)
hist(articles$n_cites)
hist(articles$pertinence)

#pour placer sur un log les données (1-10) (normalité)
articles$n_cites_log <- log(articles$n_cites)
hist(articles$n_cites_log)

#mettre entre 0-1
articles$c1_year <- minmaxNormalization(articles$year)
articles$c2_ncites <- minmaxNormalization(articles$n_cites_log)
articles$c3_pertinence <- minmaxNormalization(articles$pertinence)

hist(articles$c1_year)
hist(articles$c2_ncites)
hist(articles$c3_pertinence)

#calcul pour la centralité
articles$centrality <- minmaxNormalization(articles$c1_year+articles$c2_ncites+articles$c3_pertinence)
hist(articles$centrality)
sheet <- articles %>% 
  mutate(centrality = round(centrality, 3)) %>% 
  arrange(-centrality) %>% 
  select(author, title, year, centrality)
sheet


