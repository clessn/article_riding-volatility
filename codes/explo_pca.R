# Packages ----------------------------------------------------------------
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(plot3D)


# Data --------------------------------------------------------------------
Volatility <- readRDS("data/table3_aggregatedData.rds") %>% 
  select(riding_id, volatility, fragility_index)

Data <- readRDS("mrp/data/census_data.rds") %>% 
  select(riding_id, region, female, starts_with("age"), french,
         starts_with("income"), starts_with("educ"),
         starts_with("vote2018")) %>% 
  left_join(Volatility, ., by = "riding_id")

PcaSes <- Data %>% 
  select(starts_with(c("age", "female", "french", "income", "educ")))

PcaVote <- Data %>% 
  select(starts_with("vote2018"))


# PCA: SES ----------------------------------------------------------------

## Correlations between variables
cor <- cor(PcaSes)
ggcorrplot::ggcorrplot(PcaSes)

## Generate pca
pca <- PCA(PcaSes, scale.unit = T, graph = T)

## eig > 1 is good
eig <- get_eigenvalue(pca)
round(eig)
ve_pc1 <- eig[1,2]
ve_pc2 <- eig[2,2]

## Graph
df <- data.frame(riding = Data$riding_id,
                 region = as.numeric(factor(Data$region)),
                 dim1 = pca$ind$coord[,1],
                 dim2 = pca$ind$coord[,2],
                 dim3 = pca$ind$coord[,3])


scatter3D(x = df$dim1,
          y = df$dim2,
          z = df$dim3, colvar = df$region,
          pch = 19, phi = -90, theta = 0)



scatter3D(x = df$dim1,
          y = df$dim2,
          z = df$dim3, colvar = df$region,
                   pch = 19, phi = 10, theta = 30)

scatter3D(x = df$dim1,
          y = df$dim2,
          z = df$dim3, colvar = df$region,
          pch = 19, phi = 10, theta = 65)

df %>%
  ggplot(aes(x = dim1, y = dim2)) +
  geom_jitter(aes(color = factor(region)), shape = 19)

df %>%
  ggplot(aes(x = dim2, y = dim3)) +
  geom_jitter(aes(color = factor(region)), shape = 19)

df %>%
  ggplot(aes(x = dim1, y = dim3)) +
  geom_jitter(aes(color = factor(region)), shape = 19)


# PCA: Vote ----------------------------------------------------------------

## Correlations between variables
cor <- cor(PcaVote)
ggcorrplot::ggcorrplot(PcaVote)

## Generate pca
pca <- PCA(PcaVote, scale.unit = T, graph = T)

## eig > 1 is good
eig <- get_eigenvalue(pca)
round(eig)
ve_pc1 <- eig[1,2]
ve_pc2 <- eig[2,2]

## Graph
df <- data.frame(riding = Data$riding_id,
                 region = as.numeric(factor(Data$region)),
                 dim1 = pca$ind$coord[,1],
                 dim2 = pca$ind$coord[,2],
                 dim3 = pca$ind$coord[,3])


scatter3D(x = df$dim1,
          y = df$dim2,
          z = df$dim3, colvar = df$region,
          pch = 19)



scatter3D(x = df$dim1,
          y = df$dim2,
          z = df$dim3, colvar = df$region,
          pch = 19, phi = 10, theta = 30)

scatter3D(x = df$dim1,
          y = df$dim2,
          z = df$dim3, colvar = df$region,
          pch = 19, phi = -75, theta = 45)

df %>%
  ggplot(aes(x = dim1, y = dim2)) +
  geom_jitter(aes(color = factor(region)), shape = 19)

df %>%
  ggplot(aes(x = dim2, y = dim3)) +
  geom_jitter(aes(color = factor(region)), shape = 19)

df %>%
  ggplot(aes(x = dim1, y = dim3)) +
  geom_jitter(aes(color = factor(region)), shape = 19)
