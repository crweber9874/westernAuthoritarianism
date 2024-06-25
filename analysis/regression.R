rm(list = ls())
library(dplyr)
library(rstan)
library(tidyr)
library(modelr)
library(brms)
library(tidybayes)
library(ggplot2)
library(simplecolors)
library(patchwork)
library(fastDummies)
library(googleCloudStorageR)

#### ## ## #### #### ## #### #### ## #### ## #### #### ## #### ## ## ## ## ####
#### ## ## #### #### ## #### #### ## #### ## #### #### ## #### ## ## ## ## ####

source("/Users/Chris/Dropbox/github_repos/westernAuthoritarianism/westernAuthoritarianism/tmp/projectFunctions.R")

gcs_get_object("dataActive.csv", bucket = "grau_data", saveToDisk = "dataActive.csv", overwrite = TRUE)

dataActive <- read.csv("dataActive.csv") %>%
  mutate(gen = factor(gen)) %>%
  dummy_cols(select_columns = "gen") %>%
  filter(race == "White" | race == "Hispanic") %>%
  mutate(authSum = rowMeans(cbind(auth1, auth2, auth3, auth4), na.rm = TRUE)) %>%
  mutate(authSum = zero.one(authSum)) %>%
  mutate(authM1 = zero.one(m1.dif)) %>%
  mutate(authM2 = zero.one(m2.dif)) %>%
  mutate(authM3 = zero.one(mean_name)) %>%
  select("mip", "ideology", "trump_vote", "vote2016", "immUndocumented", "immSeparation",
   "immAsylum", "immBirthright", "immTech", "immWall", "concealCarry", "background",
   "registry", "waterDams", "envWater", "waterConservation", "female", "latino",
   "college", "income", "gen_Boomer", "gen_Greatest", "gen_Millenial", "married",
   "christian", "party3", "authSum", "authM1", "authM2", "authM3")

IV <- "female + latino + college + income + gen_Boomer + gen_Greatest +
       gen_Millenial +  married + christian +  authM3 + authM3:latino"

# Policy Politics Models
mip   <- brms.nominal(data = dataActive, IV = IV, DV = "mip")
pid3  <- brms.nominal(data = dataActive, IV = IV, DV = "party3")
ideology <- brms.ordinal(data = dataActive, IV = IV, DV = "ideology")
trump2020 <- brms.binary(data = dataActive, IV = IV, DV = "trump_vote")
trump2016 <-brms.binary(data = dataActive, IV = IV, DV = "vote2016")
immUndocumented <- brms.ordinal(data = dataActive, IV = IV, DV = "immUndocumented")
immSeparation <- brms.ordinal(data = dataActive, IV = IV, DV = "immSeparation")
immAsylum <- brms.ordinal(data = dataActive, IV = IV, DV = "immAsylum")
immBirthright <- brms.ordinal(data = dataActive, IV = IV, DV = "immBirthright")
immTech <- brms.ordinal(data = dataActive, IV = IV, DV = "immTech")
immWall <- brms.ordinal(data = dataActive, IV = IV, DV = "immWall")
concealCarry <- brms.ordinal(data = dataActive, IV = IV, DV = "concealCarry")
background <- brms.ordinal(data = dataActive, IV = IV, DV = "background")
registry <- brms.ordinal(data = dataActive, IV = IV, DV = "registry")
waterDams <- brms.ordinal(data = dataActive, IV =  IV, DV = "waterDams")
envWater <- brms.ordinal(data = dataActive, IV = IV, DV = "envWater")
waterConservation <- brms.ordinal(data = dataActive, IV = IV, DV = "waterConservation")

# Save Models

regressionModels <- list(mip, pid3, ideology, trump2020, trump2016, immUndocumented, immSeparation,
                         immAsylum, immBirthright, immTech, immWall, concealCarry, background, registry,
                         waterDams, envWater, waterConservation)
names(regressionModels) <- c("mip", "pid3", "ideology", "trump2020", "trump2016", "immUndocumented", "immSeparation",
                         "immAsylum", "immBirthright", "immTech", "immWall", "concealCarry", "background", "registry",
                         "waterDams", "envWater", "waterConservation")

save(regressionModels, file = "~/Dropbox/github_repos/westernAuthoritarianism/westernAuthoritarianism/tmp/modelsR.rda")
