################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

######                             Latent Variable Models        ############
################################################################################################################################################################################################################################################################################################################################################################################################################
################################################################################
# Modified: April 19 2024

rm(list = ls())

###  Recode Western States ###
## Note: There must be a better way!
rm(list = ls())
library(readstata13)
library(MASS)
library(ggplot2)
detach("packages:car")
library(dplyr)
library(lavaan)
library(broom)
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

source("/Users/Chris/Dropbox/masterData/Western_States/git/westernStates/authoritarianismLatino/helper/projectFunctions.R")

load("/Users/Chris/Dropbox/masterData/Western_States/git/westernStates/data/westernCleaned.rda")


################################################################################

df <- gc_pull_csv(bucket_name = "grau_data", file_name = "cWestern.csv")

df <- df %>%
  mutate(gen = factor(gen)) %>%
  dummy_cols(select_columns = "gen") %>%
  filter(race == "White" | race == "Hispanic")

modDat <- df %>%
  dplyr::select(auth1, auth2, auth3, auth4, caseid, race) %>%
  pivot_longer(cols = c(auth1, auth2, auth3, auth4), names_to = "item", values_to = "response")


# # Differential item functioning
## This is the basic model to generate latent authoritarianism ####
## Run time: Approximately 20 minutes, 10 cores
## Notes: 2PL These models exclude intercept terms, instead estimating separate parameters for each group by item combination.

dif1 <- bf(
  response ~ exp(logalpha) * eta,
  eta ~ 0 + race + (1 + race | item) + (1 | caseid),
  logalpha ~ 0 + (1 + race | item),
  nl = TRUE
)

prior2pl <-
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "caseid", nlpar = "eta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  # Add a prior to the intercept
  prior("normal(0, 5)", class = "b", nlpar = "eta")

difM1 <- brm(
  formula = dif1,
  data = modDat,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior2pl,
  iter = 3000,
  chains = 3,
  cores = 10,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
)



## Notes: These models exclude intercept terms, instead estimating separate parameters for each group by item combination.
##        This is a 2PL model, which estimates a discrimination parameter for each item and a difficulty parameter for each item. Unlike
##        the previous model, item parameter estimates do not vary by group.

# # Differential Item Functioning, 2PL
dif2 <- bf(
  response ~ exp(logalpha) * eta,
  eta ~ 0 + race + (1 | item) + (1 | caseid),
  logalpha ~ 0 + (1 | item),
  nl = TRUE
)

# # Priors
prior2pl <-
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("constant(1)", class = "sd", group = "caseid", nlpar = "eta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

difM2 <- brm(
  formula = dif2,
  data = modDat,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior2pl,
  iter = 2000,
  chains = 3,
  cores = 10,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
)
## Notes: This is a Rasch model, or a one parameter IRT model. It estimates different terms by groups.

# # Differential Item Functioning, 2PL
dif3 <- bf(
  response ~ race + (1 | item) + (1 | caseid)
)

# # Priors
priors <-
  prior("normal(0, 1)", class = "sd", group = "caseid")

difM3 <- brm(
  formula = dif3,
  data = modDat,
  family = brmsfamily("bernoulli", "logit"),
  prior = priors,
  iter = 2000,
  chains = 3,
  cores = 10,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
)

models <- list(dif1 = difM1, dif2 = difM2, df3 = difM3)

save(models, file = "/Users/Chris/Dropbox/masterData/Western_States/git/westernStates/authoritarianismLatino/models/models.rda")


## Notes: This is a two parameter IRT model, saved to a different model file.

noDIF <- bf(
  response ~ 1 + (1 | item) + (1 | caseid)
)

# # Priors
priors <-
  prior("normal(0, 1)", class = "sd", group = "caseid")

noDIF <- brm(
  formula = noDIF,
  data = modDat,
  family = brmsfamily("bernoulli", "logit"),
  prior = priors,
  iter = 2000,
  chains = 3,
  cores = 10,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
)

#######################################


modDat <- df %>%
  dplyr::select(auth1, auth2, auth3, auth4, caseid, race, party3) %>%
  pivot_longer(cols = c(auth1, auth2, auth3, auth4), names_to = "item", values_to = "response")

dif1 <- bf(
  response ~ exp(logalpha) * eta,
  eta ~ 0 + race * as.factor(party3) + (1 + race | item) + (1 | caseid),
  logalpha ~ 0 + (1 + race | item),
  nl = TRUE
)

prior2pl <-
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "caseid", nlpar = "eta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  # Add a prior to the intercept
  prior("normal(0, 5)", class = "b", nlpar = "eta")

difM4 <- brm(
  formula = dif1,
  data = modDat,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior2pl,
  iter = 2000,
  chains = 3,
  cores = 10,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
)

#######################################


dif1 <- bf(
  response ~ exp(logalpha) * eta,
  eta ~ 0 + race * as.factor(party3) + (1 | item) + (1 | caseid),
  logalpha ~ 0 + (1 | item),
  nl = TRUE
)

prior2pl <-
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha") +
  prior("constant(1)", class = "sd", group = "caseid", nlpar = "eta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  # Add a prior to the intercept
  prior("normal(0, 5)", class = "b", nlpar = "eta")

difM5 <- brm(
  formula = dif1,
  data = modDat,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior2pl,
  iter = 2000,
  chains = 3,
  cores = 10,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
)

models <- list(dif1 = difM1, dif2 = difM2, dif3 = difM3, noDIF = noDIF, difM4 = difM4, difM5 = difM5)

save(models, file = "~/Dropbox/github_repos/westernAuthoritarianism/westernAuthoritarianism/tmp/models.rda")

# This is a rather large file, all holding the stan models to compare DIF specifications.
gc_upload("projectmodels",
          path = paste0(path, "/Western_States/git/westernStates/authoritarianismLatino/tmpmodels.rda"),
          unalterable = FALSE,
          data_name = "latentModels.rda"
)
