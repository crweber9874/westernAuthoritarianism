###  Recode Western States ###
## Note: There must be a better way!
rm(list = ls())
library(readstata13)
library(MASS)
library(ggplot2)
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

gcs_get_object("rWestern.dta", bucket = "grau_data", saveToDisk = "rWestern.dta", overwrite = TRUE)
# Read stata 13
df <- read.dta13("rWestern.dta", missing.type = TRUE, generate.factors = TRUE)

################################################################################

### Recodes
state_list <- c(
  "Washington", "Oregon", "California",
  "Idaho", "Utah", "Nevada", "Arizona",
  "New Mexico", "Colorado", "Wyoming",
  "Montana", "Texas", "Oklahoma", "Kansas",
  "Sonora, Mexico", "Chihuaha, Mexico",
  "Coahila, Mexico", "British Columbia, Canada",
  "Alberta, Canada"
)
region_list <- c(
  "West", "Southwest", "Northwest",
  "West Coast", "Intermountain West",
  "Rocky Mountains", "Other"
)
state <- ifelse(df$WSS01_1 == "selected", "Washington", NA)
region <- ifelse(df$WSS01_b_1 == "selected", "West", NA)
opt <- paste0("WSS01_", c(2:19))
for (i in 1:length(opt)) {
  state <- ifelse(df[, opt[i]] == "selected", state_list[i + 1], state)
}

opt <- paste0("WSS01_b_", c(2:7))
for (i in 1:length(opt)) {
  region <- ifelse(df[, opt[i]] == "selected", region_list[i + 1], region)
}
df$vote1 <- car::recode(as.numeric(df$WSS36_b), "1=1 ; 2=0; else=NA")
df$vote2 <- car::recode(as.numeric(df$WSS36_c), "1=1 ; 2=0; else=NA")
df$voted <- ifelse(is.na(df$WSS36_b), df$vote2, df$vote1)
df$state <- state
df$region <- region

agree_key <- list(`1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
disagree_key <- list(`1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5)

library(dplyr)
library(tidyr)
df <- df %>%
  mutate(id = seq(1:nrow(df))) %>%
  mutate(state = state) %>%
  mutate(region = region) %>%
  mutate(state_residence = inputstate) %>%
  mutate(state_insult = recode(as.numeric(WSS02_1), !!!agree_key)) %>%
  mutate(state_common = recode(as.numeric(WSS02_2), !!!agree_key)) %>%
  mutate(state_interest = recode(as.numeric(WSS02_3), !!!agree_key)) %>%
  mutate(state_we = recode(as.numeric(WSS02_4), !!!agree_key)) %>%
  mutate(state_partme = recode(as.numeric(WSS02_5), !!!agree_key)) %>%
  mutate(state_proud = recode(as.numeric(WSS02_6), !!!agree_key)) %>%
  mutate(state_identity_identity_strength = state) %>%
  mutate(moral_ind1 = recode(as.numeric(WSS09_b_1), !!!agree_key)) %>%
  mutate(moral_ind2 = recode(as.numeric(WSS09_b_2), !!!agree_key)) %>%
  mutate(moral_ind3 = recode(as.numeric(WSS09_b_3), !!!agree_key)) %>%
  mutate(moral_ind4 = recode(as.numeric(WSS09_b_4), !!!agree_key)) %>%
  mutate(moral_group = recode(as.numeric(WSS09), `1` = "Religious Leader", `2` = "Science", `3` = "Teacher", `4` = "Public", `5` = "Family")) %>%
  mutate(sdo1 = recode(as.numeric(WSS10_1), !!!agree_key)) %>%
  mutate(sdo2r = recode(as.numeric(WSS10_2), !!!disagree_key)) %>%
  mutate(sdo3 = recode(as.numeric(WSS10_3), !!!agree_key)) %>%
  mutate(sdo4r = recode(as.numeric(WSS10_4), !!!disagree_key)) %>%
  mutate(rr1 = recode(as.numeric(WSS11_1), !!!agree_key)) %>%
  mutate(rr2r = recode(as.numeric(WSS11_2), !!!disagree_key)) %>%
  mutate(rr3r = recode(as.numeric(WSS11_3), !!!disagree_key)) %>%
  mutate(rr4 = recode(as.numeric(WSS11_4), !!!agree_key)) %>%
  mutate(emp1 = recode(as.numeric(WSS12_1), !!!agree_key)) %>%
  mutate(emp2 = recode(as.numeric(WSS12_2), !!!agree_key)) %>%
  mutate(emp3 = recode(as.numeric(WSS12_3), !!!agree_key)) %>%
  mutate(emp4 = recode(as.numeric(WSS12_4), !!!agree_key)) %>%
  mutate(ind1 = recode(as.numeric(WSS13_1), !!!agree_key)) %>%
  mutate(ind2r = recode(as.numeric(WSS13_2), !!!disagree_key)) %>%
  mutate(ind3 = recode(as.numeric(WSS13_3), !!!agree_key)) %>%
  mutate(ind4r = recode(as.numeric(WSS13_4), !!!disagree_key)) %>%
  # The fifth one doesn't have text.
  mutate(ind5r = recode(as.numeric(WSS13_6), !!!disagree_key)) %>%
  mutate(anxiety = recode(as.numeric(WSS14_1), !!!disagree_key)) %>%
  mutate(anger = recode(as.numeric(WSS14_2), !!!disagree_key)) %>%
  mutate(hope = recode(as.numeric(WSS14_3), !!!disagree_key)) %>%
  mutate(pride = recode(as.numeric(WSS14_4), !!!disagree_key)) %>%
  mutate(disgust = recode(as.numeric(WSS14_5), !!!disagree_key)) %>%
  mutate(enthusiasm = recode(as.numeric(WSS14_6), !!!disagree_key)) %>%
  mutate(urban_r1 = recode(as.numeric(WSS31_1), !!!agree_key)) %>%
  mutate(urban_r3 = recode(as.numeric(WSS31_2), !!!agree_key)) %>%
  mutate(urban_r3 = recode(as.numeric(WSS31_3), !!!agree_key)) %>%
  mutate(urban_r4 = recode(as.numeric(WSS31_4), !!!agree_key)) %>%
  mutate(trump_vote = voted) %>%
  mutate(age = 2020 - birthyr) %>%
  mutate(gen = car::recode(age, "18:39 = 'Millenial'; 40:59 = 'Gen X'; 60:80 = 'Boomer'; 80:92 = 'Greatest'")) %>%
  mutate(millenial = ifelse(gen == "Millenial", 1, 0)) %>%
  mutate(genx = ifelse(gen == "Gen X", 1, 0)) %>%
  mutate(boomer = ifelse(gen == "Boomer", 1, 0)) %>%
  mutate(greatest = ifelse(gen == "Greatest", 1, 0)) %>%
  mutate(female = recode(as.numeric(gender), `1` = 0, `2` = 1)) %>%
  mutate(white = car::recode(as.numeric(race), "1=1 ;  2:8=0")) %>%
  mutate(latino = ifelse(as.character(race) == "Hispanic", 1, 0)) %>%
  mutate(black = ifelse(as.character(race) == "Black", 1, 0)) %>%
  mutate(asian = ifelse(as.character(race) == "Asian", 1, 0)) %>%
  mutate(native = ifelse(as.character(race) == "Native American", 1, 0)) %>%
  mutate(twoplus = ifelse(as.character(race) == "Two or more races", 1, 0)) %>%
  mutate(other = ifelse(as.character(race) == "Other" | as.character(race) == "Middle Eastern", 1, 0)) %>%
  mutate(party3 = car::recode(as.numeric(pid3), "1= 1; 2= 3; 3 = 2; else=NA")) %>%
  mutate(pid7 = car::recode(as.numeric(pid7), "8:9 = NA")) %>%
  mutate(vote2016 = car::recode(as.numeric(presvote16post), "1=0; 2=1; else=NA")) %>%
  mutate(married = car::recode(as.numeric(marstat), "1=1; 2:6=0")) %>%
  mutate(college = car::recode(as.numeric(educ), "1:3=0; 4:8=1")) %>%
  mutate(income = car::recode(as.numeric(faminc_new), "1:7=0; 8:17=1")) %>%
  mutate(ideology = recode(as.numeric(ideo5), `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = NA_real_)) %>%
  mutate(christian = recode(as.numeric(religpew), `1` = 1, `2` = 1, `3` = 1, `4` = 1, `5` = 0, `6` = 0, `7` = 0, `8` = 0, `9` = 0, `10` = 0, `11` = 0, `12` = 0)) %>%
  mutate(interest = recode(as.numeric(newsint), `1` = 4, `2` = 3, `3` = 2, `4` = 1)) %>%
  mutate(auth1 = recode(as.numeric(WSS07_1a), `1` = 0, `2` = 1)) %>%
  mutate(auth2 = recode(as.numeric(WSS07_1b), `1` = 1, `2` = 0)) %>%
  mutate(auth3 = recode(as.numeric(WSS07_1c), `1` = 0, `2` = 1)) %>%
  mutate(auth4 = recode(as.numeric(WSS07_1d), `1` = 0, `2` = 1)) %>%
  mutate(az_t1 = recode(as.numeric(WSS28_split), `1` = 0, `2` = 1)) %>%
  mutate(az_rep_state = as.numeric(WSS28_1)) %>%
  mutate(az_rep_nat = as.numeric(WSS28_2)) %>%
  mutate(az_dem_state = as.numeric(WSS28_3)) %>%
  mutate(az_dem_nat = as.numeric(WSS28_4)) %>%
  mutate(VIOLENT = recode(as.numeric(WSS40_1_split), `1` = 1, `2` = 0)) %>%
  mutate(violent = recode(as.numeric(WSS40_1), !!!agree_key)) %>%
  mutate(burn = recode(as.numeric(WSS40_2), !!!agree_key)) %>%
  mutate(court = recode(as.numeric(WSS40_3), !!!agree_key)) %>%
  mutate(recount = recode(as.numeric(WSS40_4), !!!agree_key)) %>%
  mutate(criticize = recode(as.numeric(WSS40_5), !!!agree_key)) %>%
  mutate(SM = recode(as.numeric(WSS40_5_split), `1` = 1, `2` = 0)) %>%
  mutate(trustPres = case_when(
    as.numeric(WSS54a_1) == 1 ~ 4,
    as.numeric(WSS54a_1) == 2 ~ 3,
    as.numeric(WSS54a_1) == 3 ~ 2,
    as.numeric(WSS54a_1) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustCongress = case_when(
    as.numeric(WSS54a_2) == 1 ~ 4,
    as.numeric(WSS54a_2) == 2 ~ 3,
    as.numeric(WSS54a_2) == 3 ~ 2,
    as.numeric(WSS54a_2) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustSC = case_when(
    as.numeric(WSS54a_3) == 1 ~ 4,
    as.numeric(WSS54a_3) == 2 ~ 3,
    as.numeric(WSS54a_3) == 3 ~ 2,
    as.numeric(WSS54a_3) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustMyGov = case_when(
    as.numeric(WSS54a_4) == 1 ~ 4,
    as.numeric(WSS54a_4) == 2 ~ 3,
    as.numeric(WSS54a_4) == 3 ~ 2,
    as.numeric(WSS54a_4) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustMyLeg = case_when(
    as.numeric(WSS54a_5) == 1 ~ 4,
    as.numeric(WSS54a_5) == 2 ~ 3,
    as.numeric(WSS54a_5) == 3 ~ 2,
    as.numeric(WSS54a_5) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustPolice = case_when(
    as.numeric(WSS54a_6) == 1 ~ 4,
    as.numeric(WSS54a_6) == 2 ~ 3,
    as.numeric(WSS54a_6) == 3 ~ 2,
    as.numeric(WSS54a_6) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustScience = case_when(
    as.numeric(WSS54a_7) == 1 ~ 4,
    as.numeric(WSS54a_7) == 2 ~ 3,
    as.numeric(WSS54a_7) == 3 ~ 2,
    as.numeric(WSS54a_7) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(mip = case_when(
    as.numeric(WSS15s) == 1 ~ "environment",
    as.numeric(WSS15s) == 2 ~ "police",
    as.numeric(WSS15s) == 3 ~ "economy",
    as.numeric(WSS15s) == 4 ~ "housing",
    as.numeric(WSS15s) == 5 ~ "immigration",
    as.numeric(WSS15s) == 6 ~ "trade/foreign policy",
    as.numeric(WSS15s) == 7 ~ "trade/foreign policy",
    as.numeric(WSS15s) == 8 ~ "education",
    as.numeric(WSS15s) == 9 ~ "healthcare",
    as.numeric(WSS15s) == 10 ~ "covid",
    as.numeric(WSS15s) == 11 ~ "other",
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(psweight = weight) %>%
  ## Environment, water
  mutate(waterFish = case_when(
    as.numeric(WSS19_1) == 1 ~ 4,
    as.numeric(WSS19_1) == 2 ~ 3,
    as.numeric(WSS19_1) == 3 ~ 2,
    as.numeric(WSS19_1) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(envHabitat = case_when(
    as.numeric(WSS19_2) == 1 ~ 4,
    as.numeric(WSS19_2) == 2 ~ 3,
    as.numeric(WSS19_2) == 3 ~ 2,
    as.numeric(WSS19_2) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(envWater = case_when(
    as.numeric(WSS19_3) == 1 ~ 4,
    as.numeric(WSS19_3) == 2 ~ 3,
    as.numeric(WSS19_3) == 3 ~ 2,
    as.numeric(WSS19_3) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(envPollution = case_when(
    as.numeric(WSS19_4) == 1 ~ 4,
    as.numeric(WSS19_4) == 2 ~ 3,
    as.numeric(WSS19_4) == 3 ~ 2,
    as.numeric(WSS19_4) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(envTemp = case_when(
    as.numeric(WSS19_5) == 1 ~ 4,
    as.numeric(WSS19_5) == 2 ~ 3,
    as.numeric(WSS19_5) == 3 ~ 2,
    as.numeric(WSS19_5) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(waterDams = case_when(
    as.numeric(WSS20_1) == 1 ~ 5,
    as.numeric(WSS20_1) == 2 ~ 4,
    as.numeric(WSS20_1) == 3 ~ 3,
    as.numeric(WSS20_1) == 4 ~ 2,
    as.numeric(WSS20_1) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(waterConservation = case_when(
    as.numeric(WSS20_2) == 1 ~ 5,
    as.numeric(WSS20_2) == 2 ~ 4,
    as.numeric(WSS20_2) == 3 ~ 3,
    as.numeric(WSS20_2) == 4 ~ 2,
    as.numeric(WSS20_2) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(publicConservation = case_when(
    as.numeric(WSS22) == 1 ~ "extraction",
    as.numeric(WSS22) == 2 ~ "renewable",
    as.numeric(WSS22) == 3 ~ "recreation",
    as.numeric(WSS22) == 4 ~ "protection",
    as.numeric(WSS22) == 5 ~ "skipped",
    # Missing
    TRUE ~ NA
  )) %>%
  # Immigration, always conservative direction
  mutate(immUndocumented = case_when(
    as.numeric(WSS23_1) == 1 ~ 1,
    as.numeric(WSS23_1) == 2 ~ 2,
    as.numeric(WSS23_1) == 3 ~ 3,
    as.numeric(WSS23_1) == 4 ~ 4,
    as.numeric(WSS23_1) == 5 ~ 5,
    # Missing
    TRUE ~ NA
  )) %>%
  # Immigration, always conservative direction
  mutate(immSeparation = case_when(
    as.numeric(WSS23_2) == 1 ~ 1,
    as.numeric(WSS23_2) == 2 ~ 2,
    as.numeric(WSS23_2) == 3 ~ 3,
    as.numeric(WSS23_2) == 4 ~ 4,
    as.numeric(WSS23_2) == 5 ~ 5,
    # Missing
    TRUE ~ NA
  )) %>%
  # Immigration, always conservative direction
  mutate(immAsylum = case_when(
    as.numeric(WSS23_3) == 1 ~ 5,
    as.numeric(WSS23_3) == 2 ~ 4,
    as.numeric(WSS23_3) == 3 ~ 3,
    as.numeric(WSS23_3) == 4 ~ 2,
    as.numeric(WSS23_3) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Immigration, always conservative direction
  mutate(immBirthright = case_when(
    as.numeric(WSS23_4) == 1 ~ 5,
    as.numeric(WSS23_4) == 2 ~ 4,
    as.numeric(WSS23_4) == 3 ~ 3,
    as.numeric(WSS23_4) == 4 ~ 2,
    as.numeric(WSS23_4) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Immigration, always conservative direction
  mutate(immTech = case_when(
    as.numeric(WSS23_5) == 1 ~ 5,
    as.numeric(WSS23_5) == 2 ~ 4,
    as.numeric(WSS23_5) == 3 ~ 3,
    as.numeric(WSS23_5) == 4 ~ 2,
    as.numeric(WSS23_5) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Immigration, always conservative direction
  mutate(immWall = case_when(
    as.numeric(WSS23_6) == 1 ~ 5,
    as.numeric(WSS23_6) == 2 ~ 4,
    as.numeric(WSS23_6) == 3 ~ 3,
    as.numeric(WSS23_6) == 4 ~ 2,
    as.numeric(WSS23_6) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Immigration, always conservative direction
  mutate(immResources = case_when(
    as.numeric(WSS23_7) == 1 ~ 1,
    as.numeric(WSS23_7) == 2 ~ 2,
    as.numeric(WSS23_7) == 3 ~ 3,
    as.numeric(WSS23_7) == 4 ~ 4,
    as.numeric(WSS23_7) == 5 ~ 5,
    # Missing
    TRUE ~ NA
  )) %>%
  # Law enforcement, guns
  mutate(police = case_when(
    as.numeric(WSS25) == 1 ~ 5,
    as.numeric(WSS25) == 2 ~ 4,
    as.numeric(WSS25) == 3 ~ 3,
    as.numeric(WSS25) == 4 ~ 2,
    as.numeric(WSS25) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Law enforcement, guns
  mutate(gunHome = case_when(
    as.numeric(WSS26) == 1 ~ 1,
    as.numeric(WSS26) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  # Law enforcement, guns
  mutate(concealCarry = case_when(
    as.numeric(WSS27_1) == 1 ~ 5,
    as.numeric(WSS27_1) == 2 ~ 4,
    as.numeric(WSS27_1) == 3 ~ 3,
    as.numeric(WSS27_1) == 4 ~ 2,
    as.numeric(WSS27_1) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Law enforcement, guns
  mutate(background = case_when(
    as.numeric(WSS27_2) == 1 ~ 1,
    as.numeric(WSS27_2) == 2 ~ 2,
    as.numeric(WSS27_2) == 3 ~ 3,
    as.numeric(WSS27_2) == 4 ~ 4,
    as.numeric(WSS27_2) == 5 ~ 5,
    # Missing
    TRUE ~ NA
  )) %>%
  # National registry
  mutate(registry = case_when(
    as.numeric(WSS27_3) == 1 ~ 1,
    as.numeric(WSS27_3) == 2 ~ 2,
    as.numeric(WSS27_3) == 3 ~ 3,
    as.numeric(WSS27_3) == 4 ~ 4,
    as.numeric(WSS27_3) == 5 ~ 5,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(polMeeting = case_when(
    as.numeric(WSS32_1) == 1 ~ 1,
    as.numeric(WSS32_1) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  # Rural urban resentment
  mutate(polSign = case_when(
    as.numeric(WSS32_2) == 1 ~ 1,
    as.numeric(WSS32_2) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(polVolunteer = case_when(
    as.numeric(WSS32_3) == 1 ~ 1,
    as.numeric(WSS32_3) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(polProtest = case_when(
    as.numeric(WSS32_4) == 1 ~ 1,
    as.numeric(WSS32_4) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(poLOfficial = case_when(
    as.numeric(WSS32_5) == 1 ~ 1,
    as.numeric(WSS32_5) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(polDonate = case_when(
    as.numeric(WSS32_6) == 1 ~ 1,
    as.numeric(WSS32_6) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(polSocial = case_when(
    as.numeric(WSS32_7) == 1 ~ 1,
    as.numeric(WSS32_7) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(polPersuade = case_when(
    as.numeric(WSS32_8) == 1 ~ 1,
    as.numeric(WSS32_8) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(polNone = case_when(
    as.numeric(WSS32_9) == 1 ~ 1,
    as.numeric(WSS32_9) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  # 1 = State, 0 national
  mutate(tEfficacy = case_when(
    as.numeric(WSS33_split) == 1 ~ 1,
    as.numeric(WSS33_split) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  # Eff1: internal; Eff2: external
  mutate(eff1 = case_when(
    as.numeric(WSS33_1) == 1 ~ 1,
    as.numeric(WSS33_1) == 2 ~ 2,
    as.numeric(WSS33_1) == 3 ~ 3,
    as.numeric(WSS33_1) == 4 ~ 4,
    as.numeric(WSS33_1) == 5 ~ 5,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(eff2 = case_when(
    as.numeric(WSS33_2) == 1 ~ 1,
    as.numeric(WSS33_2) == 2 ~ 2,
    as.numeric(WSS33_2) == 3 ~ 3,
    as.numeric(WSS33_2) == 4 ~ 4,
    as.numeric(WSS33_2) == 5 ~ 5,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(buycott1 = case_when(
    as.numeric(WSS34_a) == 1 ~ 1,
    as.numeric(WSS34_a) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(buycott2 = case_when(
    as.numeric(WSS34_b) == 1 ~ 1,
    as.numeric(WSS34_b) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(completeCensus = case_when(
    as.numeric(WSS35) == 1 ~ "Yes",
    as.numeric(WSS35) == 2 ~ "Yes-Someone else",
    as.numeric(WSS35) == 3 ~ "No",
    as.numeric(WSS35) == 4 ~ "I don't recall",
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(censusAccurate = case_when(
    as.numeric(WSS35_b) == 1 ~ 4,
    as.numeric(WSS35_b) == 2 ~ 3,
    as.numeric(WSS35_b) == 3 ~ 2,
    as.numeric(WSS35_b) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(ballotType1 = case_when(
    as.numeric(WSS36_e) == 1 ~ "In Person",
    as.numeric(WSS36_e) == 2 ~ "Mail",
    as.numeric(WSS36_e) == 3 ~ "Drop Off",
    as.numeric(WSS36_e) == 4 ~ "Absentee",
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(ballotType2 = case_when(
    as.numeric(WSS36_d) == 1 ~ "In Person",
    as.numeric(WSS36_d) == 2 ~ "Mail",
    as.numeric(WSS36_d) == 3 ~ "Drop Off",
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(ballotType = ifelse(is.na(ballotType2), ballotType1, ballotType2)) %>%
  mutate(electionMail = case_when(
    as.numeric(WSS41_1) == 1 ~ 5,
    as.numeric(WSS41_1) == 2 ~ 4,
    as.numeric(WSS41_1) == 3 ~ 3,
    as.numeric(WSS41_1) == 4 ~ 2,
    as.numeric(WSS41_1) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(electionCovid = case_when(
    as.numeric(WSS41_2) == 1 ~ 5,
    as.numeric(WSS41_2) == 2 ~ 4,
    as.numeric(WSS41_2) == 3 ~ 3,
    as.numeric(WSS41_2) == 4 ~ 2,
    as.numeric(WSS41_2) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(electionLines = case_when(
    as.numeric(WSS41_3) == 1 ~ 5,
    as.numeric(WSS41_3) == 2 ~ 4,
    as.numeric(WSS41_3) == 3 ~ 3,
    as.numeric(WSS41_3) == 4 ~ 2,
    as.numeric(WSS41_3) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(electionIntimidate = case_when(
    as.numeric(WSS41_4) == 1 ~ 5,
    as.numeric(WSS41_4) == 2 ~ 4,
    as.numeric(WSS41_4) == 3 ~ 3,
    as.numeric(WSS41_4) == 4 ~ 2,
    as.numeric(WSS41_4) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(electionConcede = case_when(
    as.numeric(WSS41_5) == 1 ~ 5,
    as.numeric(WSS41_5) == 2 ~ 4,
    as.numeric(WSS41_5) == 3 ~ 3,
    as.numeric(WSS41_5) == 4 ~ 2,
    as.numeric(WSS41_5) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(electionFraud = case_when(
    as.numeric(WSS41_6) == 1 ~ 5,
    as.numeric(WSS41_6) == 2 ~ 4,
    as.numeric(WSS41_6) == 3 ~ 3,
    as.numeric(WSS41_6) == 4 ~ 2,
    as.numeric(WSS41_6) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Threat
  mutate(covidPersonal = case_when(
    as.numeric(WSS42_1) == 1 ~ 1,
    as.numeric(WSS42_1) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(covidFamily = case_when(
    as.numeric(WSS42_2) == 1 ~ 1,
    as.numeric(WSS42_2) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(covidFriend = case_when(
    as.numeric(WSS42_3) == 1 ~ 1,
    as.numeric(WSS42_3) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(covidIncome = case_when(
    as.numeric(WSS43_1) == 1 ~ 1,
    as.numeric(WSS43_1) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(covidInsurance = case_when(
    as.numeric(WSS43_2) == 1 ~ 1,
    as.numeric(WSS43_2) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(covidStimulus = case_when(
    as.numeric(WSS42_3) == 1 ~ 1,
    as.numeric(WSS42_3) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(covidFinancial = case_when(
    as.numeric(WSS43_4) == 1 ~ 1,
    as.numeric(WSS43_4) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(covidOther = case_when(
    as.numeric(WSS43_5) == 1 ~ 1,
    as.numeric(WSS43_5) == 2 ~ 0,
    # Missing
    TRUE ~ NA
  )) %>%
  # Courts, high = more conservative
  mutate(legCourt = case_when(
    as.numeric(WSS49_1) == 1 ~ 5,
    as.numeric(WSS49_1) == 2 ~ 4,
    as.numeric(WSS49_1) == 3 ~ 3,
    as.numeric(WSS49_1) == 4 ~ 2,
    as.numeric(WSS49_1) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(legPolitics = case_when(
    as.numeric(WSS49_2) == 1 ~ 5,
    as.numeric(WSS49_2) == 2 ~ 4,
    as.numeric(WSS49_2) == 3 ~ 3,
    as.numeric(WSS49_2) == 4 ~ 2,
    as.numeric(WSS49_2) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(legPower = case_when(
    as.numeric(WSS49_3) == 1 ~ 5,
    as.numeric(WSS49_3) == 2 ~ 4,
    as.numeric(WSS49_3) == 3 ~ 3,
    as.numeric(WSS49_3) == 4 ~ 2,
    as.numeric(WSS49_3) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(legPower = case_when(
    as.numeric(WSS49_4) == 1 ~ 5,
    as.numeric(WSS49_4) == 2 ~ 4,
    as.numeric(WSS49_4) == 3 ~ 3,
    as.numeric(WSS49_4) == 4 ~ 2,
    as.numeric(WSS49_4) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Trust, high = trust
  mutate(trustLeg = case_when(
    as.numeric(WSS54_1) == 1 ~ 4,
    as.numeric(WSS54_1) == 2 ~ 3,
    as.numeric(WSS54_1) == 3 ~ 2,
    as.numeric(WSS54_1) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustPres = case_when(
    as.numeric(WSS54_2) == 1 ~ 4,
    as.numeric(WSS54_2) == 2 ~ 3,
    as.numeric(WSS54_2) == 3 ~ 2,
    as.numeric(WSS54_2) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustSC = case_when(
    as.numeric(WSS54_3) == 1 ~ 4,
    as.numeric(WSS54_3) == 2 ~ 3,
    as.numeric(WSS54_3) == 3 ~ 2,
    as.numeric(WSS54_3) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustGov = case_when(
    as.numeric(WSS54_4) == 1 ~ 4,
    as.numeric(WSS54_4) == 2 ~ 3,
    as.numeric(WSS54_4) == 3 ~ 2,
    as.numeric(WSS54_4) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustStateleg = case_when(
    as.numeric(WSS54_5) == 1 ~ 4,
    as.numeric(WSS54_5) == 2 ~ 3,
    as.numeric(WSS54_5) == 3 ~ 2,
    as.numeric(WSS54_5) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(trustPolice = case_when(
    as.numeric(WSS54_6) == 1 ~ 4,
    as.numeric(WSS54_6) == 2 ~ 3,
    as.numeric(WSS54_6) == 3 ~ 2,
    as.numeric(WSS54_6) == 4 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  # Your views and beliefs, most, women, minorities
  mutate(myLeg1 = case_when(
    as.numeric(WSS57_1) == 1 ~ 5,
    as.numeric(WSS57_1) == 2 ~ 4,
    as.numeric(WSS57_1) == 3 ~ 3,
    as.numeric(WSS57_1) == 4 ~ 2,
    as.numeric(WSS57_1) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(myLeg2 = case_when(
    as.numeric(WSS57_2) == 1 ~ 5,
    as.numeric(WSS57_2) == 2 ~ 4,
    as.numeric(WSS57_2) == 3 ~ 3,
    as.numeric(WSS57_2) == 4 ~ 2,
    as.numeric(WSS57_2) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(myLeg3 = case_when(
    as.numeric(WSS57_3) == 1 ~ 5,
    as.numeric(WSS57_3) == 2 ~ 4,
    as.numeric(WSS57_3) == 3 ~ 3,
    as.numeric(WSS57_3) == 4 ~ 2,
    as.numeric(WSS57_3) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  )) %>%
  mutate(myLeg4 = case_when(
    as.numeric(WSS57_4) == 1 ~ 5,
    as.numeric(WSS57_4) == 2 ~ 4,
    as.numeric(WSS57_4) == 3 ~ 3,
    as.numeric(WSS57_4) == 4 ~ 2,
    as.numeric(WSS57_4) == 5 ~ 1,
    # Missing
    TRUE ~ NA
  ))
# NUM = 5
# XVAR = "Test"

# function(XVAR = WSS23_7, NUM = 5) {

# tt = c()
# for(i in 1:NUM){
#      paste0("as.numeric(", df$WSS23_7, ")", "==", i, "~", NUM + 1 - i) -> tt[i]
# }

# mutate(immTech = case_when(
#   as.numeric(WSS23_7) == 1 ~ 1,
#   as.numeric(WSS23_7) == 2 ~ 2,
#   as.numeric(WSS23_7) == 3 ~ 3,
#   as.numeric(WSS23_7) == 4 ~ 4,
#   as.numeric(WSS23_7) == 5 ~ 5,
#   # Missing
#   TRUE ~ NA
# ))


# CO
# 3 NV
# 4 NM
# 5 UT

df$state <- recode(as.numeric(df$state_residence), `3` = 1, `6` = 2, `29` = 3, `32` = 4, `45` = 5)
df$rwm <- 0
df$rwm[df$WSS58_open == "A non blue supportive source"] <- 1
df$rwm[df$WSS58_open == "Alex Jones News Max"] <- 1
df$rwm[df$WSS58_open == "All media lies for the democrats"] <- 1
df$rwm[df$WSS58_open == "American Thinker, The Last Refuge"] <- 1
df$rwm[df$WSS58_open == "Any non-Democrat supporting cable news, like OANN"] <- 1
df$rwm[df$WSS58_open == "American Thinker, The Last Refuge"] <- 1
df$rwm[df$WSS58_open == "Bill O'Reilly"] <- 1
df$rwm[df$WSS58_open == "Bill O'Reilly.com "] <- 1
df$rwm[df$WSS58_open == "Bill O'reilly"] <- 1
df$rwm[df$WSS58_open == "Blaze, Epoch Times"] <- 1
df$rwm[df$WSS58_open == "Bllaze, oann, Breibart, the daily cal.."] <- 1
df$rwm[df$WSS58_open == "Bongino"] <- 1
df$rwm[df$WSS58_open == "Breightbart and other news sites"] <- 1
df$rwm[df$WSS58_open == "Breitbart"] <- 1
df$rwm[df$WSS58_open == "Breitbart, Daily Mail, NewsMax"] <- 1
df$rwm[df$WSS58_open == "Breitbart, Epoch Times"] <- 1
df$rwm[df$WSS58_open == "Breitbart.com"] <- 1
df$rwm[df$WSS58_open == "Breitbart.com, One America News"] <- 1
df$rwm[df$WSS58_open == "Breitbart.com, The Blaze, OANN.com"] <- 1
df$rwm[df$WSS58_open == "Brietbart"] <- 1
df$rwm[df$WSS58_open == "Conservative Websites"] <- 1
df$rwm[df$WSS58_open == "Conservative radio. Rush Limbaugh #1"] <- 1
df$rwm[df$WSS58_open == "Conservative talk radio"] <- 1
df$rwm[df$WSS58_open == "Daily Wire"] <- 1
df$rwm[df$WSS58_open == "Daily wire"] <- 1
df$rwm[df$WSS58_open == "Daily wire, Breitbart"] <- 1
df$rwm[df$WSS58_open == "Dan Bongino"] <- 1
df$rwm[df$WSS58_open == "Dr. Steve Turley"] <- 1
df$rwm[df$WSS58_open == "Ephich Times"] <- 1
df$rwm[df$WSS58_open == "Epoch Times"] <- 1
df$rwm[df$WSS58_open == "Epoch Times Newspaper"] <- 1
df$rwm[df$WSS58_open == "Epoch Times, BBC America"] <- 1
df$rwm[df$WSS58_open == "Epoch Times, Federalist, BBC"] <- 1
df$rwm[df$WSS58_open == "Epoch Times, The Federalist"] <- 1
df$rwm[df$WSS58_open == "Epoch times"] <- 1
df$rwm[df$WSS58_open == "Epoch, CBN"] <- 1
df$rwm[df$WSS58_open == "Epoch, Denver Gazette"] <- 1
df$rwm[df$WSS58_open == "Fox Business Channel, Newsmax"] <- 1
df$rwm[df$WSS58_open == "Gateway pundit, breitbart"] <- 1
df$rwm[df$WSS58_open == "LUCIANNE, REVOLVER"] <- 1
df$rwm[df$WSS58_open == "Local Conservative radio stations"] <- 1
df$rwm[df$WSS58_open == "New York Post, GodLikeProductions, NaturalNews"] <- 1
df$rwm[df$WSS58_open == "News Max"] <- 1
df$rwm[df$WSS58_open == "NewsMax"] <- 1
df$rwm[df$WSS58_open == "NewsMax & The Blaze Network"] <- 1
df$rwm[df$WSS58_open == "Newsmax"] <- 1
df$rwm[df$WSS58_open == "Newsmax TV"] <- 1
df$rwm[df$WSS58_open == "Newsmax and OANN"] <- 1
df$rwm[df$WSS58_open == "Newsmax, Epoch Times"] <- 1
df$rwm[df$WSS58_open == "Newsmax, OANN, Internet news sites"] <- 1
df$rwm[df$WSS58_open == "OAN"] <- 1
df$rwm[df$WSS58_open == "OAN N"] <- 1
df$rwm[df$WSS58_open == "OAN Newsmax"] <- 1
df$rwm[df$WSS58_open == "OAN, C-SPAN"] <- 1
df$rwm[df$WSS58_open == "OAN, NEWSMAX"] <- 1
df$rwm[df$WSS58_open == "OAN, Newsman"] <- 1
df$rwm[df$WSS58_open == "OAN, Newsmax"] <- 1
df$rwm[df$WSS58_open == "OANN"] <- 1
df$rwm[df$WSS58_open == "OANN, Censored.news, etc"] <- 1
df$rwm[df$WSS58_open == "OANN, Newsmax, blogs"] <- 1
df$rwm[df$WSS58_open == "Oan news/ variety of blogs"] <- 1
df$rwm[df$WSS58_open == "Oann"] <- 1
df$rwm[df$WSS58_open == "One America News"] <- 1
df$rwm[df$WSS58_open == "One America News , newsmax"] <- 1
df$rwm[df$WSS58_open == "One America News Network"] <- 1
df$rwm[df$WSS58_open == "One America News, NewsMax"] <- 1
df$rwm[df$WSS58_open == "OneAmericaNews"] <- 1
df$rwm[df$WSS58_open == "POANN"] <- 1
df$rwm[df$WSS58_open == "Private Conservative news sources"] <- 1
df$rwm[df$WSS58_open == "Q"] <- 1
df$rwm[df$WSS58_open == "Quanon"] <- 1
df$rwm[df$WSS58_open == "Redstate, Daily Caller, Townhall, Breitbart,etc."] <- 1
df$rwm[df$WSS58_open == "Rush Limbaugh"] <- 1
df$rwm[df$WSS58_open == "Steven Crowder"] <- 1
df$rwm[df$WSS58_open == "The AP, Epoch Times, BBC"] <- 1
df$rwm[df$WSS58_open == "The Blaze"] <- 1
df$rwm[df$WSS58_open == "The Daily Wire"] <- 1
df$rwm[df$WSS58_open == "The Epoch Times"] <- 1
df$rwm[df$WSS58_open == "The blaze tv"] <- 1
df$rwm[df$WSS58_open == "Townhall.com"] <- 1
df$rwm[df$WSS58_open == "Washington Examiner"] <- 1
df$rwm[df$WSS58_open == "Wnd, EpochThimes, other trusted aggregators"] <- 1
df$rwm[df$WSS58_open == "breitbart"] <- 1
df$rwm[df$WSS58_open == "conservative sources"] <- 1
df$rwm[df$WSS58_open == "drudge report, yahoo news"] <- 1
df$rwm[df$WSS58_open == "epoch times"] <- 1
df$rwm[df$WSS58_open == "news max and world net daily"] <- 1
df$rwm[df$WSS58_open == "newsmax"] <- 1
df$rwm[df$WSS58_open == "newsmax oan"] <- 1

df$rwm[df$WSS58_open == "newsmax oan"] <- 1

### Item Characteristics ####
psych::alpha(cbind(
  df$state_insult, df$state_common,
  df$state_interest, df$state_we,
  df$state_partme, df$proud
)) ##
psych::alpha(cbind(
  df$moral_ind1, df$moral_ind2,
  df$moral_ind3, df$moral_ind4
)) ##
psych::alpha(cbind(
  df$sdo1, df$sdo2r,
  df$sdo3, df$sdo4r
), check.keys = TRUE) ##
psych::alpha(cbind(
  df$rr1, df$rr2r,
  df$rr3r, df$rr4
), check.keys = TRUE) ##
psych::alpha(cbind(
  df$emp1, df$emp2,
  df$emp3, df$emp4
), check.keys = TRUE) ##
psych::alpha(cbind(
  df$ind1, df$ind2r,
  df$ind3, df$ind4r, df$ind5r
), check.keys = TRUE) ##
psych::alpha(cbind(
  df$auth1, df$auth2,
  df$auth3, df$auth4
), check.keys = TRUE) ##
psych::alpha(cbind(
  df$urban_r1, df$urban_r2,
  df$urban_r3, df$urban_r4
), check.keys = TRUE) ##
psych::alpha(cbind(
  df$sdo1, df$sdo2r,
  df$sdo3, df$sdo4r
), check.keys = TRUE) ##
#### These are some psychological scales:
cor(df$anger, df$anxiety)

df$surveillance <- rowMeans(cbind(df$anger, df$anxiety, df$disgust), na.rm = T) %>% zero.one()
df$disposition <- rowMeans(cbind(df$hope, df$pride, df$enthusiasm), na.rm = T) %>% zero.one()

df$state_pride <- rowMeans(cbind(
  df$state_insult, df$state_common,
  df$state_interest, df$state_we,
  df$state_partme, df$proud
), na.rm = T) %>% zero.one()
df$moral_individualism <- rowMeans(cbind(
  df$moral_ind1, df$moral_ind2,
  df$moral_ind3, df$moral_ind4
), na.rm = T) %>% zero.one()
df$rr <- rowMeans(cbind(
  df$rr1, df$rr2r,
  df$rr3r, df$rr4
), na.rm = T) %>% zero.one()

df$group_empathy <- rowMeans(cbind(
  df$emp1, df$emp2,
  df$emp3, df$emp4
), na.rm = T) %>% zero.one()
df$individualism <- rowMeans(cbind(
  df$ind1, df$ind2r,
  df$ind3, df$ind4r, df$ind5r
), na.rm = T) %>% zero.one()
df$authoritarianism <- rowMeans(cbind(
  df$auth1, df$auth2,
  df$auth3, df$auth4
), na.rm = T)
df$urban_resentment <- rowMeans(cbind(
  df$urban_r1, df$urban_r2,
  df$urban_r3, df$urban_r4
), na.rm = T) %>% zero.one()
df$sdo <- rowMeans(cbind(
  df$sdo1, df$sdo2r,
  df$sdo3, df$sdo4r
), na.rm = T) %>% zero.one()


## dat = df %>% subset(select = c("pid", "authoritarianism", "violent", "burn", "court", "recount", "criticize", "VIOLENT", "SM"))
## write.csv(dat, file="~/Desktop/western.csv")
df$DATE <- as.Date(df$endtime, "%YY-%mm-%dd", tz = "MST")
df$post_election <- ifelse(df$DATE > "2020-11-03", 1, 0)

df$post_call <- ifelse(df$DATE >= "2020-11-07", 1, 0)
df$uncertainty <- ifelse(df$DATE > "2020-11-03", 1, 0)
df$treat <- NA
df$treat <- ifelse(df$post_call == 0 & df$uncertainty == 0, 1, df$treat)
df$treat <- ifelse(df$post_call == 0 & df$uncertainty == 1, 2, df$treat)
df$treat <- ifelse(df$post_call == 1 & df$uncertainty == 1, 3, df$treat)

df$strength <- abs(df$pid7 - 4) %>% zero.one()
write.csv(df, file = "/Users/Chris/Dropbox/masterData/Western_States/westernCleaned.csv")
gcs_upload_set_limit(300000000L)
uploadData(df, "Western_States/westernCleaned.csv")

# Set to 3 GB
gcs_upload_set_limit(300000000L)
# Use scientific notation in 3000 above
gcs_upload(file = df, bucket = "grau_data", name = "cWestern.csv")
