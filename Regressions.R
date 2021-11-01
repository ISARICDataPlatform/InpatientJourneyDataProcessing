#### Libraries ####

library(Rmisc)
library(data.table)
library(dtplyr)
library(tidyverse)
library(lubridate)
library(glue)
library(viridis)
library(magrittr)
library(ggupset)
library(survival)
library(survminer)
library(stargazer)
library(wesanderson)

#### Generation of individual datasets ####


load("processed_data.Rdata")

# order of epiweeks, for factor variables

epiweek.order <- c(glue("2019-{44:52}"), glue("2020-{1:53}"), glue("2021-{1:52}"))

# function to map a date to an epiweek and year (note that e.g. 1st January 2021 is in epiweek 53 of _2020_)

epiweek.year <- function(date){
  if(year(date)==2019 & date > ymd("2019-12-28")){
    2020
  } else if(year(date)==2021 & date < ymd("2021-01-03")){
    2020
  } else {
    year(date)
  }
}

# get the last date of data entry in an events table

get.last.seen <- function(cn, ev){
  if(!cn | (all(is.na(ev$daily_lbdat)) & all(is.na(ev$daily_dsstdat)))){
    NA
  } else {
    max(c(ev$daily_lbdat, ev$daily_dsstdat), na.rm = T) %>% as.character()
  }
}

complete.data <- patient.data %>%
  mutate(site.name = replace(site.name, site.name == subjid, NA)) %>%
  # recode outcome for censored/unknown
  mutate(outcome = replace(outcome, is.na(exit.date) & is.na(outcome), "censored")) %>%
  mutate(outcome = replace(outcome, is.na(outcome), "unknown")) %>%
  # get last seen dates
  mutate(last.seen = map2_chr(censored, events, get.last.seen)) %>%
  mutate(last.seen = as.Date(parse_date_time(last.seen, orders = c("ymd","dmy")))) %>%
  # patients with NA start dates are not useful
  filter(!is.na(start.date))  %>%
  mutate(epiweek.start = epiweek(start.date)) %>%
  mutate(epiweek.year.start = map_dbl(start.date, epiweek.year)) %>%
  # from epiweek 10 2020 onwards
  filter(epiweek.year.start >= 2020 & (epiweek.year.start == 2021 | epiweek.start >= 10)) %>%
  # age grouping for this analysis
  mutate(final.agegp= cut(consolidated.age, breaks = c(-1, 20, 40, 60, 70, 80, 150), right = FALSE)) %>%
  # year-epiweek
  mutate(year.epiweek.start = glue("{epiweek.year.start}-{epiweek.start}")) %>%
  mutate(year.epiweek.start = factor(year.epiweek.start, levels = epiweek.order)) %>%
  # text sex variable
  mutate(sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female",
                         TRUE ~ NA_character_)) %>%
  # factor age group variable
  mutate(nice.agegp = case_when(as.character(final.agegp) == "[-1,20)" ~ "0-19",
                                as.character(final.agegp) == "[20,40)" ~ "20-39",
                                as.character(final.agegp) == "[40,60)" ~ "40-59",
                                as.character(final.agegp) == "[60,70)" ~ "60-69",
                                as.character(final.agegp) == "[70,80)" ~ "70-79",
                                as.character(final.agegp) == "[80,150)" ~ "80+")) %>%
  mutate(nice.agegp = factor(nice.agegp, levels = c("0-19", "20-39", "40-59", "60-69", "70-79", "80+"))) %>%
  # new symptom coding
  mutate(symp.cough.any = cough.any) %>%
  mutate(symp.fever = case_when(fever_ceoccur_v2 == 1 ~ TRUE,
                                fever_ceoccur_v2 == 2 ~ FALSE,
                                TRUE ~ NA)) %>%
  mutate(symp.shortness.breath = case_when(shortness.breath == 1 ~ TRUE,
                                           shortness.breath == 2 ~ FALSE,
                                           TRUE ~ NA)) %>%
  mutate(symp.fatigue = case_when(fatigue_ceoccur_v2 == 1 ~ TRUE,
                                  fatigue_ceoccur_v2 == 2 ~ FALSE,
                                  TRUE ~ NA)) %>%
  mutate(symp.myalgia = case_when(myalgia_ceoccur_v2 == 1 ~ TRUE,
                                  myalgia_ceoccur_v2 == 2 ~ FALSE,
                                  TRUE ~ NA)) %>%
  mutate(symp.vomit = case_when(vomit_ceoccur_v2 == 1 ~ TRUE,
                                vomit_ceoccur_v2 == 2 ~ FALSE,
                                TRUE ~ NA)) %>%
  mutate(symp.confusion = case_when(confusion_ceoccur_v2 == 1 ~ TRUE,
                                    confusion_ceoccur_v2 == 2 ~ FALSE,
                                    TRUE ~ NA)) %>%
  mutate(symp.abdominal.pain = case_when(abdopain_ceoccur_v2 == 1 ~ TRUE,
                                         abdopain_ceoccur_v2 == 2 ~ FALSE,
                                         TRUE ~ NA)) %>%
  mutate(symp.diarrhoea = case_when(diarrhoea_ceoccur_v2 == 1 ~ TRUE,
                                    diarrhoea_ceoccur_v2 == 2 ~ FALSE,
                                    TRUE ~ NA)) %>%
  mutate(symp.headache = case_when(headache_ceoccur_v2 == 1 ~ TRUE,
                                   headache_ceoccur_v2 == 2 ~ FALSE,
                                   TRUE ~ NA)) %>%
  mutate(symp.loss.taste = case_when(ageusia_ceoccur_v2 == 1 ~ TRUE,
                                     ageusia_ceoccur_v2 == 2 ~ FALSE,
                                     TRUE ~ NA)) %>%
  mutate(symp.loss.smell = case_when(anosmia_ceoccur_v2 == 1 ~ TRUE,
                                     anosmia_ceoccur_v2 == 2 ~ FALSE,
                                     TRUE ~ NA)) %>%
  mutate(symp.wheezing = case_when(wheeze_ceoccur_v2 == 1 ~ TRUE,
                                   wheeze_ceoccur_v2 == 2 ~ FALSE,
                                   TRUE ~ NA)) %>%
  mutate(symp.runny.nose = case_when(runnynose_ceoccur_v2 == 1 ~ TRUE,
                                     runnynose_ceoccur_v2 == 2 ~ FALSE,
                                     TRUE ~ NA)) %>%
  mutate(symp.seizures = case_when(seizures_cecoccur_v2 == 1 ~ TRUE,
                                   seizures_cecoccur_v2 == 2 ~ FALSE,
                                   TRUE ~ NA)) %>%
  mutate(symp.rash = case_when(rash_ceoccur_v2 == 1 ~ TRUE,
                               rash_ceoccur_v2 == 2 ~ FALSE,
                               TRUE ~ NA)) %>%
  mutate(symp.ulcers = case_when(skinulcers_ceoccur_v2 == 1 ~ TRUE,
                                 skinulcers_ceoccur_v2 == 2 ~ FALSE,
                                 TRUE ~ NA)) %>%
  mutate(symp.bleeding = case_when(bleed_ceoccur_v2 == 1 ~ TRUE,
                                   bleed_ceoccur_v2 == 2 ~ FALSE,
                                   TRUE ~ NA)) %>%
  mutate(symp.lymphadenopathy = case_when(lymp_ceoccur_v2 == 1 ~ TRUE,
                                          lymp_ceoccur_v2 == 2 ~ FALSE,
                                          TRUE ~ NA)) %>%
  mutate(symp.earpain = case_when(earpain_ceoccur_v2 == 1 ~ TRUE,
                                  earpain_ceoccur_v2 == 2 ~ FALSE,
                                  TRUE ~ NA)) %>%
  mutate(symp.conjunctivitis = case_when(conjunct_ceoccur_v2 == 1 ~ TRUE,
                                         conjunct_ceoccur_v2 == 2 ~ FALSE,
                                         TRUE ~ NA)) %>%
  mutate(comorb.chronic.cardiac = case_when(chrincard == 1 ~ TRUE,
                                            chrincard == 2 ~ FALSE,
                                            TRUE ~ NA)) %>%
  mutate(comorb.diabetes = case_when(diabetes == 1 ~ TRUE,
                                     diabetes == 2 ~ FALSE,
                                     TRUE ~ NA))  %>%
  mutate(comorb.hypertension = case_when(hypertension_mhyn == 1 ~ TRUE,
                                         hypertension_mhyn == 2 ~ FALSE,
                                         TRUE ~ NA))  %>%
  mutate(comorb.pulmonary = case_when(chronicpul_mhyn == 1 ~ TRUE,
                                      chronicpul_mhyn == 2 ~ FALSE,
                                      TRUE ~ NA))  %>%
  mutate(comorb.kidney = case_when(renal_mhyn == 1 ~ TRUE,
                                   renal_mhyn == 2 ~ FALSE,
                                   TRUE ~ NA))  %>%
  mutate(comorb.kidney = case_when(renal_mhyn == 1 ~ TRUE,
                                   renal_mhyn == 2 ~ FALSE,
                                   TRUE ~ NA))  %>%
  mutate(comorb.obesity = case_when(obesity_mhyn == 1 ~ TRUE,
                                    obesity_mhyn == 2 ~ FALSE,
                                    TRUE ~ NA))  %>%
  mutate(comorb.asthma = case_when(asthma_mhyn == 1 ~ TRUE,
                                   asthma_mhyn == 2 ~ FALSE,
                                   TRUE ~ NA))  %>%
  mutate(comorb.dementia = case_when(dementia_mhyn == 1 ~ TRUE,
                                     dementia_mhyn == 2 ~ FALSE,
                                     TRUE ~ NA))  %>%
  mutate(comorb.neuro = case_when(chronicneu_mhyn == 1 ~ TRUE,
                                  chronicneu_mhyn == 2 ~ FALSE,
                                  TRUE ~ NA))  %>%
  mutate(comorb.rheuma = case_when(rheumatologic_mhyn == 1 ~ TRUE,
                                   rheumatologic_mhyn == 2 ~ FALSE,
                                   TRUE ~ NA))  %>%
  mutate(comorb.cancer = case_when(malignantneo_mhyn == 1 ~ TRUE,
                                   malignantneo_mhyn == 2 ~ FALSE,
                                   TRUE ~ NA))  %>%
  mutate(comorb.smoking = case_when(smoking_mhyn == 1 ~ TRUE,
                                    smoking_mhyn == 2 ~ FALSE,
                                    TRUE ~ NA))  %>%
  mutate(comorb.haemo = case_when(chronichaemo_mhyn == 1 ~ TRUE,
                                  chronichaemo_mhyn == 2 ~ FALSE,
                                  TRUE ~ NA))  %>%
  mutate(comorb.liver = case_when(liver.disease == 1 ~ TRUE,
                                  liver.disease == 2 ~ FALSE,
                                  TRUE ~ NA))  %>%
  mutate(comorb.malnut = case_when(malnutrition_mhyn == 1 ~ TRUE,
                                   malnutrition_mhyn == 2 ~ FALSE,
                                   TRUE ~ NA))  %>%
  mutate(comorb.liver = case_when(liver.disease == 1 ~ TRUE,
                                  liver.disease == 2 ~ FALSE,
                                  TRUE ~ NA))  %>%
  mutate(comorb.pregnancy = case_when(pregnancy == 1 ~ TRUE,
                                      pregnancy == 2 ~ FALSE,
                                      TRUE ~ NA))  %>%
  mutate(comorb.hiv = case_when(aidshiv_mhyn == 1 ~ TRUE,
                                aidshiv_mhyn == 2 ~ FALSE,
                                TRUE ~ NA)) %>%
  # replace nonsensical time to ICU values with NA ICU ever variable
  mutate(ICU.ever = replace(ICU.ever, ICU.ever & (start.to.ICU < 0 | start.to.ICU > start.to.exit), F))

# this is big and we no longer need it
rm(patient.data)

# factor months of onset and start; nosocomial variable

complete.data <- complete.data %>%
  mutate(month.onset = month(onset.date, label = T,abbr = F)) %>%
  mutate(month.onset = factor(month.onset, ordered = F, levels = c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
  mutate(month.start = month(start.date, label = T,abbr = F)) %>%
  mutate(month.start = factor(month.start, ordered = F, levels = c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
  mutate(nice.agegp = factor(nice.agegp, ordered = F, levels = c("40-59", "0-19", "20-39", "60-69", "70-79", "80+"))) %>%
  mutate(nosocomial = onset.date > admission.date)

# function to obtain sites for some datasets
site.numberer <- function(x,y){
  if (startsWith(x, "F")) {
    str_split_fixed(x, "-", Inf)[1]
  } else if (startsWith(x, "A-A")) {
    substr(x, 1,12)
  } else {
    str_match(y, "([0-9]+)[a-zA-Z_].*")[, 2]
  }
}

# tidy up some site names manually

complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "00570"))] <- "ApolloHospitalsChennai"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "F309"))] <- "F309 Marseille Conception"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "F320"))] <- "F320 Colombes- Louis Mourier"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "F324"))] <- "F324 Paris - Robert Debre"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "F327"))] <- "F327 Le Havre"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "F329"))] <- "F327 Le Havre"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "R1H12"))] <- "barts_health_nhs_t"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RA201"))] <- "royal_surrey_nhsft"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RK5BC"))] <- "sherwood_forest_ho"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RMC01"))] <- "bolton_nhst"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RTD02"))] <- "the_newcastle_upon"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RVV09"))] <- "east_kent_hospital"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RW602"))] <- "the_pennine_acute"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RWEAA"))] <- "university_hospitab"
complete.data$site.name[which(is.na(complete.data$site.name) & startsWith(complete.data$subjid, "RXL01"))] <- "blackpool_teaching"

# there are two site lists which are both needed to get all this right. Available with the dataset

site.list.1 <- read_csv("")
site.list.2 <- read_csv("")

# More code to get all the countries right

country.lookup <- complete.data %>%
  select(subjid, redcap_data_access_group, data.source) %>%
  dplyr::mutate(site.number = map2_chr(subjid, redcap_data_access_group, site.numberer))  %>%
  left_join(site.list.1 %>% select(site.number, site.name, Country), by = "site.number") %>%
  left_join(site.list.2 %>%
              filter(!is.na(site.number)) %>%
              filter(!duplicated(site.number)) %>%
              select(site.number, site.name, Country),
            by = "site.number") %>%
  mutate(Country.x = replace(Country.x, data.source=="UK", "UK")) %>%
  mutate(Country.y = replace(Country.y, data.source=="UK", "UK")) %>%
  mutate(Country = map2_chr(Country.x, Country.y, function(Cx, Cy){
    if(is.na(Cx) & is.na(Cy)){
      NA
    } else if(is.na(Cx)){
      Cy
    } else if (is.na(Cy)){
      Cx
    } else if (Cx==Cy){
      Cx
    } else if (Cx == "United States"){
      Cx
    } else if (Cy == "France" | Cy == "Taiwan" | Cy == "South Korea" | Cy == "Viet Nam"){
      Cy
    } else {
      NA
    }
  })) %>%
  as_tibble()

country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$site.number, "F"))] <- "France"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "559")] <- "Ireland"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$subjid, "00801"))] <- "Portugal"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "00802")] <- "Brazil"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "00804")] <- "Indonesia"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "00815")] <- "Indonesia"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "00821")] <- "Mexico"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "00824")] <- "Indonesia"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "00806")] <- "Norway"
country.lookup$Country[which(is.na(country.lookup$Country) & country.lookup$site.number == "00814")] <- "Laos"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$site.number, "A-AE"))] <- "Nepal"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$site.number, "A-AF"))] <- "Pakistan"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$site.number, "A-AA"))] <- "India"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$site.number, "A-AD"))] <- "Malaysia"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$subjid, "0801"))] <- "Portugal"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$subjid, "00570"))] <- "India"
country.lookup$Country[which(is.na(country.lookup$Country) & startsWith(country.lookup$subjid, "R1H12"))] <- "UK"


# count of common and GI symptoms

complete.data <- complete.data %>%
  select(-Country) %>%
  left_join(country.lookup %>% select(subjid, Country))  %>%
  mutate(Country = replace(Country, Country == "United States", "United States of America"))

recorded.symptom.counter <- complete.data %>%
  select(subjid | (starts_with("symp.")))

recorded.symptom.counter <- recorded.symptom.counter %>%
  pivot_longer(2:ncol(recorded.symptom.counter), values_to="symptom.count") %>%
  group_by(subjid) %>%
  summarise(symptom.count = sum(symptom.count, na.rm = T))

complete.data<- complete.data %>%
  left_join(recorded.symptom.counter, by="subjid") %>%
  filter(symptom.count >= 1)

complete.data %>%
  select(symp.shortness.breath, symp.wheezing, symp.cough.any, symp.runny.nose) %>%
  rowSums(na.rm = TRUE) ->
  complete.data$symp.n.respiratory
complete.data %>%
  select(symp.shortness.breath, symp.wheezing, symp.cough.any, symp.runny.nose) %>%
  rowSums(na.rm = FALSE) %>%
  is.na %>%
  not ->
  complete.data$symp.all.respiratory.recorded
complete.data %>%
  select(symp.diarrhoea, symp.abdominal.pain, symp.vomit) %>%
  rowSums(na.rm = TRUE) ->
  complete.data$symp.n.gi
complete.data %>%
  select(symp.diarrhoea, symp.abdominal.pain, symp.vomit) %>%
  rowSums(na.rm = FALSE) %>%
  is.na %>%
  not ->
  complete.data$symp.all.gi.recorded
complete.data %>%
  select(symp.shortness.breath, symp.fever, symp.cough.any, symp.fatigue) %>%
  rowSums(na.rm=TRUE) ->
  complete.data$symp.n.common
complete.data %>%
  select(symp.shortness.breath, symp.fever, symp.cough.any, symp.fatigue) %>%
  rowSums(na.rm=FALSE) %>%
  is.na %>%
  not ->
  complete.data$symp.all.common.recorded

# Format and order symptoms and comorbidities

comorb.cols <- colnames(complete.data)[which(startsWith(colnames(complete.data), "comorb."))]
symp.cols <- colnames(complete.data)[which(startsWith(colnames(complete.data), "symp."))]

nice.comorb.cols <- c("Chronic cardiac disease",
                      "Diabetes",
                      "Hypertension",
                      "Chronic pulmonary disease",
                      "Chronic kidney disease",
                      "Obesity",
                      "Asthma",
                      "Dementia",
                      "Chronic neurological disorder",
                      "Rheumatological disorder",
                      "Malignant neoplasm",
                      "Smoking",
                      "Chronic haemotologic disease",
                      "Liver disease",
                      "Malnutrition",
                      "Pregnancy",
                      "HIV/AIDS"
)

nice.symp.cols <- c("Cough",
                    "Fever",
                    "Shortness of breath",
                    "Fatigue",
                    "Myalgia",
                    "Vomiting",
                    "Confusion",
                    "Abdominal pain",
                    "Diarrhoea",
                    "Headache",
                    "Ageusia",
                    "Anosmia",
                    "Wheezing",
                    "Runny nose",
                    "Seizures",
                    "Rash",
                    "Ulcers",
                    "Bleeding",
                    "Lymphadenopathy",
                    "Ear pain",
                    "Conjunctivitis"
)


comorb.cols <- comorb.cols[order(nice.comorb.cols)]
comorb.cols <- comorb.cols[c(15,1:14,16:17)]
symp.cols <- symp.cols[1:21][order(nice.symp.cols)]
all.comorb.cols.string <- paste(comorb.cols[c(1:14, 16:17)], collapse = " + ")
all.symp.cols.string <- paste(symp.cols, collapse = " + ")
all.comorb.cols.string.nax <- paste(glue("{comorb.cols[c(1:14, 16:17)]}_na_excluded"), collapse = " + ")
all.symp.cols.string.nax <-paste(glue("{symp.cols}_na_excluded"), collapse = " + ")

# Filter to admission or onset in 2020

complete.data <- complete.data %>% 
  filter(start.date <= ymd("2020-12-31") | onset.date <= ymd("2020-12-31")) 

# Data for onset to admission analysis

onset.admission.data <- complete.data %>%
  filter(!is.na(onset.to.admission))  %>%
  # 97.5% percentile is 24 days
  filter(onset.to.admission >= 0 & onset.to.admission <= 24) %>%
  dplyr::mutate(epiweek.onset = epiweek(onset.date)) %>%
  dplyr::mutate(epiweek.year.onset = map_dbl(onset.date, epiweek.year)) %>%
  dplyr::mutate(year.epiweek.onset = glue("{epiweek.year.onset}-{epiweek.onset}"), .envir = .SD) %>%
  dplyr::mutate(year.epiweek.onset = factor(year.epiweek.onset, levels = epiweek.order))

onset.admission.data <- onset.admission.data %>%
  mutate(symp.any.respiratory = symp.n.respiratory >= 1) %>%
  mutate(symp.any.respiratory.complete = symp.n.respiratory >= 1 & symp.all.respiratory.recorded) %>%
  mutate(symp.any.gi = symp.n.gi >= 1) %>%
  mutate(symp.any.gi.complete = symp.n.gi >= 1 & symp.all.gi.recorded) %>%
  mutate(symp.any.common = symp.n.common >= 1) %>%
  mutate(symp.any.common.complete = symp.n.common >= 1 & symp.all.common.recorded) %>%
  mutate(outcome = replace(outcome, is.na(outcome), "unknown"))

# Data for analysis of covariates of ICU/HDU admission

# for comorbidities, want NA as its own class, not a missing data category

na.to.f <- function(val){
  ifelse(is.na(val), "UNKNOWN", as.character(val))

}

icu.ever.data <- complete.data %>%
  filter(!is.na(ICU.ever)) %>%
  filter(!is.na(start.date)) %>%
  mutate_at(comorb.cols, list(na_excluded = ~na.to.f(.))) %>%
  mutate_at(symp.cols, list(na_excluded = ~na.to.f(.))) %>%
  # count and group days before admission
  mutate(group.OtoA = replace(onset.to.admission, onset.to.admission > 22, NA)) %>%
  mutate(group.OtoA = cut(group.OtoA, breaks = c(23, 14, 7, 0, -10000), right = F)) %>%
  mutate(group.OtoA = fct_relevel(group.OtoA, "[0,7)"))

# Data for time to ICU/HDU analysis

time.to.icu.data <- complete.data %>%
  filter(!is.na(ICU.ever) & ICU.ever) %>%
  filter(!is.na(start.to.ICU)) %>%
  # 97.5% percentile is 13 days
  filter(start.to.ICU >=0 & start.to.ICU <= 13) %>%
  mutate_at(comorb.cols, list(na_excluded = ~na.to.f(.))) %>%
  mutate_at(symp.cols, list(na_excluded = ~na.to.f(.)))  %>%
  # count and group days before admission
  mutate(group.OtoA = replace(onset.to.admission, onset.to.admission > 22, NA)) %>%
  mutate(group.OtoA = cut(group.OtoA, breaks = c(23, 14, 7, 0, -10000), right = F)) %>%
  mutate(group.OtoA = fct_relevel(group.OtoA, "[0,7)"))

# Data for full hospital stay

overall.hospital.stay.data <- complete.data %>%
  mutate(last.seen = replace(last.seen, outcome == "unknown", (complete.data %>% filter(outcome=="unknown") %>% pull(exit.date)) -1)) %>%
  mutate(time.to.censor = as.numeric(last.seen - start.date)) %>%
  mutate(start.to.outcome = ifelse(!is.na(start.to.exit), start.to.exit, time.to.censor))  %>%
  filter(!is.na(start.date)) %>%
  filter(start.to.outcome >= 0) %>%
  mutate(start.to.exit = replace(start.to.exit, outcome == "unknown", NA)) %>%
  mutate(outcome = replace(outcome, outcome == "unknown", "censored")) %>%
  mutate_at(comorb.cols, list(na_excluded = ~na.to.f(.))) %>%
  mutate_at(symp.cols, list(na_excluded = ~na.to.f(.))) %>%
  mutate(group.OtoA = replace(onset.to.admission, onset.to.admission > 22, NA)) %>%
  mutate(group.OtoA = cut(group.OtoA, breaks = c(23, 14, 7, 0, -10000), right = F)) %>%
  mutate(group.OtoA = fct_relevel(group.OtoA, "[0,7)"))

#### Onset to admission regression ####

# Additional pre-processing for the regression

onset.admission.data.2 <- onset.admission.data %>%
  filter(year(onset.date) == 2020) %>%
  mutate(month.onset = fct_relevel(month.onset, "April")) %>%
  mutate(outcome2 = case_when(outcome == "death" ~ "death",
                              outcome == "discharge" ~ "discharge"))

# Group countries with less than 200 records
merged.countries <- names(table(onset.admission.data.2$Country))[which(table(onset.admission.data.2$Country)<=200)]

merged.S.America <- c("Argentina", "Chile", "Ecuador", "Mexico", "Dominican Republic")
merged.Europe <- c("Austria", "Czechia", "Estonia", "Germany", "Greece", "Ukraine")
merged.Asia <- c("Taiwan", "Hong Kong", "Japan", "South Korea", "Thailand", "Viet Nam", "New Zealand", "Malaysia")
merged.Africa.ME <- c("South Africa", "Ghana", "Turkey", "Qatar", "Kuwait", "Saudi Arabia")

# These should both be empty
setdiff(c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ), merged.countries)
setdiff(merged.countries, c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ))


onset.admission.data.2 <- onset.admission.data.2 %>%
  mutate(Country.2 = case_when(Country %in% merged.S.America ~ "South America",
                               Country %in% merged.Europe ~ "Europe",
                               Country %in% merged.Asia ~ "Asia",
                               Country %in% merged.Africa.ME ~ "Africa and Middle East",
                               TRUE ~ Country
  ))

ota.model <- glm(as.formula(paste0("log(onset.to.admission + 1) ~ month.onset + 
                                  symp.n.common + 
                                  nice.agegp + 
                                  sex + 
                                  outcome2 +
                                  Country.2")), 
                 data = onset.admission.data.2)


pvals <- summary(ota.model)$coefficients[,4]

coef.adjust <- function(x) (exp(x) - 1)*100

covlabels <- c("Month of symptom onset (ref: April)\\\\\\hspace{0.3cm}March",
               "\\hspace{0.3cm}May",
               "\\hspace{0.3cm}June",
               "\\hspace{0.3cm}July",
               "\\hspace{0.3cm}August",
               "\\hspace{0.3cm}September",
               "\\hspace{0.3cm}October",
               "\\hspace{0.3cm}November",
               "\\hspace{0.3cm}December",
               "Number of common symptoms (max 4)",
               "Number of GI symptoms (max 3)",
               "Age group (ref: 40-59)\\\\\\hspace{0.3cm}0-19",
               "\\hspace{0.3cm}20-39",
               "\\hspace{0.3cm}60-69",
               "\\hspace{0.3cm}70-79",
               "\\hspace{0.3cm}80+",
               "Sex (ref: Female)\\\\\\hspace{0.3cm}Male",
               "Final outcome (ref: death)\\\\\\hspace{0.3cm}Discharge",
               "Country (ref: Africa and Middle East - grouped)\\\\\\hspace{0.3cm}Asia - grouped",
               "\\hspace{0.3cm}Belgium",
               "\\hspace{0.3cm}Brazil",
               "\\hspace{0.3cm}Canada",
               "\\hspace{0.3cm}Colombia",
               "\\hspace{0.3cm}Europe - grouped",
               "\\hspace{0.3cm}France",
               "\\hspace{0.3cm}India",
               "\\hspace{0.3cm}Indonesia",
               "\\hspace{0.3cm}Ireland",
               "\\hspace{0.3cm}Israel",
               "\\hspace{0.3cm}Italy",
               "\\hspace{0.3cm}Nepal",
               "\\hspace{0.3cm}Netherlands",
               "\\hspace{0.3cm}Norway",
               "\\hspace{0.3cm}Pakistan",
               "\\hspace{0.3cm}Peru",
               "\\hspace{0.3cm}Poland",
               "\\hspace{0.3cm}Portugal",
               "\\hspace{0.3cm}Romania",
               "\\hspace{0.3cm}South and Central America and Caribbean - grouped",
               "\\hspace{0.3cm}Spain",
               "\\hspace{0.3cm}United Kingdom",
               "\\hspace{0.3cm}United States of America",
               "(Intercept)")

CI.effect <- confint(ota.model)
CI.effect[,1] <- map_dbl(CI.effect[,1], coef.adjust)
CI.effect[,2] <- map_dbl(CI.effect[,2], coef.adjust)

# Wald tests for inclusion of all variables
variable.indexes <- list(2:10, 11, 12:16, 17, 18, 19:42)

wald.ps <- map(variable.indexes, function(x){
  wt <- wald.test(vcov(ota.model), coef(ota.model), x)
  wt
})


# Write the model table
write_lines(stargazer(ota.model, 
                      dep.var.caption = "",
                      dep.var.labels  = "\\% increase in time\\\\ & to admission (days)",
                      ci = TRUE,
                      ci.custom = list(CI.effect),
                      p.auto = F,
                      single.row = TRUE,
                      star.cutoffs = c(0.05, 0.01, 0.001),
                      apply.coef = coef.adjust,
                      covariate.labels = covlabels
),
"SuppFile4.tex")


#### ICU/HDU admission regression ####

# Additional pre-processing for the regression

icu.ever.data.2 <- icu.ever.data %>%
  filter(year(start.date) == 2020) %>%
  mutate(month.start = fct_relevel(month.start, "April")) %>%
  mutate(outcome2 = case_when(outcome == "death" ~ "death",
                              outcome == "discharge" ~ "discharge"))  

# Group countries with less than 200 records
merged.countries <- names(table(icu.ever.data.2$Country))[which(table(icu.ever.data.2$Country)<=200)]

merged.S.America <- c("Argentina", "Chile", "Ecuador", "Mexico", "Dominican Republic")
merged.Europe <- c("Austria", "Czechia", "Estonia", "Germany", "Greece", "Ukraine")
merged.Asia <- c( "Taiwan", "Hong Kong", "Japan", "South Korea", "Malaysia", "Thailand", "Viet Nam", "New Zealand")
merged.Africa.ME <- c("South Africa", "Ghana", "Turkey", "Qatar", "Kuwait", "Saudi Arabia")

# These should both be empty
setdiff(c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ), merged.countries)
setdiff(merged.countries, c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ))

icu.ever.data.2 <- icu.ever.data.2 %>%
  mutate(Country.2 = case_when(Country %in% merged.S.America ~ "South America",
                               Country %in% merged.Europe ~ "Europe",
                               Country %in% merged.Asia ~ "Asia and Oceania",
                               Country %in% merged.Africa.ME ~ "Africa and Middle East",
                               TRUE ~ Country
  )) %>%
  mutate(Country.2 = factor(Country.2, ordered = F)) %>%
  mutate(Country.2 = fct_relevel(Country.2, "Asia and Oceania"))

icu.ever.model.2 <- glm(as.formula(glue("ICU.ever ~ month.start + 
                          nice.agegp +
                          sex*comorb.pregnancy_na_excluded +
                          group.OtoA +
                          outcome2 +
                          {all.comorb.cols.string.nax} +
                          Country.2"
)), 
data = icu.ever.data.2, family=binomial)

covlabels <- c("Month of COVID admission (ref: April)\\\\\\hspace{0.3cm}March",
               "\\hspace{0.3cm}May",
               "\\hspace{0.3cm}June",
               "\\hspace{0.3cm}July",
               "\\hspace{0.3cm}August",
               "\\hspace{0.3cm}September",
               "\\hspace{0.3cm}October",
               "\\hspace{0.3cm}November",
               "\\hspace{0.3cm}December",
               "Number of GI symptoms (max 3)",
               "Age group (ref: 40-59)\\\\\\hspace{0.3cm}0-19",
               "\\hspace{0.3cm}20-39",
               "\\hspace{0.3cm}60-69",
               "\\hspace{0.3cm}70-79",
               "\\hspace{0.3cm}80+",
               "Sex (ref: Female)\\\\\\hspace{0.3cm}Male",
               "Days from symptom onset to hospital admission (ref: 0-6):\\\\\\hspace{0.3cm}Nosocomial infection",
               "\\hspace{0.3cm}7-13",
               "\\hspace{0.3cm}14+",
               "Final outcome (ref: death)\\\\\\hspace{0.3cm}Discharge",
               "Pregnant (ref: no)\\\\\\hspace{0.3cm}Yes",
               "\\hspace{0.3cm}Unknown",
               "Comorbidities (ref: absent)\\\\\\hspace{0.3cm}Asthma\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic cardiac disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic haemotologic disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic kidney disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic neurological disorder\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic pulmonary disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Dementia\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Diabetes\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}HIV/AIDS\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Hypertension\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Liver disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Malignant neoplasm\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Malnutrition\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Obesity\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Rheumatological disorder\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Smoking\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "Country (ref: Asia and Oceania - grouped)\\\\\\hspace{0.3cm} Africa and Middle East - grouped",
               "\\hspace{0.3cm}Belgium",
               "\\hspace{0.3cm}Brazil",
               "\\hspace{0.3cm}Canada",
               "\\hspace{0.3cm}Colombia",
               "\\hspace{0.3cm}Europe - grouped",
               "\\hspace{0.3cm}France",
               "\\hspace{0.3cm}India",
               "\\hspace{0.3cm}Indonesia",
               "\\hspace{0.3cm}Ireland",
               "\\hspace{0.3cm}Israel",
               "\\hspace{0.3cm}Italy",
               "\\hspace{0.3cm}Nepal",
               "\\hspace{0.3cm}Netherlands",
               "\\hspace{0.3cm}Norway",
               "\\hspace{0.3cm}Pakistan",
               "\\hspace{0.3cm}Peru",
               "\\hspace{0.3cm}Poland",
               "\\hspace{0.3cm}Portugal",
               "\\hspace{0.3cm}Romania",
               "\\hspace{0.3cm}South and Central America and Caribbean - grouped",
               "\\hspace{0.3cm}Spain",
               "\\hspace{0.3cm}United Kingdom",
               "\\hspace{0.3cm}United States of America",
               "(Intercept)")

CI.effect <- confint.default(icu.ever.model.2)
CI.effect[,1] <- map_dbl(CI.effect[,1],exp)
CI.effect[,2] <- map_dbl(CI.effect[,2], exp)

# Wald test for inclusion of all variables
variable.indexes <- c(list(2:10, 11:15, 16, c(17,18), 19:21, 22), map(seq(23, 53, by= 2), function(x) c(x, x+1)), list(55:72))

wald.ps <- map(variable.indexes, function(x){
  wt <- wald.test(vcov(icu.ever.model.2)[-c(79,80),-c(79,80)], coef(icu.ever.model.2)[-c(79,80)], x)
  wt
})
# Write the model table
write_lines(stargazer(icu.ever.model.2, 
                      dep.var.caption = "",
                      dep.var.labels  = "Odds ratio",
                      ci = TRUE,
                      ci.custom = list(CI.effect),
                      p.auto = F,
                      single.row = TRUE,
                      star.cutoffs = c(0.05, 0.01, 0.001),
                      apply.coef = exp,
                      covariate.labels = covlabels
),
"SuppFile5.tex")

#### Time to ICU/HDU regression ####

# Additional pre-processing for the regression

time.to.icu.data.2 <-  time.to.icu.data %>%
  filter(year(start.date) == 2020) %>%
  mutate(month.start = fct_relevel(month.start, "April")) %>%
  mutate(outcome2 = case_when(outcome == "death" ~ "death",
                              outcome == "discharge" ~ "discharge")) %>%
  mutate(month.start = fct_drop(month.start)) 

# Group countries with less than 200 records
merged.countries <- names(table(time.to.icu.data.2$Country))[which(table(time.to.icu.data.2$Country)<=200)]

# These should both be empty
merged.S.America <- c("Argentina", "Chile", "Ecuador", "Peru", "Dominican Republic", "Mexico")
merged.Europe <- c("Austria", "Czechia", "Estonia", "Germany", "Greece", "Norway", "Poland", "Romania", "Ukraine")
merged.Asia <- c("Taiwan", "Hong Kong", "Japan", "South Korea", "Viet Nam", "New Zealand", "Malaysia", "Thailand")
merged.Africa.ME <- c("South Africa", "Ghana", "Israel", "Qatar", "Kuwait", "Saudi Arabia")


setdiff(c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ), merged.countries)
setdiff(merged.countries, c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ))



time.to.icu.data.2 <- time.to.icu.data.2 %>%
  mutate(Country.2 = case_when(Country %in% merged.S.America ~ "South America",
                               Country %in% merged.Europe ~ "Europe",
                               Country %in% merged.Asia ~ "Asia and Oceania",
                               Country %in% merged.Africa.ME ~ "Africa and Middle East",
                               TRUE ~ Country
  ))

time.to.ICU.model <- glm(as.formula(glue("log(start.to.ICU + 1) ~ 
                          month.start + 
                          nice.agegp +
                          sex*comorb.pregnancy_na_excluded +
                          group.OtoA +
                          outcome2 +
                          {all.comorb.cols.string.nax} +
                          Country.2")), 
                         data = time.to.icu.data.2)

coef.adjust <- function(x) (exp(x) - 1)*100

covlabels <- c("Month of COVID admission (ref: April)\\\\\\hspace{0.3cm}March",
               "\\hspace{0.3cm}May",
               "\\hspace{0.3cm}June",
               "\\hspace{0.3cm}July",
               "\\hspace{0.3cm}August",
               "\\hspace{0.3cm}September",
               "\\hspace{0.3cm}October",
               "\\hspace{0.3cm}November",
               "\\hspace{0.3cm}December",
               "Number of GI symptoms (max 3)",
               "Age group (ref: 40-59)\\\\\\hspace{0.3cm}0-19",
               "\\hspace{0.3cm}20-39",
               "\\hspace{0.3cm}60-69",
               "\\hspace{0.3cm}70-79",
               "\\hspace{0.3cm}80+",
               "Sex (ref: Female)\\\\\\hspace{0.3cm}Male",
               "Days from symptom onset to hospital admission (ref: 0-6):\\\\\\hspace{0.3cm}Nosocomial infection",
               "\\hspace{0.3cm}7-13",
               "\\hspace{0.3cm}14+",
               "Final outcome (ref: death)\\\\\\hspace{0.3cm}Discharge",
               "Pregnant (ref: no)\\\\\\hspace{0.3cm}Yes",
               "\\hspace{0.3cm}Unknown",
               "Comorbidities (ref: absent)\\\\\\hspace{0.3cm}Asthma\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic cardiac disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic haemotologic disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic kidney disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic neurological disorder\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic pulmonary disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Dementia\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Diabetes\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}HIV/AIDS\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Hypertension\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Liver disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Malignant neoplasm\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Malnutrition\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Obesity\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Rheumatological disorder\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Smoking\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "Country (ref: Africa and Middle East - grouped)\\\\\\hspace{0.3cm}Asia and Oceania - grouped",
               "\\hspace{0.3cm}Belgium",
               "\\hspace{0.3cm}Brazil",
               "\\hspace{0.3cm}Canada",
               "\\hspace{0.3cm}Colombia",
               "\\hspace{0.3cm}Europe - grouped",
               "\\hspace{0.3cm}France",
               "\\hspace{0.3cm}India",
               "\\hspace{0.3cm}Indonesia",
               "\\hspace{0.3cm}Ireland",
               "\\hspace{0.3cm}Italy",
               "\\hspace{0.3cm}Nepal",
               "\\hspace{0.3cm}Netherlands",
               "\\hspace{0.3cm}Pakistan",
               "\\hspace{0.3cm}Portugal",
               "\\hspace{0.3cm}South and Central America and Caribbean - grouped",
               "\\hspace{0.3cm}Spain",
               "\\hspace{0.3cm}United Kingdom",
               "\\hspace{0.3cm}United States of America",
               "(Intercept)")

CI.effect <- confint.default(time.to.ICU.model)
CI.effect[,1] <- map_dbl(CI.effect[,1], coef.adjust)
CI.effect[,2] <- map_dbl(CI.effect[,2], coef.adjust)

# Wald tests for inclusion of all variables

variable.indexes <- c(list(2:10, 11:15, 16, c(17,18), 19:21, 22), map(seq(23, 53, by= 2), function(x) c(x, x+1)), list(55:73))

wald.ps <- map(variable.indexes, function(x){
  wt <- wald.test(vcov(time.to.ICU.model)[-c(74,75),-c(74,75)], coef(time.to.ICU.model)[-c(74,75)], x)
  wt
})


write_lines(stargazer(time.to.ICU.model, 
                      dep.var.caption = "",
                      dep.var.labels  = "\\% increase in time\\\\ & to admission (days)",
                      ci = TRUE,
                      ci.custom = list(CI.effect),
                      p.auto = F,
                      single.row = TRUE,
                      star.cutoffs = c(0.05, 0.01, 0.001),
                      apply.coef = coef.adjust,
                      covariate.labels = covlabels
),
"SuppFile6.tex")


#### Outcome/time to outcome regressions ####

# Additional pre-processing for the regression

overall.hospital.stay.data.2 <- overall.hospital.stay.data %>%
  mutate(factor.outcome = factor(overall.hospital.stay.data$outcome)) %>%
  # 97.5% percentile is 45
  filter(start.to.outcome <= 45) %>%
  mutate(time.to.event = ifelse(is.na(start.to.exit), time.to.censor +1 , start.to.exit +1)) %>%
  filter(!is.na(time.to.event) & time.to.event > 0) %>%
  filter(year(start.date) == 2020) %>%
  mutate(month.start = fct_drop(month.start)) %>%
  # no censored patients allowed here
  filter(outcome != "censored")


overall.hospital.stay.data.2 <- overall.hospital.stay.data.2 %>% 
  mutate(month.start = fct_relevel(month.start, "April")) %>%
  # nonsensical time to ICUs changed to non-ICU patients
  mutate(ICU.ever = replace(ICU.ever, ICU.ever & (start.to.ICU <0 | start.to.ICU > start.to.exit), F)) 

# Group countries with less than 200 records
merged.countries <- names(table(overall.hospital.stay.data.2$Country))[which(table(overall.hospital.stay.data.2$Country)<=200)]

merged.S.America <- c("Argentina", "Chile", "Ecuador", "Mexico", "Dominican Republic")
merged.Europe <- c("Austria", "Czechia", "Estonia", "Germany", "Greece", "Ukraine", "Poland")
merged.Asia <- c(  "Hong Kong", "Japan", "South Korea", "Thailand", "New Zealand", "Nepal")
merged.Africa.ME <- c("South Africa", "Ghana",  "Qatar", "Kuwait", "Saudi Arabia")

setdiff(c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ), merged.countries)
setdiff(merged.countries, c(merged.S.America,merged.Europe, merged.Asia, merged.Africa.ME ))

overall.hospital.stay.data.2 <- overall.hospital.stay.data.2 %>%
  mutate(Country.2 = case_when(Country %in% merged.S.America ~ "South America",
                               Country %in% merged.Europe ~ "Europe",
                               Country %in% merged.Asia ~ "Asia",
                               Country %in% merged.Africa.ME ~ "Africa and Middle East",
                               TRUE ~ Country
  )) %>%
  mutate(death = outcome == "death")

# Logistic model for CFR

logistic.cfr <- glm(as.formula(glue("death ~ (month.start + nice.agegp) * ICU.ever + 
                          sex*comorb.pregnancy_na_excluded +
                          group.OtoA +
                          {all.comorb.cols.string.nax} +
                          Country.2"
)), 
data = overall.hospital.stay.data.2, family=binomial)


CI.cfr <- confint.default(logistic.cfr)
CI.cfr[,1] <- map_dbl(CI.cfr[,1],exp)
CI.cfr[,2] <- map_dbl(CI.cfr[,2], exp)

# Wald tests for inclusion of all variables and interaction terms

variable.indexes <- c(list(2:10, 11:15, 16, 17, c(18,19), 20:22), map(seq(23, 53, by= 2), function(x) c(x, x+1)), list(55:76), list(77:85), list(86:90))

wald.ps <- map(variable.indexes, function(x){
  wt <- aod::wald.test(vcov(logistic.cfr)[-c(91,92),-c(91,92)], coef(logistic.cfr)[-c(91,92)], x)
  wt
  
})

# the following rows calculate confidence intervals for coefficients with interactions

coefs_var <- vcov(logistic.cfr)
bma.mm <- model.matrix(logistic.cfr)
dof <- nrow(bma.mm) - ncol(bma.mm)


month.death.lower.cis <- map_dbl(1:9, function(group.no){
  icu.coef <- 16
  coef.alone <- 1 + group.no
  coef.combo <- 76 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(logistic.cfr)[c(coef.alone, coef.combo)]) - half.ci
  
}) %>% exp()


submatrix <- coefs_var[c(16, 85), c(16, 85)]
temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
half.ci <- qt(0.975, dof) * temp[1,1]

sum(coef(logistic.cfr)[c(16, 85)]) - half.ci
sum(coef(logistic.cfr)[c(16, 85)]) + half.ci


month.death.upper.cis <- map_dbl(1:9, function(group.no){
  icu.coef <- 16
  coef.alone <- 1 + group.no
  coef.combo <- 76 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(logistic.cfr)[c(coef.alone, coef.combo)]) + half.ci
  
}) %>% exp()

age.death.lower.cis <- map_dbl(1:5, function(group.no){
  icu.coef <- 16
  coef.alone <- 10 + group.no
  coef.combo <- 85 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(logistic.cfr)[c(coef.alone, coef.combo)]) - half.ci
  
}) %>% exp()

age.death.upper.cis <- map_dbl(1:5, function(group.no){
  icu.coef <- 16
  coef.alone <- 10 + group.no
  coef.combo <- 85 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(logistic.cfr)[c(coef.alone, coef.combo)]) + half.ci
  
}) %>% exp()



covlabels <- c("Month of COVID admission (ref: April)\\\\\\hspace{0.3cm}March",
               "\\hspace{0.3cm}May",
               "\\hspace{0.3cm}June",
               "\\hspace{0.3cm}July",
               "\\hspace{0.3cm}August",
               "\\hspace{0.3cm}September",
               "\\hspace{0.3cm}October",
               "\\hspace{0.3cm}November",
               "\\hspace{0.3cm}December",
               "Age group (ref: 40-59)\\\\\\hspace{0.3cm}0-19",
               "\\hspace{0.3cm}20-39",
               "\\hspace{0.3cm}60-69",
               "\\hspace{0.3cm}70-79",
               "\\hspace{0.3cm}80+",
               "ICU/HDU admission",
               "Sex (ref: Female)\\\\\\hspace{0.3cm}Male",
               "Number of GI symptoms (max 3)",
               "Days from symptom onset to hospital admission (ref: 0-6):\\\\\\hspace{0.3cm}Nosocomial infection",
               "\\hspace{0.3cm}7-13",
               "\\hspace{0.3cm}14+",
               "Pregnant (ref: no)\\\\\\hspace{0.3cm}Yes",
               "\\hspace{0.3cm}Unknown",
               "Comorbidities (ref: absent)\\\\\\hspace{0.3cm}Asthma\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic cardiac disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic haemotologic disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic kidney disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic neurological disorder\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Chronic pulmonary disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Dementia\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Diabetes\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}HIV/AIDS\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Hypertension\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Liver disease\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Malignant neoplasm\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Malnutrition\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Obesity\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Rheumatological disorder\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "\\hspace{0.3cm}Smoking\\\\\\hspace{0.6cm}Present",
               "\\hspace{0.6cm}Unknown",
               "Country (ref: Africa and Middle East - grouped)\\\\\\hspace{0.3cm} Asia and Oceania - grouped",
               "\\hspace{0.3cm}Belgium",
               "\\hspace{0.3cm}Brazil",
               "\\hspace{0.3cm}Canada",
               "\\hspace{0.3cm}Colombia",
               "\\hspace{0.3cm}Europe - grouped",
               "\\hspace{0.3cm}France",
               "\\hspace{0.3cm}India",
               "\\hspace{0.3cm}Indonesia",
               "\\hspace{0.3cm}Ireland",
               "\\hspace{0.3cm}Israel",
               "\\hspace{0.3cm}Italy",
               "\\hspace{0.3cm}Netherlands",
               "\\hspace{0.3cm}Norway",
               "\\hspace{0.3cm}Pakistan",
               "\\hspace{0.3cm}Peru",
               "\\hspace{0.3cm}Portugal",
               "\\hspace{0.3cm}Romania",
               "\\hspace{0.3cm}South and Central America and Caribbean - grouped",
               "\\hspace{0.3cm}Spain",
               "\\hspace{0.3cm}United Kingdom",
               "\\hspace{0.3cm}United States of America",
               "Interaction: ICU/HDU admission $\times$ month of admission (ref: May)\\\\\\hspace{0.3cm}March",
               "\\hspace{0.3cm}May",
               "\\hspace{0.3cm}June",
               "\\hspace{0.3cm}July",
               "\\hspace{0.3cm}August",
               "\\hspace{0.3cm}September",
               "\\hspace{0.3cm}October",
               "\\hspace{0.3cm}November",
               "\\hspace{0.3cm}December",
               "Interaction: ICU/HDU admission $\times$ age group (ref: 40-59)\\\\\\hspace{0.3cm}0-19",
               "\\hspace{0.3cm}20-39",
               "\\hspace{0.3cm}60-69",
               "\\hspace{0.3cm}70-79",
               "\\hspace{0.3cm}80+",
               "(Intercept)")

# Linear regression for time to event where that event is death

linear.time.to.death <- glm(as.formula(glue("log(time.to.event + 1) ~ (month.start + nice.agegp) * ICU.ever + 
                                    sex*comorb.pregnancy_na_excluded + 
                                    group.OtoA +
                                    {all.comorb.cols.string.nax} + 
                                    Country.2"
)), 
data = overall.hospital.stay.data.2 %>% filter(outcome == "death"))

CI.death <- confint(linear.time.to.death)
CI.death[,1] <- map_dbl(CI.death[,1], coef.adjust)
CI.death[,2] <- map_dbl(CI.death[,2], coef.adjust)

# Wald tests for inclusion of month of admission, and interaction terms
wt.death.time <- wald.test(varb = vcov(linear.time.to.death), b = coef(linear.time.to.death), Terms = 2:10)
wt.death.intertime <- wald.test(varb = vcov(linear.time.to.death), b = coef(linear.time.to.death), Terms = 78:86)
wt.death.interage <- wald.test(varb = vcov(linear.time.to.death), b = coef(linear.time.to.death), Terms = 87:91)

# the following rows calculate confidence intervals for coefficients with interactions

coefs_var <- vcov(linear.time.to.death)
bma.mm <- model.matrix(linear.time.to.death)
dof <- nrow(bma.mm) - ncol(bma.mm)

(coef(linear.time.to.death)[2:10] + coef(linear.time.to.death)[77:85]) %>% coef.adjust()

month.death.lower.cis <- map_dbl(1:9, function(group.no){
  icu.coef <- 16
  coef.alone <- 1 + group.no
  coef.combo <- 76 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.death)[c(coef.alone, coef.combo)]) - half.ci
  
}) %>% coef.adjust()

month.death.upper.cis <- map_dbl(1:9, function(group.no){
  icu.coef <- 16
  coef.alone <- 1 + group.no
  coef.combo <- 76 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.death)[c(coef.alone, coef.combo)]) + half.ci
  
}) %>% coef.adjust()

(coef(linear.time.to.death)[11:15] + coef(linear.time.to.death)[86:90]) %>% coef.adjust()

age.death.lower.cis <- map_dbl(1:5, function(group.no){
  icu.coef <- 16
  coef.alone <- 10 + group.no
  coef.combo <- 85 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.death)[c(coef.alone, coef.combo)]) - half.ci
  
}) %>% coef.adjust()

age.death.upper.cis <- map_dbl(1:5, function(group.no){
  icu.coef <- 16
  coef.alone <- 10 + group.no
  coef.combo <- 85 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.death)[c(coef.alone, coef.combo)]) + half.ci
  
}) %>% coef.adjust()

(coef(linear.time.to.death)[11:15] + coef(linear.time.to.death)[86:90]) %>% coef.adjust

# Linear regression for time to event where that event is discharge

linear.time.to.discharge <- glm(as.formula(glue("log(time.to.event + 1) ~ (month.start + nice.agegp) * ICU.ever + 
                                    sex*comorb.pregnancy_na_excluded + 
                                    group.OtoA +
                                    {all.comorb.cols.string.nax} + 
                                    Country.2"
)), 
data = overall.hospital.stay.data.2 %>% filter(outcome == "discharge"))

CI.discharge <- confint(linear.time.to.discharge)
CI.discharge[,1] <- map_dbl(CI.discharge[,1], coef.adjust)
CI.discharge[,2] <- map_dbl(CI.discharge[,2], coef.adjust)

# Wald tests for inclusion of month of admission, and interaction terms
wt.discharge.time <- wald.test(varb = vcov(linear.time.to.discharge), b = coef(linear.time.to.discharge), Terms = 2:10)
wt.discharge.intertime <- wald.test(varb = vcov(linear.time.to.discharge), b = coef(linear.time.to.discharge), Terms = 78:86)
wt.discharge.interage <- wald.test(varb = vcov(linear.time.to.discharge), b = coef(linear.time.to.discharge), Terms = 87:91)

# the following rows calculate confidence intervals for coefficients with interactions

coefs_var <- vcov(linear.time.to.discharge)
bma.mm <- model.matrix(linear.time.to.discharge)
dof <- nrow(bma.mm) - ncol(bma.mm)

month.discharge.lower.cis <- map_dbl(1:9, function(group.no){
  icu.coef <- 16
  coef.alone <- 1 + group.no
  coef.combo <- 76 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.discharge)[c(coef.alone, coef.combo)]) - half.ci
  
}) %>% coef.adjust()

month.discharge.upper.cis <- map_dbl(1:9, function(group.no){
  icu.coef <- 16
  coef.alone <- 1 + group.no
  coef.combo <- 76 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.discharge)[c(coef.alone, coef.combo)]) + half.ci
  
}) %>% coef.adjust()

age.discharge.lower.cis <- map_dbl(1:5, function(group.no){
  icu.coef <- 16
  coef.alone <- 10 + group.no
  coef.combo <- 86 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.discharge)[c(coef.alone, coef.combo)]) - half.ci
  
}) %>% coef.adjust()

age.discharge.upper.cis <- map_dbl(1:5, function(group.no){
  icu.coef <- 16
  coef.alone <- 10 + group.no
  coef.combo <- 86 + group.no
  
  submatrix <- coefs_var[c(coef.alone, coef.combo), c(icu.coef, coef.combo)]
  temp <- matrix(c(1,1), nrow = 1) %*% submatrix %*% matrix(c(1,1), nrow = 2) %>% sqrt
  half.ci <- qt(0.975, dof) * temp[1,1]
  sum(coef(linear.time.to.discharge)[c(coef.alone, coef.combo)]) + half.ci
  
}) %>% coef.adjust()

(coef(linear.time.to.discharge)[11:15] + coef(linear.time.to.discharge)[86:90]) %>% coef.adjust

# Combine columns and write the regression table

coefs.cfr = exp(coef(logistic.cfr))
coefs.death = coef.adjust(coef(linear.time.to.death))
coefs.discharge = coef.adjust(coef(linear.time.to.discharge))

write_lines(stargazer(logistic.cfr, linear.time.to.death, linear.time.to.discharge, 
                      ci = c(TRUE, TRUE, TRUE),  
                      single.row = TRUE,
                      covariate.labels = covlabels,
                      star.cutoffs = c(0.05, 0.01, 0.001),
                      coef = list(coefs.cfr, coefs.death, coefs.discharge),
                      ci.custom = list(CI.cfr, CI.death, CI.discharge),
                      p.auto = F),
            "Table4.tex")
