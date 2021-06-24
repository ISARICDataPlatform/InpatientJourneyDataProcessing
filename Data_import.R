#### Package import ####

library(tidyselect)
library(plyr)
library(tidyverse)
library(magrittr)
library(glue)
library(viridis)
library(ggupset)
library(sf)
library(rgeos)
library(magrittr)
library(binom)
library(fitdistrplus)
library(lubridate)
library(grid)
library(binom)
library(boot)
library(survival)
library(survminer)
library(broom)
library(taRifx)
library(gridExtra)
library(psych)
library(cowplot)
library(scales)
library(ISOcodes)


#### File names ####

# Substitute file name here - data available on application (see https://www.iddo.org/covid19/data-sharing/accessing-data)
# This file should be REDcap output



data.path <- ""
uk.data.dict.file <- ""
row.data.dict.file <- ""
eot.data.dict.file <- ""
rapid.data.dict.file <- ""
site.list.file <- ""
uk.data.file <- ""
row.data.file <- ""
eot.data.file <- ""
rapid.data.file <- ""


#### Dataset inclusion flags and embargo calculation ####

# flags for inclusion of the four data files

options(warn = 1)


if(verbose)
  cat("Setting up datasets and embargo date...\n")

embargo.length <- 0

use.uk.data <- TRUE
use.row.data <- TRUE
use.eot.data <- TRUE
use.rapid.data <- TRUE

single.input <-
  sum(use.uk.data, use.row.data, use.eot.data, use.rapid.data)

ref.date <-  as.Date(substr(uk.data.file, start = 1, stop  = 10))
embargo.limit <- ref.date - embargo.length

if (!use.uk.data & !use.row.data & !use.eot.data & !use.rapid.data) {
  stop("No data to be imported")
}

#### List of sites and countries ####

if (verbose)
  cat("Getting sites and countries...\n")

# This generates a list of sites and a mapping to countries, and to URLs for country flags
# This more or less has to be done by hard-coding country codes, because they do not conform to international two- or three- letter codes
# In fact different codes can be used for the same country (e.g. Ireland)

site.list <- read_csv(glue("{data.path}/{site.list.file}")) %>%
  mutate(site.number = map_chr(site.number, function(sn) substr(sn, 6, nchar(sn)))) %>%
  left_join(ISO_3166_1, by = c("country.code" = "Alpha_3")) %>%
  dplyr::rename(Country = Name)

# The four file imports are subtly different.

# There is a general problem with non-numeric entries in numerical columns. This function replaces them with NA with a warning

careful.as.numeric <- function(value, subjid, colname) {
  out <- suppressWarnings(as.numeric(value))
  if (!is.na(value) & is.na(out)) {
    warning(
      glue(
        "Non-numerical value '{value}' transformed to NA for column {colname}, subject ID {subjid}"
      )
    )
  }
  out
}


pcareful.as.numeric <- function(value.col, subjid, colname) {
  if (is.numeric(value.col)) {
    value.col
  } else {
    map2_dbl(value.col, subjid, function(x, y) {
      careful.as.numeric(x, y, colname)
    })
  }
}

# Checks for dates in the future and returns NA if they are

careful.date.check <-
  function(value, subjid, colname, check.early = F) {
    if (is.na(value)) {
      return(NA)
    } else if (value > today()) {
      warning(
        glue(
          "Future date '{as.character(value)}' transformed to NA for column {colname}, subject ID {subjid}"
        )
      )
      return(NA)
    } else if (value < ymd("2019-01-01") & check.early) {
      warning(
        glue(
          "Implausably early date '{as.character(value)}' transformed to NA for column {colname}, subject ID {subjid}"
        )
      )
      return(NA)
    } else {
      return(as.Date(value, origin = "1970-01-01"))
    }
  }

pcareful.date.check <-
  function(value.col, subjid, colname, check.early = F) {
    map2_dbl(value.col, subjid, function(x, y) {
      careful.date.check(x, y, colname, check.early)
    })
  }

# Checks for ages (strictly) between 0 and 1 and multiples by 100

careful.fractional.age <- function(value, subjid, colname) {
  if (is.na(value)) {
    return(NA)
  } else if (0 < value & 1 > value) {
    warning(
      glue(
        "Fractional age {value} transformed to {value*100} for column {colname}, subject ID {subjid}"
      )
    )
    return(100 * value)
  }
  return(value)
}

to.keep.columns <-
  c(
    'dsstdat',
    'daily_dsstdat',
    'daily_lbdat',
    'hostdat',
    'cestdat',
    'dsstdtc',
    'daily_fio2_lborres',
    'age_estimateyears',
    'hodur',
    'invasive_prdur',
    'subjid',
    'liver_mhyn',
    'chroniccard_mhyn',
    'chronicpul_mhyn',
    'asthma_mhyn',
    'renal_mhyn',
    'modliver',
    'mildliver',
    'chronicneu_mhyn',
    'malignantneo_mhyn',
    'chronhaemo_mhyn',
    'aidshiv_mhyn',
    'obesity_mhyn',
    'diabetescom_mhyn',
    'diabetes_mhyn',
    'rheumatologic_mhyn',
    'dementia_mhyn',
    'malnutrition_mhyn',
    'smoking_mhyn',
    'other_mhyn',
    'fever_ceoccur_v2',
    'cough_ceoccur_v2',
    'cough_ceoccur_v2_2',
    'coughsput_ceoccur_v2',
    'coughhb_ceoccur_v2',
    'sorethroat_ceoccur_v2',
    'runnynose_ceoccur_v2',
    'earpain_ceoccur_v2',
    'wheeze_ceoccur_v2',
    'chestpain_ceoccur_v2',
    'myalgia_ceoccur_v2',
    'jointpain_ceoccur_v2',
    'fatigue_ceoccur_v2',
    'shortbreath_ceoccur_v2',
    'lowerchest_ceoccur_v2',
    'headache_ceoccur_v2',
    'confusion_ceoccur_v2',
    'seizures_cecoccur_v2',
    'abdopain_ceoccur_v2',
    'vomit_ceoccur_v2',
    'diarrhoea_ceoccur_v2',
    'conjunct_ceoccur_v2',
    'rash_ceoccur_v2',
    'skinulcers_ceoccur_v2',
    'lymp_ceoccur_v2',
    'bleed_ceoccur_v2',
    'antiviral_cmyn',
    'antibiotic_cmyn',
    'corticost_cmyn',
    'antifung_cmyn',
    'oxygen_cmoccur',
    'noninvasive_proccur',
    'invasive_proccur',
    'pronevent_prtrt',
    'inhalednit_cmtrt',
    'tracheo_prtrt',
    'extracorp_prtrt',
    'rrt_prtrt',
    'inotrop_cmtrt',
    'other_cmyn',
    'agedat',
    'pregyn_rptestcd',
    'redcap_event_name',
    'dsterm',
    'dsstdtcyn',
    'icu_hostdat',
    'icu_hoendat',
    'hodur',
    'invasive_prdur',
    'antiviral_cmyn',
    'antiviral_cmtrt___1',
    'antiviral_cmtrt___2',
    'antiviral_cmtrt___3',
    'antiviral_cmtrt___4',
    'antiviral_cmtrt___5',
    'antiviral_cmtrt___6',
    'antiviral_cmtype',
    'antibiotic_cmyn',
    'antifung_cmyn',
    'noninvasive_proccur',
    'daily_noninvasive_prtrt',
    'invasive_proccur',
    'daily_invasive_prtrt',
    'extracorp_prtrt',
    'daily_ecmo_prtrt',
    'icu_hoterm',
    'daily_hoterm',
    'rrt_prtrt',
    'daily_rrt_cmtrt',
    'inotrop_cmtrt',
    'daily_inotrope_cmyn',
    'daily_fio2_lborres',
    'daily_nasaloxy_cmtrt',
    'daily_nasaloxy_cmtrt',
    'corna_mbcat',
    'corna_mbcaty',
    'coronaother_mborres',
    'mborres',
    'mbtestcd',
    "other_mbyn",
    "other_mborres",
    'redcap_data_access_group',
    'rheumatologic_mhyn',
    'sex',
    'healthwork_erterm',
    'rr_vsorres',
    'oxy_vsorres',
    'oxy_vsorresu',
    'hr_vsorres',
    'sysbp_vsorres',
    'temp_vsorres',
    'temp_vsorresu',
    'daily_crp_lborres',
    'daily_bun_lborres',
    'daily_bun_lborresu',
    'daily_wbc_lborres',
    'daily_wbc_lborresu',
    'daily_pt_lborres',
    'ddimer_lborres',
    'daily_bil_lborresu',
    'daily_aptt_lborres',
    'daily_lymp_lborres',
    'daily_neutro_lborres',
    'daily_alt_lborres',
    'daily_ast_lborres',
    'daily_bil_lborres',
    'daily_bun_lborresu',
    'daily_bil_lborresu',
    'offlabel_cmyn',
    'oxygenhf_cmoccur',
    "ageusia_ceoccur_v2",
    "anosmia_ceoccur_v2",
    "chrincard",
    "hypertension_mhyn",
    "chronichaemo_mhyn",
    "data.source",
    "Country",
    "site.name",
    "interleukin_cmyn",
    "conv_plasma_cmyn",
    "modliv",
    "mildliv",
    "ceterm_clin_dx",
    "ethnic",
    "other_ethnic"
  )


##### UK data import #####

uk.data.dict <-
  read_csv(glue("{data.path}/{uk.data.dict.file}")) %>%
  filter(`Variable / Field Name` %in% to.keep.columns)


data.dict <- uk.data.dict 

if (use.uk.data) {
  if (verbose)
    cat("Importing UK data...\n")
  
  # the UK data has some jaw-dropping differences between column formats.
  # For a small number of radio buttons 0 is FALSE and 2 NA; for the rest 2 FALSE and 3 NA!!!
  # This function converts the former to the latter
  
  radio.button.convert <- function(x) {
    map_dbl(x, function(y) {
      if (is.na(y)) {
        NA
      } else {
        switch(as.character(y),
               "1" = 1,
               "0" = 2,
               "2" = 3)
      }
    })
  }
  
  # we can use the data dictionary file to identify columns that should be numeric and those that should be text

  uk.column.types <- uk.data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)
  
  uk.date.columns <-
    uk.data.dict %>% filter(`Text Validation Type OR Show Slider Number` == "date_dmy") %>%
    pull(`Variable / Field Name`)
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  
  uk.data <-
    read_csv(glue("{data.path}/{uk.data.file}"), guess_max = 2000000)
  
  uk.data <- uk.data %>% 
    mutate_at(uk.date.columns, parse_date_time, orders = c("dmy", "ymd")) %>%
    mutate_at(uk.date.columns, as.Date)
  
  # Columns that should be text
  text.columns.temp <-
    uk.column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(uk.data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <-
    text.columns[which(text.columns %>% map_lgl(
      function(x)
        ! is.Date(uk.data %>% pull(x)) &
        !is.character(uk.data %>% pull(x))
    ))]
  
  # Columns that should be numerical
  nontext.columns.temp <-
    uk.column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <-
    colnames(uk.data)[which(map_lgl(colnames(uk.data), function(x)
      any(startsWith(
        x, glue("{nontext.columns.temp}___")
      ))))]
  nontext.columns <-
    intersect(colnames(uk.data),
              c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already numerical
  nontext.columns <-
    nontext.columns[which(nontext.columns %>% map_lgl(function(x)
      ! is.numeric(uk.data %>% pull(x))))]
  

  uk.data <- uk.data %>%
    # Columns that should be character are converted to character.
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character)
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA.
  for (ntc in nontext.columns) {
    uk.data <-
      uk.data %>% mutate_at(vars(all_of(ntc)),
                            .funs = ~ pcareful.as.numeric(., subjid = subjid, colname = ntc))
  }
  
  uk.data <- uk.data %>%
    # Country can't be got from the site names
    dplyr::mutate(Country = "UK") %>%
    # data.source is the origin of the data (UK/ROW/EOT/RAPID)
    dplyr::mutate(data.source = "UK") %>%
    dplyr::mutate(site.name = redcap_data_access_group) %>%
    # These are the three radio buttons that behave differently in the raw data
    mutate_at(c("asthma_mhyn", "modliv", "mildliver"),
              radio.button.convert) %>%
    # 1 is SARS-2 in ROW data but MERS in UK, and vice versa. Converting UK to ROW format. "3" in UK is also SARS-2, wonderfully.
    mutate(corna_mbcaty = map_dbl(corna_mbcaty, function(x) {
      if (is.na(x)) {
        NA
      } else {
        switch(as.character(x),
               "1" = 2,
               "2" = 1,
               "3" = 1,
               x)
      }
    }))
  
  for (cn in to.keep.columns) {
    if (!(cn %in% colnames(uk.data))) {
      uk.data <- uk.data %>% mutate(!!cn := NA)
    }
  }
  
  uk.data <-
    uk.data %>% dplyr::select(-setdiff(colnames(uk.data), to.keep.columns))
  
} else {
  uk.data <- NULL
}

##### EOT data import #####

if (use.eot.data) {
  if (verbose)
    cat("Importing EOT data...\n")
  
  eot.data.dict <-
    read_csv(glue("{data.path}/{eot.data.dict.file}")) %>%
    filter(`Variable / Field Name` %in% to.keep.columns)
  
  eot.column.types <- eot.data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`) 
  
  eot.date.columns <-
    eot.data.dict %>% filter(`Text Validation Type OR Show Slider Number` == "date_dmy") %>%
    pull(`Variable / Field Name`)
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  
  eot.data <-
    read_csv(glue("{data.path}/{eot.data.file}"), guess_max = 400000) %>%
    filter(redcap_data_access_group != '062st_vincents_uni') # duplicated...
  
  eot.data <- eot.data %>% 
    mutate_at(eot.date.columns, parse_date_time, orders = c("dmy", "ymd")) %>%
    mutate_at(eot.date.columns, as.Date)
  
  # Columns that should be text
  text.columns.temp <-
    eot.column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(eot.data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <-
    text.columns[which(text.columns %>% map_lgl(
      function(x)
        ! is.Date(eot.data %>% pull(x)) &
        !is.character(eot.data %>% pull(x))
    ))]
  
  # Columns that should be numerical
  nontext.columns.temp <-
    eot.column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <-
    colnames(eot.data)[which(map_lgl(colnames(eot.data), function(x)
      any(startsWith(
        x, glue("{nontext.columns.temp}___")
      ))))]
  nontext.columns <-
    intersect(colnames(eot.data),
              c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already character
  nontext.columns <-
    nontext.columns[which(nontext.columns %>% map_lgl(function(x)
      ! is.numeric(eot.data %>% pull(x))))]
  
  eot.data <- eot.data %>%
    # Columns that should be character are converted to character.
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character)
  
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA.
  # this may actually need a for loop!
  for (ntc in nontext.columns) {
    eot.data <-
      eot.data %>% mutate_at(
        vars(tidyselect::all_of(ntc)),
        .funs = ~ pcareful.as.numeric(., subjid = subjid, colname = ntc)
      )
  }
  
  eot.data <- eot.data %>%
    # some variables have different names in different datasets
    dplyr::rename(chrincard = chroniccard_mhyn) %>%
    # join in the country table
    dplyr::mutate(site.number = map2_chr(subjid, redcap_data_access_group, function(x, y) {
      if (startsWith(x, "F")) {
        str_split_fixed(x, "-", Inf)[1]
      } else {
        str_match(y, "([0-9]+)[a-zA-Z].*")[, 2]
      }
    })) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    # add data source
    # A 5 in corna_mbcat in EOT and CORE is "not done". This field is a nightmare; best changed to NA
    mutate(corna_mbcat = replace(corna_mbcat, corna_mbcat == 5, NA))  %>%
    dplyr::mutate(data.source = "EOT")
  
  
  for (cn in to.keep.columns) {
    if (!(cn %in% colnames(eot.data))) {
      eot.data <- eot.data %>% mutate(!!cn := NA)
    }
  }
  
  eot.data <-
    eot.data %>% dplyr::select(-setdiff(colnames(eot.data), to.keep.columns))
  
} else {
  eot.data <- NULL
}


##### ISARIC Core data import #####

if (use.row.data) {
  if (verbose)
    cat("Importing ISARIC data...\n")
  
  row.data.dict <-
    read_csv(glue("{data.path}/{row.data.dict.file}")) %>%
    filter(`Variable / Field Name` %in% to.keep.columns)
  
  row.column.types <- row.data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`) 
  
  row.date.columns <-
    row.data.dict %>% filter(`Text Validation Type OR Show Slider Number` == "date_dmy") %>%
    pull(`Variable / Field Name`)
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  
  row.data <-
    read_csv(glue("{data.path}/{row.data.file}"), guess_max = 250000)
  
  row.data <- row.data %>% 
    mutate_at(row.date.columns, parse_date_time, orders = c("dmy", "ymd")) %>%
    mutate_at(row.date.columns, as.Date)
  
  # Columns that should be text
  text.columns.temp <-
    row.column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(row.data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <-
    text.columns[which(text.columns %>% map_lgl(
      function(x)
        ! is.Date(row.data %>% pull(x)) &
        !is.character(row.data %>% pull(x))
    ))]
  
  # Columns that should be numerical
  nontext.columns.temp <-
    row.column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <-
    colnames(row.data)[which(map_lgl(colnames(row.data), function(x)
      any(startsWith(
        x, glue("{nontext.columns.temp}___")
      ))))]
  nontext.columns <-
    intersect(colnames(row.data),
              c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already character
  nontext.columns <-
    nontext.columns[which(nontext.columns %>% map_lgl(function(x)
      ! is.numeric(row.data %>% pull(x))))]
  
  row.data <- row.data %>%
    # Columns that should be character are converted to character.
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character)
  
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA.
  for (ntc in nontext.columns) {
    row.data <-
      row.data %>% mutate_at(vars(all_of(ntc)),
                             .funs = ~ pcareful.as.numeric(., subjid = subjid, colname = ntc))
  }
  
  row.data <- row.data %>%
    # some variables have different names in different datasets
    dplyr::rename(
      chrincard = chroniccard_mhyn,
      modliv = modliver_mhyn,
      mildliver = mildliv_mhyn,
      chronichaemo_mhyn = chronhaemo_mhyn,
      diabetescom_mhyn = diabetiscomp_mhyn,
      rheumatologic_mhyn = rheumatology_mhyr,
      icu_hoendat = hoendat
    ) %>%
    mutate(hosttim = as.character(hosttim)) %>%
    mutate(agedat = as.Date(NA)) %>%
    # join in the country table
    dplyr::mutate(site.number = map2_chr(subjid, redcap_data_access_group, function(x, y) {
      if (startsWith(x, "F")) {
        str_split_fixed(x, "-", Inf)[1]
      } else if(startsWith(x, "A-A")){
        NA
      } else {
        str_match(y, "([0-9]+)[a-zA-Z_-].*")[, 2]
      }
    })) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    # A 5 in corna_mbcat in EOT and CORE is "not done". This field is a nightmare; best changed to NA
    mutate(corna_mbcat = replace(corna_mbcat, corna_mbcat == 5, NA))  %>%
    dplyr::mutate(data.source = "Core")
  
  
  for (cn in to.keep.columns) {
    if (!(cn %in% colnames(row.data))) {
      row.data <- row.data %>% mutate(!!cn := NA)
    }
  }
  
  row.data <-
    row.data %>% dplyr::select(-setdiff(colnames(row.data), to.keep.columns))
  
} else {
  row.data <- NULL
}


##### RAPID data import #####


if (use.rapid.data) {
  if (verbose)
    cat("Importing RAPID data...\n")
  
  rapid.data.dict <-
    read_csv(glue("{data.path}/{rapid.data.dict.file}")) 
  
  all.colnames <- rapid.data.dict %>% pull(`Variable / Field Name`)
  
  # RAPID xxx_cmyn fields are "on day of admission". overall_xxx_cmyn are "ever". Change these over.
  
  overall.col.exists <- map_lgl(all.colnames, function(cn){
    glue("overall_{cn}") %in% all.colnames & 
      (endsWith(cn, "cmyn") | endsWith(cn, "occur") | endsWith(cn, "prtrt") | cn == "icu_hoterm")
  })
  
  columns.for.replacement <- all.colnames[overall.col.exists]
  columns.to.replace <- glue("overall_{columns.for.replacement}")
  
  rapid.data.dict <- rapid.data.dict %>%
    filter(`Variable / Field Name` %in% to.keep.columns) 
  
  rapid.date.columns <-
    rapid.data.dict %>% filter(`Text Validation Type OR Show Slider Number` == "date_dmy") %>%
    pull(`Variable / Field Name`)
  
  renamer <- function(cn){
    str_match(cn, "overall_(.*)")[,2]
  }
  
  # readr warnings about parsing failure can often be dealt with by increasing guess_max
  # Then change columns.for.replacement to the standard names used in the other datasets
  
  rapid.data <-
    read_csv(glue("{data.path}/{rapid.data.file}"), guess_max = 200000)  %>%
    dplyr::select(-any_of(columns.for.replacement)) %>%
    rename_with(.fn = renamer, .cols = any_of(columns.to.replace))
  
  rapid.data <-
    rapid.data %>% 
    mutate_at(rapid.date.columns, parse_date_time, orders = c("dmy", "ymd")) %>%
    mutate_at(rapid.date.columns, as.Date)
  
  rapid.column.types <- rapid.data.dict %>% dplyr::select(1, 4) %>%
    dplyr::rename(col.name = `Variable / Field Name`, type = `Field Type`)
  
  # Columns that should be text
  text.columns.temp <-
    rapid.column.types %>% filter(type %in% c("text", "descriptive", "notes", "file")) %>% pull(col.name)
  # some data dictionary columns are not in the data!
  text.columns <- intersect(text.columns.temp, colnames(rapid.data))
  # don't waste time on ones that are already character, and avoid doing anything to date columns
  text.columns <-
    text.columns[which(text.columns %>% map_lgl(
      function(x)
        ! is.Date(rapid.data %>% pull(x)) &
        !is.character(rapid.data %>% pull(x))
    ))]
  
  # Columns that should be numerical
  nontext.columns.temp <-
    rapid.column.types %>% filter(!(type %in% c("text", "descriptive", "notes", "file"))) %>% pull(col.name)
  # radio buttons appear differently in the CSV. E.g. "ethnic" becomes "ethnic___1", "ethnic___2", etc
  nontext.columns.extra <-
    colnames(rapid.data)[which(map_lgl(colnames(rapid.data), function(x)
      any(startsWith(
        x, glue("{nontext.columns.temp}___")
      ))))]
  nontext.columns <-
    intersect(colnames(rapid.data),
              c(nontext.columns.temp, nontext.columns.extra))
  # don't waste time on ones that are already character
  nontext.columns <-
    nontext.columns[which(nontext.columns %>% map_lgl(function(x)
      ! is.numeric(rapid.data %>% pull(x))))]
  
  rapid.data <- rapid.data %>%
    # Columns that should be character are converted to character.
    # Note also that some of these _could_ be numerical (e.g. temperature measurements) but the fields are free text
    mutate_at(text.columns, as.character)
  
  # Columns that should be numeric are converted to numeric. Parse failures becomes NA.
  # this may actually need a for loop!
  for (ntc in nontext.columns) {
    rapid.data <-
      rapid.data %>% mutate_at(
        vars(tidyselect::all_of(ntc)),
        .funs = ~ pcareful.as.numeric(., subjid = subjid, colname = ntc)
      )
  }
  
  rapid.data <- rapid.data %>%
    dplyr::rename(daily_dsstdat = daily_date, daily_hoterm = daily_icu_hoterm,
                  icu_hostdat = overall_icu_hostdat, icu_hoendat = overall_icu_hoendat, agedat = brthdtc,
                  daily_noninvasive_prtrt = daily_noninvasive_proccur, daily_invasive_prtrt = daily_invasive_proccur) %>%
    # join in the country table
    dplyr::mutate(site.number = map2_chr(subjid, redcap_data_access_group, function(x, y) {
      if (startsWith(x, "F")) {
        str_split_fixed(x, "-", Inf)[1]
      } else {
        str_match(y, "([0-9]+)[a-zA-Z].*")[, 2]
      }
    })) %>%
    left_join(site.list, by = "site.number") %>%
    dplyr::select(-site.number) %>%
    # A 3 in corna_mbcat in RAPID is "not done". It is a positive test in CORE and EOT. Change to NA
    mutate(corna_mbcat = replace(corna_mbcat, corna_mbcat == 3, NA))  %>%
    # add data source
    dplyr::mutate(data.source = "RAPID")
  
  for (cn in to.keep.columns) {
    if (!(cn %in% colnames(rapid.data))) {
      rapid.data <- rapid.data %>% mutate(!!cn := NA)
    }
  }
  
  rapid.data <-
    rapid.data %>% dplyr::select(-setdiff(colnames(rapid.data), to.keep.columns))
  
} else{
  rapid.data <- NULL
}


#e verything should be ready for bind_rows now

raw.data <- bind_rows(uk.data, row.data, eot.data, rapid.data)

# There are occasional duplicate IDs from different sites. Here we assign new ones.

splitting.ids <- raw.data %>% 
  group_by(subjid, redcap_data_access_group) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(subjid) %>% 
  summarise(count = n(), sites = list(redcap_data_access_group)) %>% 
  ungroup() %>% filter(count > 1) %>% 
  mutate(site1 = map_chr(sites, function(x) x[1]), site2 = map_chr(sites, function(x) x[2]))

for(i in 1:nrow(splitting.ids)){
  
  subjid <- splitting.ids$subjid[i]
  site <- splitting.ids$site2[i]
  
  raw.data$subjid[which(raw.data$subjid == subjid & raw.data$redcap_data_access_group == site)] <- glue("{subjid}_dupx")
  
}

##### Value adjustments #####

if (verbose)
  cat("Setting future dates to NA...\n")

date.columns <-
  c("dsstdat",
    "daily_dsstdat",
    "daily_lbdat",
    "hostdat",
    "cestdat",
    "dsstdtc")

# Check dates for nonsense values

for (dc in date.columns) {
  raw.data <-
    raw.data %>% mutate_at(
      vars(all_of(dc)),
      .funs = ~ pcareful.date.check(
        .,
        subjid = subjid,
        colname = dc,
        check.early = T
      )
    )
}

# Check DOBs for nonsense values (since these can, obviously, sensibly be much earlier than other dates)

for (dc in "agedat") {
  raw.data <-
    raw.data %>% mutate_at(
      vars(all_of(dc)),
      .funs = ~ pcareful.date.check(
        .,
        subjid = subjid,
        colname = dc,
        check.early = F
      )
    )
}

raw.data <- raw.data %>%
  mutate_at(c(date.columns, "agedat"), function(x)
    as.Date(x, origin = "1970-01-01"))


# Now, some fields need to be numerical even if the data dictionary does not think they are.

if (verbose)
  cat("Manually adjusting some fields...\n")

raw.data <- raw.data %>%
  mutate(daily_fio2_lborres = map2_dbl(subjid, daily_fio2_lborres, function(x, y)
    careful.as.numeric(y, x, "daily_fio2_lborres"))) %>%
  mutate(age_estimateyears = map2_dbl(subjid, age_estimateyears, function(x, y)
    careful.as.numeric(y, x, "age_estimateyears"))) %>%
  mutate(hodur = map2_dbl(subjid, hodur, function(x, y)
    careful.as.numeric(y, x, "hodur"))) %>%
  mutate(invasive_prdur = map2_dbl(subjid, invasive_prdur, function(x, y)
    careful.as.numeric(y, x, "invasive_prdur")))

# Replace the fractional ages

raw.data <-
  raw.data %>%  mutate(age_estimateyears = map2_dbl(age_estimateyears, subjid, function(x, y) {
    careful.fractional.age(x, y, "age_estimateyears")
  }))

# Demographic data is in the first row

if (verbose)
  cat("Making patient data frame...\n")

demog.data <-
  raw.data %>% group_by(subjid) %>% slice(1) %>% ungroup()

# Join in a copy of all rows as the events column for each patient

event.data <-
  raw.data %>% group_by(subjid) %>% nest() %>% dplyr::rename(events = data) %>% ungroup()

if (verbose)
  cat("Joining events tables...\n")

patient.data <- demog.data %>% left_join(event.data)  %>%
  # cut out any rows where the IDs suggest test data
  filter(!str_detect(subjid, "[tT][eE][sS][tT]"))

# This is looking for individuals with multiple exit rows and the same ID, which are probably duplicates and excluded

if (verbose)
  cat("Identifying duplicate IDs...\n")

patient.data <- patient.data %>%
  mutate(multiple.exit.rows  = map2_lgl(subjid, events, function(y, x) {
    outcome.rows <-
      x %>% filter((
        startsWith(redcap_event_name, "dischargeoutcome") |
          startsWith(redcap_event_name, "dischargedeath")
      ) & !is.na(dsterm))
    nrow(outcome.rows) > 1
  }))

for (probable.duplicate.id in patient.data %>% filter(multiple.exit.rows) %>% pull(subjid)) {
  warning(glue(
    "Probable duplicate patient ID {probable.duplicate.id}. Ignoring this ID.\n"
  ))
}

patient.data <- patient.data %>%
  filter(!multiple.exit.rows) %>%
  dplyr::select(-multiple.exit.rows)

#### Comorbidities, symptoms, and treatments ####

# read the data dictionary to get lists of columns for symptoms at admission, comorbidities, and treatments

if (verbose)
  cat("Making reference tables for comorbidiities, symptoms, and treatments...\n")

d.dict <- data.dict %>%
  dplyr::select(`Variable / Field Name`, `Form Name`, `Field Type`, `Field Label`) %>%
  dplyr::rename(
    field.name = `Variable / Field Name`,
    form.name = `Form Name`,
    field.type = `Field Type`,
    field.label = `Field Label`
  )

comorbidities.colnames <-
  d.dict %>% filter(
    form.name == "comorbidities" &
      field.type == "radio" &
      field.name != "diabetes_type_mhyn" &
      field.name != "stercap_vsorres" &
      field.name != "dehydration_vsorres"
  ) %>%
  filter(!startsWith(field.name, "vulnerable")) %>%
  pull(field.name)
admission.symptoms.colnames <- d.dict %>%
  filter(
    form.name == "admission_signs_and_symptoms" &
      field.type == "radio" &
      field.name != "bleed_ceterm_v2" &
      field.name != "temp_vsorresu" &
      field.name != "oxy_vsorresu"  &
      field.name != "stercap_vsorres" &
      field.name != "dehydration_vsorres" &
      field.name != "no_symptoms"
  ) %>%
  pull(field.name)
treatment.colnames <- d.dict %>%
  filter(
    form.name == "treatment" &
      field.type == "radio" &
      field.name != "bloodgroup" &
      field.label != "Would you like to add another antibiotic?" &
      field.label != "Would you like to add another Corticosteroid agent?"
  ) %>%
  pull(field.name)

#### COMORBIDITIES ####

comorbidities.labels <- d.dict %>%
  filter(form.name == "comorbidities" & field.type == "radio") %>%
  filter(field.name != "diabetes_type_mhyn") %>%
  filter(!startsWith(field.name, "vulnerable")) %>%
  pull(field.label) %>%
  map_chr(function(x)
    str_split_fixed(x, "\\(", Inf)[1]) %>%
  map_chr(function(x)
    sub("\\s+$", "", x))

# At some point, farting around with regexes is more trouble than its worth

comorbidities.labels[1] <- "Chronic cardiac disease"
comorbidities.labels[19] <- "Other"

comorbidities <-
  tibble(field = comorbidities.colnames, label = comorbidities.labels)

# Add pregnancy to the list

comorbidities <-
  bind_rows(comorbidities, tibble(field = "pregnancy", label = "Pregnancy"))

# recode pregnancy for the sake of the denominator

# Group liver disease categories

for (cn in c("mildliver", "modliv", "liver_mhyn")) {
  if (!(cn %in% colnames(patient.data))) {
    patient.data <- patient.data %>% mutate(!!cn := NA)
  }
}

patient.data <- patient.data %>%
  mutate(liver.disease = pmap_dbl(list(mildliver, modliv, liver_mhyn), function(mild, moderate, any) {
    if (is.na(mild) & is.na(moderate) & is.na(any)) {
      NA
    } else if (!is.na(any)) {
      any
    } else if (is.na(mild)) {
      moderate
    } else if (is.na(moderate)) {
      mild
    } else if (mild == 1 | moderate == 1) {
      1
    } else if (mild == 2 & moderate == 2) {
      2
    } else {
      3
    }
  }))

comorbidities <-
  comorbidities %>% bind_rows(list(field = "liver.disease", label = "Liver disease")) %>%
  filter(field != "mildliver" & field != "modliv")

# Group diabetes categories

patient.data <- patient.data %>%
  mutate(diabetes = pmap_dbl(list(diabetes_mhyn, diabetescom_mhyn, diabetes_mhyn), function(simple, complex, any) {
    if (is.na(simple) & is.na(complex) & is.na(any)) {
      NA
    } else if (!is.na(any)) {
      # any is from RAPID currently and should not overlap with the others
      any
    } else if (is.na(simple)) {
      complex
    } else if (is.na(complex)) {
      simple
    } else if (simple == 1 | complex == 1) {
      1
    } else if (simple == 2 & complex == 2) {
      2
    } else {
      2
    }
  }))

comorbidities <-
  comorbidities %>% bind_rows(list(field = "diabetes", label = "Diabetes")) %>%
  filter(field != "diabetes_mhyn" & field != "diabetescom_mhyn")


#### SYMPTOMS ####

# Note that bleed_ceterm_v2 is wrongly described as a radio button; it is free text

admission.symptoms.labels <- d.dict %>%
  filter(
    form.name == "admission_signs_and_symptoms" &
      # startsWith(field.label, "4") &
      field.type == "radio" &
      field.name != "bleed_ceterm_v2" &
      field.name != "temp_vsorresu" &
      field.name != "oxy_vsorresu" &
      field.name != "stercap_vsorres" &
      field.name != "dehydration_vsorres" &
      field.name != "no_symptoms"
  ) %>%
  pull(field.label) %>%
  # str_match(pattern = "4a\\.[0-9]+\\.[\\.]?[0-9]?\\s(.*)") %>%
  map_chr(function(x)
    str_split_fixed(x, "\\(", Inf)[1]) %>%
  map_chr(function(x)
    sub("\\s+$", "", x)) %>%
  map_chr(function(x)
    sub("\\?$", "", x)) %>%
  map_chr(function(x)
    sub(":$", "", x))

admission.symptoms.labels[2] <- "Cough: no sputum"

admission.symptoms <-
  tibble(field = admission.symptoms.colnames, label = admission.symptoms.labels)

# Cough records have not been entered coherently, and are recoded to be mutually exclusive. Someone with a cough with sputum does not have a cough without it

patient.data <- patient.data %>%
  mutate(cough.cols = pmap(list(cough_ceoccur_v2_2, cough_ceoccur_v2, coughsput_ceoccur_v2, coughhb_ceoccur_v2), function(c2, c1.c, c1.s, c1.b){
    if(!is.na(c2)){
      cough.any <- case_when(c2 == 0 ~ FALSE,
                             c2 == 1 ~ TRUE,
                             c2 == 2 ~ TRUE,
                             c2 == 3 ~ TRUE,
                             TRUE ~ NA)
    } else {
      cough.any <- case_when(any(c(c1.c, c1.s, c1.b) == 1) ~ TRUE,
                             all(c(c1.c, c1.s, c1.b) == 2) ~ FALSE,
                             TRUE ~ NA)
    }
    
    if(is.na(c2) & any(is.na(c(c1.c, c1.s, c1.b)))){
      cough.nosputum <- NA
      cough.sputum <- NA
      cough.bloodysputum <- NA
    } else if(!is.na(c2)){
      if(c2 == 4){
        cough.nosputum <- NA
        cough.sputum <- NA
        cough.bloodysputum <- NA
      } else if(c2 == 0){
        cough.nosputum <- 2
        cough.sputum <- 2
        cough.bloodysputum <- 2
      } else if(c2 == 1){
        cough.nosputum <- 1
        cough.sputum <- 2
        cough.bloodysputum <- 2
      } else if(c2 == 2){
        cough.nosputum <- 2
        cough.sputum <- 1
        cough.bloodysputum <- 2
      } else {
        cough.nosputum <- 2
        cough.sputum <- 2
        cough.bloodysputum <- 1
      } 
    } else {
      if(any(c(c1.c, c1.s, c1.b) == 3) | any(is.na(c(c1.c, c1.s, c1.b)))){
        cough.nosputum <- NA
        cough.sputum <- NA
        cough.bloodysputum <- NA
      } else if(all(c(c1.c, c1.s, c1.b) == 2)){
        cough.nosputum <- 2
        cough.sputum <- 2
        cough.bloodysputum <- 2
      } else if(c1.s == 1){
        cough.nosputum <- 2
        if(c1.b == 1){
          cough.sputum <- 2
          cough.bloodysputum <- 1
        } else {
          cough.sputum <- 1
          cough.bloodysputum <- 2
        }
      } else if(c1.b == 1) {
        cough.nosputum <- 2
        cough.sputum <- 2
        cough.bloodysputum <- 1
      } else {
        cough.nosputum <- c1.c
        cough.sputum <- 2
        cough.bloodysputum <- 2
      }
    }
    list(cough.any = cough.any, cough.sputum = cough.sputum, cough.nosputum = cough.nosputum, cough.bloodysputum = cough.bloodysputum)
  })) %>%
  bind_cols(., bind_rows(!!!.$cough.cols)) %>%
  dplyr::select(-cough.cols) 

admission.symptoms <-
  admission.symptoms %>% bind_rows(list(field = "cough.nosputum", label = "Cough (no sputum)")) %>%
  bind_rows(list(field = "cough.sputum", label = "Cough (with sputum)")) %>%
  bind_rows(list(field = "cough.bloodysputum", label = "Cough (bloody sputum / haemoptysis)")) %>%
  filter(
    field != "cough_ceoccur_v2" &
      field != "coughsput_ceoccur_v2" & 
      field != "coughhb_ceoccur_v2" &
      field != "cough_ceoccur_v2_2"
  )

# Group shortness of breath categories

patient.data <- patient.data %>%
  mutate(shortness.breath = map2_dbl(shortbreath_ceoccur_v2, lowerchest_ceoccur_v2, function(adult, paed) {
    if (is.na(adult) & is.na(paed)) {
      NA
    } else if (is.na(adult)) {
      paed
    } else if (is.na(paed)) {
      adult
    } else if (adult == 1 | paed == 1) {
      1
    } else if (adult == 2 & paed == 2) {
      2
    } else {
      2
    }
  }))

admission.symptoms <-
  admission.symptoms %>% bind_rows(list(field = "shortness.breath", label = "Shortness of breath")) %>%
  filter(field != "shortbreath_ceoccur_v2" &
           field != "lowerchest_ceoccur_v2")



#### TREATMENTS ####

treatment.labels <- d.dict %>%
  filter(
    form.name == "treatment" &
      field.type == "radio" &
      field.name != "bloodgroup" &
      field.label != "Would you like to add another antibiotic?" &
      field.label != "Would you like to add another Corticosteroid agent?"
  ) %>%
  pull(field.label) %>%
  # str_match(pattern = "6\\.[0-9]+[\\.]?[0-9]?[\\.]?\\s(.*)") %>%
  # as_tibble() %>%
  # pull(2) %>%
  map_chr(function(x)
    str_split_fixed(x, "\\(", Inf)[1]) %>%
  # I don't know why you can't figure this out nicely. Do it later.
  map_chr(function(x)
    sub("\\s+$", "", x)) %>%
  map_chr(function(x)
    sub("\\?+$", "", x)) %>%
  map_chr(function(x)
    sub("\\s+$", "", x))

treatment.labels[5] <- "Off-label / compassionate use medications"
treatment.labels[8] <- "Nasal / mask oxygen therapy"
treatment.labels[9] <- "High flow oxygen therapy"
treatment.labels[13] <- "Inhaled nitric oxide"
treatment.labels[14] <- "Tracheostomy"
treatment.labels[18] <- "Other"

treatments <-
  tibble(field = treatment.colnames, label = treatment.labels)

# This function extracts a named column from the events table. If sanity.check == T it expects only one non-NA value in that column

extract.named.column.from.events <-
  function(events.tibble,
           subjid,
           column.name,
           sanity.check = FALSE) {
    out <-
      events.tibble %>% filter(!is.na(!!as.name(column.name))) %>% pull(column.name)
    
    if (length(out) > 1 & sanity.check) {
      
      write_lines(glue("{subjid},{column.name}"), "patients_w_dup_entries.txt", append = TRUE)
      
      warning(glue(
        "Too many entries in column {column.name} for patient {subjid}"
      ))
      out[1]
    } else if (length(out) == 0) {
      NA
    } else {
      out
    }
  }

# Add new columns with more self-explanatory names as needed

if (verbose)
  cat("Adding new columns...\n")


patient.data <- patient.data %>%
  # Consolidated age is the exact age at enrolment if this is present. Otherwise it is taken from the estimated age column. Negative and huge values are changed to NA.
  dplyr::mutate(consolidated.age = pmap_dbl(list(age_estimateyears, agedat, dsstdat), function(ageest, dob, doa) {
    if (is.na(dob)) {
      ageest
    } else {
      out <- floor(decimal_date(doa) - decimal_date(dob))
      ifelse(out >= 0 & out <= 120, out, NA)
    }
  })) %>%
  # Age groups in five and ten year incerements
  dplyr::mutate(agegp5 = cut(consolidated.age, c(seq(0, 90, by = 5), 120), right = FALSE)) %>%
  dplyr::mutate(agegp5 = fct_relabel(agegp5, function(a) {
    # make nicer labels
    temp <- substr(a, 2, nchar(a) - 1)
    newlabels <- map_chr(temp, function(x) {
      components <- as.numeric(str_split_fixed(x, ",", Inf))
      components[2] <- components[2] - 1
      paste(components, collapse = "-")
    })
    str_replace(newlabels, "90-119", "90+")
  })) %>%
  dplyr::mutate(agegp10 = cut(consolidated.age, c(seq(0, 70, by = 10), 120), right = FALSE)) %>%
  dplyr::mutate(agegp10 = fct_relabel(agegp10, function(a) {
    # make nicer labels
    temp <- substr(a, 2, nchar(a) - 1)
    newlabels <- map_chr(temp, function(x) {
      components <- as.numeric(str_split_fixed(x, ",", Inf))
      components[2] <- components[2] - 1
      paste(components, collapse = "-")
    })
    str_replace(newlabels, "70-119", "70+")
  }))

# Make a nicer pregnancy variable

patient.data <- patient.data %>%
  mutate(pregnancy = pmap_dbl(list(pregyn_rptestcd, sex, consolidated.age), function(preg, sx, age) {
    if (is.na(preg)) {
      # use the same rules as the UK data dictionary
      if (!is.na(sx) & sx == 1) {
        2
      } else if (!is.na(age) & (age < 12 | age > 55)) {
        2
      } else {
        3
      }
    } else if (preg == 999) {
      2
    } else if (preg == 998) {
      3
    } else if (preg == 0) {
      2
    } else {
      preg
    }
  }))


patient.data <- patient.data %>%
  # check if symptoms, comorbidities and treatments were actually recorded
  dplyr::mutate(symptoms.recorded = pmap_lgl(list(
    !!!rlang::parse_exprs(admission.symptoms$field)
  ), ~ any(!is.na(c(
    ...
  ))))) %>%
  dplyr::mutate(comorbidities.recorded = pmap_lgl(list(
    !!!rlang::parse_exprs(comorbidities$field)
  ), ~ any(!is.na(c(
    ...
  ))))) %>%
  dplyr::mutate(treatments.recorded = map_lgl(events, function(x) {
    temp <-
      x %>% mutate(tr = pmap_lgl(list(
        !!!rlang::parse_exprs(treatments$field)
      ), ~ any(!is.na(c(
        ...
      )))))
    any(temp$tr)
  }))


patient.data <- patient.data %>%
  # exit date is whenever the patient leaves the site.
  dplyr::mutate(exit.date = map2_chr(subjid, events, function(y, x) {
    outcome.rows <-
      x %>% filter((
        startsWith(redcap_event_name, "dischargeoutcome") |
          startsWith(redcap_event_name, "dischargedeath")
      ) & !is.na(dsstdtc))
    if (nrow(outcome.rows) == 0) {
      return(NA)
    } else {
      if (nrow(outcome.rows) > 1) {
        warning(glue("Multiple exit dates for patient {y}"))
      }
      return(outcome.rows  %>% slice(nrow(outcome.rows)) %>% pull(dsstdtc) %>% as.character())
    }
  })) %>%
  dplyr::mutate(exit.date = as.Date(parse_date_time(exit.date, orders = c("ymd", "dmy")))) %>%
  # exit code is the reason for leaving the site.
  dplyr::mutate(exit.code = pmap_chr(list(subjid, events, exit.date), function(y, x, ed) {
    outcome.rows <-
      x %>% filter((
        startsWith(redcap_event_name, "dischargeoutcome") |
          startsWith(redcap_event_name, "dischargedeath")
      ) & !is.na(dsterm))
    if (nrow(outcome.rows) == 0) {
      if(!is.na(ed)){
        return("unknown")
      } else {
        return(NA)
      }
    } else {
      return(switch(
        outcome.rows %>% pull(dsterm) %>% as.character(),
        "1" = "discharge",
        "2" = "hospitalisation",
        "3" = "transfer",
        "4" = "death",
        "5" = "transfer.palliative",
        "6" = "unknown"
      ))
    }
  })) %>%
  dplyr::mutate(exit.code = factor(
    exit.code,
    levels = c(
      "discharge",
      "hospitalisation",
      "transfer",
      "death",
      "transfer.palliative",
      "unknown"
    )
  )) %>%
  # censorship occurs if the patient is still in site. 
  dplyr::mutate(censored = pmap_lgl(list(subjid, events, exit.date), function(y, x, ed) {
    if (x %>% pull(redcap_event_name) %>% startsWith("discharge") %>% any() %>% not()) {
      # still in site
      return(TRUE)
    } else {
      temp <-
        x %>% filter((
          startsWith(redcap_event_name, "dischargeoutcome") |
            startsWith(redcap_event_name, "dischargedeath")
        ) & !is.na(dsterm))
      if (nrow(temp) == 0) {
        return(is.na(ed))
      } else {
        # either we have a given exit date or a given exit term
        return((temp %>% pull(dsterm) %>% is.na() %>% any()) & is.na(ed))
      }
    }
  }))

patient.data <- patient.data %>%
  # outcome is death, discharge or other for transfers etc
  dplyr::mutate(outcome = pmap_chr(list(subjid, censored, events), function(id, x, y) {
    if (x) {
      return("censored")
    } else {
      
      temp <- y %>% filter((
        startsWith(redcap_event_name, "dischargeoutcome") |
          startsWith(redcap_event_name, "dischargedeath")
      ) & !is.na(dsterm)) %>% pull(dsterm) %>% as.character()
      if(length(temp) == 0){
        return(NA_character_)
      } else {
        return(switch(
          temp,
          "1" = "discharge",
          "4" = "death",
          NA_character_
        ))
      }
    }
  })) %>%
  # is the outcome date known?
  dplyr::mutate(outcome.date.known = map2_dbl(outcome, events, function(x, y) {
    if (!(x %in% c("discharge", "death"))) {
      return(2)
    } else {
      return(y %>% filter((
        startsWith(redcap_event_name, "dischargeoutcome") |
          startsWith(redcap_event_name, "dischargedeath")
      ) & !is.na(dsterm)) %>% pull(dsstdtcyn))
    }
  })) %>%
  # If so, what is it?
  dplyr::mutate(outcome.date = map2_chr(outcome, events, function(x, y) {
    if (!(x %in% c("discharge", "death"))) {
      return(NA)
    } else {
      if (length(y %>% filter((
        startsWith(redcap_event_name, "dischargeoutcome") |
        startsWith(redcap_event_name, "dischargedeath")
      ) & !is.na(dsterm)) %>% pull(dsstdtc) %>% as.character()) > 1) {
        stop("Multiple outcome dates?")
      }
      return(y %>% filter((
        startsWith(redcap_event_name, "dischargeoutcome") |
          startsWith(redcap_event_name, "dischargedeath")
      ) & !is.na(dsterm)) %>% pull(dsstdtc) %>% as.character())
    }
  })) %>%
  # oh for purrr::map_date!!!
  dplyr::mutate(outcome.date = as.Date(parse_date_time(outcome.date, orders = c("ymd","dmy")))) %>%
  # Death and discharge dates are NA if the patient is not dead/discharged
  dplyr::mutate(death.date = map2_chr(outcome, outcome.date, function(x, y) {
    if (is.na(y)) {
      NA
    }
    ifelse(x == "death", as.character(y), NA)
  })) %>%
  dplyr::mutate(death.date = as.Date(parse_date_time(death.date, orders = c("ymd","dmy")))) %>%
  dplyr::mutate(discharge.date = map2_chr(outcome, outcome.date, function(x, y) {
    if (is.na(y)) {
      NA
    }
    ifelse(x == "discharge", as.character(y), NA)
  }))  %>%
  dplyr::mutate(discharge.date = as.Date(parse_date_time(discharge.date, orders = c("ymd","dmy"))))



patient.data <- patient.data %>%
  # ICU start date
  dplyr::mutate(ICU.start.date = map2_chr(events, subjid, function(x, id)
    as.character(extract.named.column.from.events(x, id, "icu_hostdat", TRUE)))) %>%
  dplyr::mutate(ICU.start.date = as.Date(parse_date_time(ICU.start.date, orders = c("ymd", "dmy")))) %>%
  # ICU end date
  dplyr::mutate(ICU.end.date = map2_chr(events, subjid, function(x, id)
    as.character(extract.named.column.from.events(x, id, "icu_hoendat", TRUE)))) %>%
  dplyr::mutate(ICU.end.date = as.Date(parse_date_time(ICU.end.date,  orders = c("ymd", "dmy")))) %>%
  # ICU duration is a variable that may not have been entered, and if not it may be calculable
  dplyr::mutate(ICU.duration.temp = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "hodur", TRUE))) %>%
  dplyr::mutate(ICU.duration = pmap_dbl(list(subjid, ICU.start.date, ICU.end.date, ICU.duration.temp),
                                        function(id, st, en, durt) {
                                          print(id)
                                          if (all(is.na(c(as.character(st), as.character(en), durt)))) {
                                            NA
                                          } else if (any(is.na(c(st, en)) &
                                                         !is.na(durt))) {
                                            durt
                                          } else if (is.na(durt)) {
                                            as.numeric(difftime(en, st))
                                          } else {
                                            possibilities <- c(durt, as.numeric(difftime(en, st)))
                                            # split the difference if neither is absurd. Hopefully they are usually the same.
                                            mean(possibilities[which(possibilities >=
                                                                       0 & possibilities <= 200)])
                                          }
                                        })) %>%
  # IMV duration
  dplyr::mutate(IMV.duration  = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "invasive_prdur", TRUE))) %>%
  # these are just for the sake of having more self-explanatory column names
  dplyr::mutate(admission.date = hostdat) %>%
  dplyr::mutate(enrolment.date = dsstdat) %>%
  dplyr::mutate(onset.date = cestdat) %>%
  # start.date is either the admission date or the date of symptom onset, whichever is _later_ - nosocomial cases are counted from disease onset
  dplyr::mutate(start.date = map2_chr(admission.date, onset.date, function(x, y) {
    suppressWarnings(as.character(max(x, y, na.rm = T)))
  })) %>%
  dplyr::mutate(start.date = as.Date(parse_date_time(start.date, orders = c("ymd", "dmy"))))


# Various boolean treatment flags
patient.data <- patient.data %>%
  dplyr::mutate(antiviral.any = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "antiviral_cmyn", TRUE))) %>%
  dplyr::mutate(antiviral.Ribavirin = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "antiviral_cmtrt___1", TRUE))) %>%
  dplyr::mutate(
    antiviral.Lopinavir.Ritonvir = map2_dbl(events, subjid, function(x, id)
      extract.named.column.from.events(x, id, "antiviral_cmtrt___2", TRUE))
  ) %>%
  dplyr::mutate(
    antiviral.Interferon.alpha = map2_dbl(events, subjid, function(x, id)
      extract.named.column.from.events(x, id, "antiviral_cmtrt___3", TRUE))
  ) %>%
  dplyr::mutate(
    antiviral.Interferon.beta = map2_dbl(events, subjid, function(x, id)
      extract.named.column.from.events(x, id, "antiviral_cmtrt___4", TRUE))
  ) %>%
  dplyr::mutate(
    antiviral.Neuraminidase.inhibitors = map2_dbl(events, subjid, function(x, id)
      extract.named.column.from.events(x, id, "antiviral_cmtrt___5", TRUE))
  ) %>%
  dplyr::mutate(antiviral.other = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "antiviral_cmtrt___6", TRUE)))   %>%
  dplyr::mutate(antiviral.freetext = map2_chr(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "antiviral_cmtype", TRUE))) %>%
  dplyr::mutate(antibiotic.any = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "antibiotic_cmyn", TRUE))) %>%
  dplyr::mutate(antifungal.any = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "antifung_cmyn", TRUE))) %>%
  dplyr::mutate(steroid.any = map2_dbl(events, subjid, function(x, id)
    extract.named.column.from.events(x, id, "corticost_cmyn", TRUE)))

####Date wrangling ####

# This is the function to return information about a patient's time on an intervention (e.g. IMV) from both daily and final forms

if (verbose)
  cat("Wrangling dates for treatment modalities...\n")

process.event.dates <-
  function(events.tbl,
           summary.status.name,
           daily.status.name) {
    subtbl <-
      events.tbl %>% dplyr::select(dsstdat,
                                   daily_dsstdat,
                                   !!summary.status.name,
                                   !!daily.status.name)
    
    colnames(subtbl)[3:4] <- c("summary.col", "daily.col")
    
    check.rows <-
      # rows with some information on this variable
      subtbl %>% filter(!is.na(summary.col) | !is.na(daily.col))
    if (nrow(check.rows) == 0) {
      # we don't know if this person ever had this treatment
      ever <- NA
    } else {
      ever <-
        any((check.rows$summary.col == 1) |
              (check.rows$daily.col == 1),
            na.rm = T)
    }
    
    
    rows <-
      subtbl %>% filter(!is.na(daily.col) &
                          !(is.na(dsstdat) & is.na(daily_dsstdat))) %>%
      mutate(consolidated.dssdat = map2_chr(dsstdat, daily_dsstdat, function(x, y)
        ifelse(is.na(y), as.character(x), as.character(y)))) %>%
      mutate(consolidated.dssdat = as.Date(parse_date_time(consolidated.dssdat, orders = c("ymd","dmy"))))
    
    if (nrow(rows) == 0) {
      # no reference to this treatment
      start.date <- NA
      end.date <- NA
      first.after.date <- NA
      multiple.periods <- NA
    } else if (!any(rows$daily.col == 1)) {
      # didn't have this treatment so dates are irrelevent
      start.date <- NA
      end.date <- NA
      first.after.date <- NA
      multiple.periods <- NA
    } else {
      # did have this treatment
      start.date <-
        rows %>% filter(daily.col == 1) %>% slice(1) %>% pull(consolidated.dssdat)
      last.date <-
        rows %>% filter(daily.col == 1) %>% slice(n()) %>% pull(consolidated.dssdat)
      if (is.na(start.date)) {
        # sometimes happens. ever == TRUE but dates unknown
        end.date <- NA
        first.after.date <- NA
      } else if (last.date == rows %>% filter(!is.na(consolidated.dssdat)) %>% slice(n()) %>% pull(consolidated.dssdat)) {
        # Patient was on at last report
        end.date <- NA
        first.after.date <- NA
      } else {
        # They were off at the next report
        end.date <- last.date
        
        next.report.row <-
          max(which(rows$consolidated.dssdat == last.date)) + 1
        first.after.date <- rows$consolidated.dssdat[next.report.row]
      }
      if (nrow(rows) <= 2 | is.na(start.date)) {
        multiple.periods <- F
      } else {
        # we are looking for the number of instances of 2 (no) then 1 (yes). If this is more than 1, or more than 0 with the first report
        # then the patient went on multiple times
        temp <-
          map_dbl(2:nrow(rows), function(x)
            rows$daily.col[x] - rows$daily.col[x - 1])
        multiple.periods <-
          length(which(temp == -1)) > 1 |
          (length(which(temp == -1)) > 0 &  rows$daily.col[1] == 1)
      }
    }
    list(
      ever = ever,
      start.date = start.date,
      end.date = end.date,
      first.after.date = first.after.date,
      multiple.periods = multiple.periods
    )
  }




patient.data <- patient.data %>%
  # NIV
  mutate(NIMV.cols  = map(events, function(el) {
    process.event.dates(el, "noninvasive_proccur", "daily_noninvasive_prtrt")
  })) %>%
  mutate(NIMV.cols = map(NIMV.cols, function(x) {
    names(x) <- glue("NIMV.{names(x)}")
    x
  })) %>%
  {
    bind_cols(., bind_rows(!!!.$NIMV.cols))
  } %>%
  dplyr::select(-NIMV.cols) %>%
  # IMV
  mutate(IMV.cols  = map2(subjid, events, function(id, el) {
    process.event.dates(el, "invasive_proccur", "daily_invasive_prtrt")
  })) %>%
  mutate(IMV.cols = map(IMV.cols, function(x) {
    names(x) <- glue("IMV.{names(x)}")
    x
  })) %>%
  {
    bind_cols(., bind_rows(!!!.$IMV.cols))
  } %>%
  dplyr::select(-IMV.cols) %>%
  # ECMO
  mutate(ECMO.cols  = map2(subjid, events, function(id, el) {
    process.event.dates(el, "extracorp_prtrt", "daily_ecmo_prtrt")
  })) %>%
  mutate(ECMO.cols = map(ECMO.cols, function(x) {
    names(x) <- glue("ECMO.{names(x)}")
    x
  })) %>%
  {
    bind_cols(., bind_rows(!!!.$ECMO.cols))
  } %>%
  dplyr::select(-ECMO.cols) %>%
  # ICU - we already have this information incompletely from icu_hostdat, icu_hoendat and hodur,
  # so these columns are "ICU2" and we consolidate later
  mutate(ICU.cols  = map(events, function(el) {
    process.event.dates(el, "icu_hoterm", "daily_hoterm")
  })) %>%
  mutate(ICU.cols = map(ICU.cols, function(x) {
    names(x) <- glue("ICU2.{names(x)}")
    x
  })) %>%
  {
    bind_cols(., bind_rows(!!!.$ICU.cols))
  } %>%
  dplyr::select(-ICU.cols) %>%
  # RRT
  mutate(RRT.cols  = map(events, function(el) {
    process.event.dates(el, "rrt_prtrt", "daily_rrt_cmtrt")
  })) %>%
  mutate(RRT.cols = map(RRT.cols, function(x) {
    names(x) <- glue("RRT.{names(x)}")
    x
  })) %>%
  {
    bind_cols(., bind_rows(!!!.$RRT.cols))
  } %>%
  dplyr::select(-RRT.cols) %>%
  # Inotrope
  mutate(Inotrope.cols  = map(events, function(el) {
    process.event.dates(el, "inotrop_cmtrt", "daily_inotrope_cmyn")
  })) %>%
  mutate(Inotrope.cols = map(Inotrope.cols, function(x) {
    names(x) <- glue("Inotrope.{names(x)}")
    x
  })) %>%
  {
    bind_cols(., bind_rows(!!!.$Inotrope.cols))
  } %>%
  dplyr::select(-Inotrope.cols)

patient.data <- patient.data %>%
  # O2t
  mutate(O2t.cols  = map(events, function(el) {
    process.event.dates(el, "oxygen_cmoccur", "daily_nasaloxy_cmtrt")
  })) %>%
  mutate(O2t.cols = map(O2t.cols, function(x) {
    names(x) <- glue("O2t.{names(x)}")
    x
  })) %>%
  {
    bind_cols(., bind_rows(!!!.$O2t.cols))
  } %>%
  dplyr::select(-O2t.cols)

# O2 ever - more complex

patient.data <- patient.data %>%
  mutate(O2.ever = map_lgl(events, function(x) {
    x2 <-
      x %>% filter(
        !is.na(daily_fio2_lborres) |
          !is.na(daily_nasaloxy_cmtrt) | !is.na(oxygen_cmoccur)
      )
    if (nrow(x2) == 0) {
      NA
    } else {
      x2 <-
        x2 %>% mutate(O2.ever = daily_fio2_lborres > .21 |
                        daily_nasaloxy_cmtrt == 1 | oxygen_cmoccur == 1)
      any(x2$O2.ever, na.rm = T)
    }
  }))

# ICU.start.date and ICU.end.date have values from daily sheets but don't always have values from the outcome sheet. Where available, the outcome sheet is preferred.

patient.data <- patient.data %>%
  # ICU.ever is either ICU2.ever or non-NA icu_hoterm. At the moment, NA ICU2.evers lead to NA ICU.evers in the absence of a icu_hoterm.
  # It's possible that they should be FALSE instead
  mutate(ICU.ever = !is.na(ICU.start.date) | ICU2.ever) %>%
  mutate(ICU.start.date = replace(ICU.start.date, is.na(ICU.start.date), ICU2.start.date[which(is.na(ICU.start.date))])) %>%
  mutate(ICU.end.date = replace(ICU.end.date, is.na(ICU.end.date), ICU2.end.date[which(is.na(ICU.end.date))])) %>%
  mutate(ICU.multiple.periods = ICU2.multiple.periods)

# if we can get those from the daily forms then we can get this
patient.data$ICU.duration[is.na(patient.data$ICU.duration) == TRUE] <-
  as.numeric(difftime(
    patient.data$ICU.end.date[is.na(patient.data$ICU.duration) == TRUE],
    patient.data$ICU.start.date[is.na(patient.data$ICU.duration) == TRUE],
    unit = "days"
  ))

# drop the ICU2 columns

patient.data2 <-
  patient.data %>% dplyr::select(-starts_with("ICU2"))

#### Calculation of time periods; most are self-explanatory ####

patient.data <- patient.data %>%
  dplyr::mutate(NIMV.duration = map2_dbl(NIMV.end.date, NIMV.start.date, function(x, y) {
    as.numeric(difftime(x, y,  unit = "days"))
  })) %>%
  dplyr::mutate(
    admission.to.exit = as.numeric(difftime(exit.date, admission.date,  unit =
                                              "days")),
    onset.to.admission = as.numeric(difftime(admission.date, onset.date, unit =
                                               "days")),
    start.to.exit = as.numeric(difftime(exit.date, start.date,  unit =
                                          "days"))
  ) %>%
  # censored if admission.to.exit is NA
  dplyr::mutate(admission.to.censored = map2_dbl(admission.to.exit, admission.date, function(x, y) {
    if (is.na(x)) {
      # censored until today
      as.numeric(difftime(ref.date, y,  unit = "days"))
    }
    else {
      NA
    }
  })) %>%
  # censored if admission.to.exit is NA
  dplyr::mutate(start.to.censored = map2_dbl(admission.to.exit, start.date, function(x, y) {
    if (is.na(x)) {
      as.numeric(difftime(ref.date, y,  unit = "days"))
    }
    else {
      NA
    }
  })) %>%
  dplyr::mutate(admission.to.death = pmap_dbl(list(exit.code, exit.date, admission.date), function(x, y, z) {
    if (!is.na(x) & x == "death") {
      as.numeric(difftime(y, z,  unit = "days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(start.to.death = pmap_dbl(list(exit.code, exit.date, start.date), function(x, y, z) {
    if (!is.na(x) & x == "death") {
      as.numeric(difftime(y, z,  unit = "days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(admission.to.discharge = pmap_dbl(list(exit.code, exit.date, admission.date), function(x, y, z) {
    if (!is.na(x) & x == "discharge") {
      as.numeric(difftime(y, z,  unit = "days"))
    } else {
      NA
    }
  })) %>%
  dplyr::mutate(start.to.discharge = pmap_dbl(list(exit.code, exit.date, start.date), function(x, y, z) {
    if (!is.na(x) & x == "discharge") {
      as.numeric(difftime(y, z,  unit = "days"))
    } else {
      NA
    }
  }))


# admission and start to various ongoing events
patient.data <- patient.data %>%
  mutate(
    admission.to.ICU = as.numeric(difftime(ICU.start.date, admission.date, unit =
                                             "days")),
    admission.to.IMV = as.numeric(difftime(IMV.start.date, admission.date, unit =
                                             "days")),
    admission.to.NIMV = as.numeric(difftime(NIMV.start.date, admission.date, unit =
                                              "days")),
    admission.to.ECMO = as.numeric(difftime(ECMO.start.date, admission.date, unit =
                                              "days")),
    admission.to.O2t = as.numeric(difftime(O2t.start.date, admission.date, unit =
                                             "days")),
    start.to.ICU = as.numeric(difftime(ICU.start.date, start.date, unit =
                                         "days")),
    start.to.IMV = as.numeric(difftime(IMV.start.date, start.date, unit =
                                         "days")),
    start.to.NIMV = as.numeric(difftime(NIMV.start.date, start.date, unit =
                                          "days")),
    start.to.ECMO = as.numeric(difftime(ECMO.start.date, start.date, unit =
                                          "days")),
    start.to.O2t = as.numeric(difftime(O2t.start.date, start.date, unit =
                                         "days"))
  )

#### Untangling COVID test fields ####

if (verbose)
  cat("Untangling SARS-CoV-19 test results...\n")

patient.data <- patient.data %>%
  mutate(cov.test.result =
           map2_dbl(events, subjid, function(x, id)
             extract.named.column.from.events(x, id, column.name = "corna_mbcat", sanity.check =  TRUE))) %>%
  mutate(cov.test.organism =
           map2_dbl(events, subjid, function(x, id)
             extract.named.column.from.events(x, id, column.name = "corna_mbcaty", sanity.check =  TRUE))) %>%
  mutate(
    cov.other.organism.freetext =
      map2_chr(events, subjid, function(x, id)
        extract.named.column.from.events(x, id, column.name = "coronaother_mborres", sanity.check =  TRUE))
  ) %>%
  mutate(any.test.result.1 =
           map2(events, subjid, function(x, id)
             extract.named.column.from.events(x, id, column.name = "mborres"))) %>%
  mutate(any.test.freetext.1 =
           map2(events, subjid, function(x, id)
             extract.named.column.from.events(x, id, column.name = "mbtestcd")))  %>%
  mutate(any.test.result.2 =
           map2(events, subjid, function(x, id)
             extract.named.column.from.events(x, id, column.name = "other_mbyn"))) %>%
  mutate(any.test.freetext.2 =
           map2(events, subjid, function(x, id)
             extract.named.column.from.events(x, id, column.name = "other_mborres")))


# Function to check for text that looks like it means SARS-CoV-2. Lots of typos in these fields. This may catch other coronaviruses.

probable.cov.freetext <- function(text) {
  str_detect(text, "[sS][aA][rR][sS]") |
    str_detect(text, "[cC][oO]r?[vV]") |
    str_detect(text, "[nN][cC][oO][cC]") |
    str_detect(text, "[nN][cC][vV][oO]") |
    str_detect(text, "[cC][oO][iI][vV][dD]") |
    str_detect(text, "[cC][vV][iI][dD]") |
    str_detect(text, "[cC][oO][vV][oO][iI][dD]") |
    str_detect(text, "[cC]or?n?o?n[ao]vir[iu]s") |
    str_detect(text, "[cC]OR?N?O?N[AO]VIR[IU]S") |
    str_detect(text, "[cC]or?n?o?n[ao]\\s[Vv]ir[iu]s") |
    str_detect(text, "[cC]OR?N?O?N[AO]\\sVIR[IU]S") |
    str_detect(text, "[Cc][Oo][Rr][Oo][Nn][Aa]")
}

# IMPORTANT: "positive.COV19.test" refers to "evidence for a positive test". FALSE is not evidence for a negative test.

patient.data <- patient.data %>%
  mutate(positive.COV19.test = pmap_lgl(list(
    subjid,
    cov.test.result,
    cov.test.organism,
    cov.other.organism.freetext,
    any.test.result.1,
    any.test.freetext.1,
    any.test.result.2,
    any.test.freetext.2
  ),
  function(id,
           ctest,
           cpos,
           cfreetext,
           anytest.1,
           anyfreetext.1,
           anytest.2,
           anyfreetext.2) {
    
    cpos.covlikely = probable.cov.freetext(cfreetext)
    anytestprocessed.1 <-
      as.numeric(anytest.1)
    anyfreetextprocessed.1 <-
      anyfreetext.1[which(!is.na(anytest.1))]
    anytestprocessed.2 <-
      as.numeric(anytest.2)
    anyfreetextprocessed.2 <-
      anyfreetext.2[which(!is.na(anytest.2))]
    if ((ctest %in% c(1, 2, 3)) &
        ((!is.na(cpos) &
          cpos == 1) | (cpos == 888 & !is.na(cfreetext) & cpos.covlikely))) {
      # You have COVID if:
      # 1) Your SARS-CoV-2 test result is a weak or strong positive for SARS-CoV-2
      # OR 2) Your SARS-CoV-2 test result is a weak or strong positive for "another virus" but the free text says that virus is actually SARS-CoV-2(!)
      return(TRUE)
    } else if (ctest %in% c(0, 4) |
               is.na(ctest) |
               (ctest  %in% c(1, 2) &
                (is.na(cpos) |
                 cpos == 2 | (
                   !is.na(cpos.covlikely) & !cpos.covlikely
                 )))) {
      # Patients get here if:
      # 1) They have a negative SARS-CoV-2 test
      # OR 2) They have an NA entry for SARS-CoV-2 test result
      # OR 3) They have a weak or strong positive test for a coronavirus infection AND
      #   3A) The entry for virus species is NA
      #   OR 3B) The entry for virus species is "MERS"
      #   OR 3C) The free text is missing or says that virus is not SARS-CoV-2
      if (length(anyfreetext.1) > 0 |
          length(anyfreetext.2) > 0) {
        # This examines the free text from the daily form, looking for COVID-like words
        otherpos.covlikely.1 = map_lgl(anyfreetextprocessed.1, probable.cov.freetext)
        otherpos.covlikely.2 = map_lgl(anyfreetextprocessed.2, probable.cov.freetext)
        if (any(
          !is.na(otherpos.covlikely.1) &
          !is.na(anytestprocessed.1) &
          otherpos.covlikely.1 & anytestprocessed.1 == 1
        ) |
        any(
          !is.na(otherpos.covlikely.2) &
          !is.na(anytestprocessed.2) &
          otherpos.covlikely.2 & anytestprocessed.2 == 1
        )) {
          # words found and the test was positive
          return(TRUE)
        } else {
          # words not found or the test was negative
          return(FALSE)
        }
      }
    } else {
      return(FALSE)
    }
  }))

# Duplicates can go now

duplicate.patients <- read_lines("patients_w_dup_entries.txt")
patient.data <- patient.data %>% filter(!(subjid %in% duplicate.patients))

# Impose the embargo

if (verbose)
  cat("Imposing the embargo...\n")

patient.data <-  patient.data %>%
  filter((admission.date <= embargo.limit &
            enrolment.date <= embargo.limit))


# Replace nonsensical ages

patient.data <- patient.data %>% mutate(consolidated.age = replace(consolidated.age, consolidated.age < 0 | consolidated.age > 120, NA))

# Clinical Care Asia site names need to be filled in

patient.data <- patient.data %>% mutate(site.name = map2_chr(subjid, site.name, function(sb, sn){
  case_when(!startsWith(sn, "A-") ~ sn,
            TRUE ~ substr(sb, 1, 12))
  
}))

save(patient.data, "processed_data.Rdata")

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