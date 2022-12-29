library(data.table)
library(purrr)
library(magrittr)

# Read dataset -------------------------------------------------------------------------------------
dt_bigf <- read.table("psychometric-data.csv", header = TRUE, sep = "\t", na.strings = "") %>%
  as.data.table()

# Format dataset -----------------------------------------------------------------------------------
dt_bigf[, c("country", "education", "urban", "gender", "engnat", "hand", "religion",
            "orientation", "race", "voted", "married", "os", "browser") :=
          .(factor(country),
            factor(education, labels = c("le_hs", "hs", "uni_degree", "grad_degree"), exclude = 0),
            factor(urban, labels = c("rural", "suburban", "urban"), exclude = 0),
            factor(gender, labels = c("male", "female", "other"), exclude = 0),
            factor(engnat, labels = c("yes", "no"), exclude = 0),
            factor(hand, labels = c("right", "left", "both"), exclude = 0),
            factor(religion, labels = c("agnostic", "atheist", "buddhist", "catholic", "mormon",
                                        "protestant", "christian_other", "hindu",
                                        "jewish", "muslim", "sikh", "other"), exclude = 0),
            factor(orientation, labels = c("heterosexual", "bisexual", "homosexual", "asexual",
                                           "other"), exclude = 0),
            factor(race, labels = c("asian", "arab", "black", "misc", "other"), exclude = 0),
            factor(voted, labels = c("yes", "no"), exclude = 0),
            factor(married, labels = c("no", "yes", "divorced"), exclude = 0),
            factor(operatingsystem), factor(browser))
]

# family size wrong - women with most children had 44
dt_bigf[familysize > 44 | familysize == 0, familysize := NA]

# age wrong
dt_bigf[age > 99, age := NA]

# zeros are NAs
names_o <- names(dt_bigf) %>% grep(pattern = "^O", value = TRUE)
dt_bigf[, (names_o) := lapply(.SD, function(col) ifelse(col == 0, NA, col)), .SDcols = names_o]

# Eloquence ----------------------------------------------------------------------------------------
vcl_fluke <- paste0("VCL", c(6, 9, 12))
dt_bigf[, (vcl_fluke) := lapply(.SD, function(col) as.integer(col == 0)), .SDcols = vcl_fluke]
dt_bigf[, eloquence := rowMeans(.SD), .SDcols = patterns("^VCL")]

# Openness -----------------------------------------------------------------------------------------
# start at zero
dt_bigf[, (names_o) := .SD - 1, .SDcols = names_o]

# numbers of missing answers per person (all values are missing for two obs)
dt_bigf[, O_NA := rowSums(is.na(.SD)), .SDcols = names_o]

# 1 question = 4 trials (range 0-4)
dt_bigf[, trials := 40 - 4 * O_NA]

# mark zero trials as missing
dt_bigf[trials == 0, trials := NA]

# binomial data and single score
dt_bigf[!is.na(trials), hits := rowSums(.SD, na.rm = TRUE), .SDcols = names_o]
dt_bigf[, c("misses", "openness") := .(trials - hits, hits / trials)]

# Removing columns ---------------------------------------------------------------------------------
dt_bigf[, c(grep("^[ACENOGV]", names(dt_bigf), value = TRUE), "operatingsystem") := NULL]
setcolorder(dt_bigf, neworder = c("introelapse", "testelapse", "surveyelapse", "browser",
                                  "screenw", "screenh", "os", "country", "education", "urban",
                                  "gender", "engnat", "age", "hand", "religion", "orientation",
                                  "race", "voted", "married", "familysize", "eloquence",
                                  "openness", "trials", "hits", "misses"))