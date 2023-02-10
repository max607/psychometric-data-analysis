# Read dataset -------------------------------------------------------------------------------------

dt_bigf <- read.table("psychometric-data.csv", header = TRUE, sep = "\t", na.strings = "") %>%
  as.data.table()

vars_full <- c("education", "gender", "urban", "engnat", "age", "hand", "religion", "orientation",
               "race", "voted", "married", "familysize", "eloquence")
vars_cat <- c("education", "gender", "urban", "engnat", "hand", "religion", "orientation", "race",
              "voted", "married")

# Format dataset -----------------------------------------------------------------------------------

dt_bigf[, c("country", "education", "urban", "gender", "engnat", "hand", "religion",
            "orientation", "race", "voted", "married", "os", "browser") :=
          .(factor(country),
            factor(education, levels = 1:4, labels = c("le_hs", "hs", "uni_degree", "grad_degree"),
                   exclude = 0),
            factor(urban, levels = 1:3, labels = c("rural", "suburban", "urban"), exclude = 0),
            factor(gender, levels = 1:3, labels = c("male", "female", "other"), exclude = 0),
            factor(engnat, levels = 2:1, labels = c("no", "yes"), exclude = 0),
            factor(hand, levels = 1:3, labels = c("right", "left", "both"), exclude = 0),
            factor(religion, levels = 1:12,
                   labels = c("agnostic", "atheist", "buddhist", "catholic", "mormon", "protestant",
                              "christian_other", "hindu", "jewish", "muslim", "sikh", "other"),
                   exclude = 0),
            factor(orientation, levels = 1:5,
                   labels = c("heterosexual", "bisexual", "homosexual", "asexual", "other"),
                   exclude = 0),
            factor(race, levels = c(4, 1:3, 5),
                   labels = c("misc", "asian", "arab", "black", "other"), exclude = 0),
            factor(voted, levels = 2:1, labels = c("no", "yes"), exclude = 0),
            factor(married, levels = 1:3, labels = c("no", "yes", "divorced"), exclude = 0),
            factor(operatingsystem), factor(browser))
]

# Identifying wrong values -------------------------------------------------------------------------

# family size wrong - women with most children had 44
dt_bigf[familysize > 20 | familysize == 0, familysize := NA]

# age wrong
dt_bigf[age > 99, age := NA]

# zeros are NAs
names_o <- names(dt_bigf) %>% grep(pattern = "^O", value = TRUE)
dt_bigf[, (names_o) := lapply(.SD, function(col) ifelse(col == 0, NA, col)), .SDcols = names_o]

# Eloquence ----------------------------------------------------------------------------------------

vcl_fluke <- paste0("VCL", c(6, 9, 12))
dt_bigf[, (vcl_fluke) := lapply(.SD, function(col) as.integer(col == 0)), .SDcols = vcl_fluke]
dt_bigf[, eloquence := rowSums(.SD), .SDcols = patterns("^VCL")]

# Openness -----------------------------------------------------------------------------------------

# start at zero
dt_bigf[, (names_o) := .SD - 1, .SDcols = names_o]

# p_corr1 <- cor(dt_bigf[, .SD, .SDcols = patterns("^O")], use = "pairwise.complete.obs") %>%
#   ggcorrplot::ggcorrplot(method = "square", type = "lower", lab = TRUE, digits = 1,
#                          ggtheme = ggplot2::theme_bw, outline.color = "black",
#                          colors = c("#1984c5", "#e2e2e2", "#c23728"), show.legend = FALSE)

openness_negation <- paste0("O", c(2, 4, 6))
dt_bigf[, (openness_negation) := 4 - .SD, .SDcols = openness_negation]

# p_corr2 <- cor(dt_bigf[, .SD, .SDcols = patterns("^O")], use = "pairwise.complete.obs") %>%
#   ggcorrplot::ggcorrplot(method = "square", type = "lower", lab = TRUE, digits = 1,
#                          ggtheme = ggplot2::theme_bw, outline.color = "black",
#                          colors = c("#1984c5", "#e2e2e2", "#c23728"), show.legend = FALSE)

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

# Technical sanity checks --------------------------------------------------------------------------

# filter 8 people who were way to quick
dt_bigf <- dt_bigf[testelapse > 75,]

# Complete observations ----------------------------------------------------------------------------

dt_bigf.fix <- na.omit(dt_bigf, cols = c(vars_full, "openness"))

