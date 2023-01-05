library(mgcv)
library(purrr)

# Formulas -----------------------------------------------------------------------------------------

make_formula <- function(vars, response, id_splines = NULL) {
  if (!is.null(id_splines)) {
    vars[id_splines] <- paste0("s(", vars[id_splines], ", bs = 'cr', k = 10)")
  }

  paste(vars, collapse = " + ") %>%
    paste(response, "~", .) %>%
    as.formula()
}

vars_full <- c("education", "gender", "urban", "engnat", "age", "hand", "religion", "orientation",
               "race", "voted", "married", "familysize", "eloquence")

# Variable selection -------------------------------------------------------------------------------

## Model space - complete search -------------------------------------------------------------------

search_all_models <- function(dt, vars, response, id_splines, run = TRUE) {
  dt_fix <- na.omit(dt, cols = c(vars, response))

  vars[id_splines] <- paste0("s(", vars[id_splines], ", bs = 'cr', k = 10)")

  all_formulas <- seq_along(vars) %>%
    lapply(combn, x = vars, simplify = FALSE) %>%
    flatten() %>%
    lapply(unlist) %>%
    c("1", .) %>%
    lapply(paste, collapse = " + ") %>%
    sapply(function(form) paste(response, "~", form))

  if (!run) return(all_formulas)

  vec_bic <- sapply(all_formulas, function(form) {
    BIC(gam(as.formula(form), data = dt_fix, family = gaussian(), method = "REML"))
  })

  # return
  message("Dropped ", nrow(dt) - nrow(dt_fix), " observations.")
  data.table(formula = all_formulas, BIC = vec_bic) %>%
    setkey("BIC")
}

if (file.exists("complete-search.rds")) {
  res <- readRDS("complete-search.rds")
  setDT(res)
} else {
  res <- search_all_models(dt_bigf, vars = vars_full, response = "openness", id_splines = c(5, 13))
}

## Final model - all complete observations ---------------------------------------------------------

m.lam.fin <- as.formula(res[1, formula]) %>%
  gam(data = dt_bigf, family = gaussian(), method = "REML")

# Sensitivity analysis -----------------------------------------------------------------------------

## Final model as binomial model ------------------------------------------------------------------

m.gam.fin <- sub("openness", "cbind(hits, misses)", res[1, formula]) %>%
  as.formula() %>%
  gam(data = dt_bigf, family = binomial(), method = "REML")

data.table(lam = residuals(m.lam.fin), gam = residuals(m.gam.fin, type = "response")) %>%
  ggplot(aes(x = lam, y = gam)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth()

data.table(lam = predict(m.lam.fin), gam = predict(m.gam.fin, type = "response")) %>%
  ggplot(aes(x = lam, y = gam)) +
  geom_jitter(height = 0.001, width = 0.001, alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth()

pred_lam <- predict(m.lam.fin)
rbind(
  data.table(pred = pred_lam, model = "lam",
             truth = dt_bigf[as.integer(names(pred_lam)), openness]),
  data.table(pred = predict(m.gam.fin, type = "response"), model = "gam",
             truth = dt_bigf[as.integer(names(pred_lam)), openness])
) %>%
  ggplot(aes(x = truth, y = pred, color = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(se = FALSE) +
  ylim(0, 1)

mean(residuals(m.lam.fin)^2)
mean(residuals(m.gam.fin, type = "response")^2)

### Quasi ------------------------------------------------------------------------------------------

# maybe REML is already quasi
m.gam.fin.quasi <- sub("openness", "cbind(hits, misses)", res[1, formula]) %>%
  as.formula() %>%
  gam(data = dt_bigf, family = quasibinomial(), method = "REML")

ggcross(m.gam.fin, m.gam.fin.quasi)  # just splines differ

mean(residuals(m.gam.fin, type = "response")^2)
mean(residuals(m.gam.fin.quasi, type = "response")^2)

## Optimizer ---------------------------------------------------------------------------------------

m.lam.fin.efs <- as.formula(res[1, formula]) %>%
  gam(data = dt_bigf, family = gaussian(), method = "REML", optimizer = "efs")

ggcross(m.lam.fin, m.lam.fin.efs)

# not checked: optimizer in complete search

## Smooth effect -----------------------------------------------------------------------------------

# not necessary: edf is large and significant

