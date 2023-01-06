library(mgcv)
library(purrr)

if (Sys.info()[["sysname"]] == "Linux") options(mc.cores = 3) else options(mc.cores = 1)

# Formulas -----------------------------------------------------------------------------------------

make_formula <- function(vars, response, id_splines = integer(0)) {
  if (length(id_splines) > 0) {
    vars[id_splines] <- paste0("s(", vars[id_splines], ", bs = 'cr', k = 10)")
  }

  paste(vars, collapse = " + ") %>% paste(response, "~", .)
}

vars_full <- c("education", "gender", "urban", "engnat", "age", "hand", "religion", "orientation",
               "race", "voted", "married", "familysize", "eloquence")

# Variable selection -------------------------------------------------------------------------------

## Model space - complete search -------------------------------------------------------------------

search_all_models <- function(dt, vars, response, id_splines, run = TRUE) {

  # use same data for all models
  dt_fix <- na.omit(dt, cols = c(vars, response))

  # formulate all models
  all_combinations <- seq_along(vars) %>%
    lapply(combn, x = vars, simplify = FALSE) %>%
    flatten() %>%
    lapply(unlist) %>%
    c("1", .)

  all_formulas <- lapply(all_combinations, function(vec_names, response, vars, id_splines) {
    make_formula(vec_names, response, which(vec_names %in% vars[id_splines]))
  }, response = response, vars = vars, id_splines = id_splines)

  # for tests
  if (!run) return(all_formulas)

  # this may take a while
  vec_aic <- parallel::mclapply(all_formulas, function(form) {
    AIC(gam(as.formula(form), data = dt_fix, family = gaussian(), method = "REML"))
  }) %>% unlist()

  # return
  message("Dropped ", nrow(dt) - nrow(dt_fix), " observations.")
  data.table(AIC = vec_aic, formula = all_formulas, vars = all_combinations) %>%
    setkey("AIC")
}

if (file.exists("complete-search-aic.rds")) {
  res <- readRDS("complete-search-aic.rds")
  setDT(res)
} else {
  res <- search_all_models(dt_bigf, vars = vars_full, response = "openness", id_splines = c(5, 13))
}

res[, probs := exp((AIC[[1]] - AIC) / 2)][, probs := probs / sum(probs)]

## Final model - all complete observations ---------------------------------------------------------

m.lam.fin <- as.formula(res[1, formula][[1]]) %>%
  gam(data = dt_bigf, family = gaussian(), method = "REML")

# Sensitivity analysis -----------------------------------------------------------------------------

## Final model as binomial model ------------------------------------------------------------------

m.gam.fin <- sub("openness", "cbind(hits, misses)", res[1, formula][[1]]) %>%
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
m.gam.fin.quasi <- sub("openness", "cbind(hits, misses)", res[1, formula][[1]]) %>%
  as.formula() %>%
  gam(data = dt_bigf, family = quasibinomial(), method = "REML")

ggcross(m.gam.fin, m.gam.fin.quasi)  # just splines differ

mean(residuals(m.gam.fin, type = "response")^2)
mean(residuals(m.gam.fin.quasi, type = "response")^2)

## Optimizer ---------------------------------------------------------------------------------------

m.lam.fin.efs <- as.formula(res[1, formula][[1]]) %>%
  gam(data = dt_bigf, family = gaussian(), method = "REML", optimizer = "efs")

ggcross(m.lam.fin, m.lam.fin.efs)

# not checked: optimizer in complete search

## Smooth effect -----------------------------------------------------------------------------------

# not necessary: edf is large and significant

