if (Sys.info()[["sysname"]] == "Linux") options(mc.cores = 3) else options(mc.cores = 1)

# Variable selection -------------------------------------------------------------------------------

if (file.exists("complete-search-aic.rds")) {
  res <- readRDS("complete-search-aic.rds")
  setDT(res)
} else {
  res <- search_all_models(dt_bigf, vars = vars_full, response = "openness", id_splines = c(5, 13))
  saveRDS(res, "complete-search-aic.rds")
}

res[, probs := exp((AIC[[1]] - AIC) / 2)][, probs := probs / sum(probs)]

## Final model - all complete observations ---------------------------------------------------------

m.lam.fin <- as.formula(res[1, formula][[1]]) %>%
  gam(data = dt_bigf, family = gaussian(), method = "REML")

m.lam.fin.sel <- as.formula(res[1, formula][[1]]) %>%
  gam(data = dt_bigf.fix, family = gaussian(), method = "ML")

m.lam.min <- gam(openness ~ 1, data = dt_bigf.fix, family = gaussian(), method = "ML")

# Sensitivity analysis -----------------------------------------------------------------------------

## Final model as binomial model ------------------------------------------------------------------

m.gam.fin <- sub("openness", "cbind(hits, misses)", res[1, formula][[1]]) %>%
  as.formula() %>%
  gam(data = dt_bigf, family = binomial(), method = "REML")

# mean(residuals(m.lam.fin)^2)
# mean(residuals(m.gam.fin, type = "response")^2)

### Quasi ------------------------------------------------------------------------------------------

# maybe REML is already quasi
m.gam.fin.quasi <- sub("openness", "cbind(hits, misses)", res[1, formula][[1]]) %>%
  as.formula() %>%
  gam(data = dt_bigf, family = quasibinomial(), method = "REML")

# mean(residuals(m.gam.fin, type = "response")^2)
# mean(residuals(m.gam.fin.quasi, type = "response")^2)

## Optimizer ---------------------------------------------------------------------------------------

m.lam.fin.efs <- as.formula(res[1, formula][[1]]) %>%
  gam(data = dt_bigf, family = gaussian(), method = "REML", optimizer = "efs")

