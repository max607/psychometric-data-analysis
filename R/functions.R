# Misc ---------------------------------------------------------------------------------------------

dnorm_args <- function(x) {
  list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
}

used_obs <- function(model, n) {
  seq_len(n) %in% as.integer(names(fitted(model)))
}

# Formulas -----------------------------------------------------------------------------------------

make_formula <- function(vars, response, id_splines = integer(0)) {
  if (length(id_splines) > 0) {
    vars[id_splines] <- paste0("s(", vars[id_splines], ", bs = 'cr', k = 10)")
  }

  paste(vars, collapse = " + ") %>% paste(response, "~", .)
}

# Model space - complete search --------------------------------------------------------------------

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
    AIC(gam(as.formula(form), data = dt_fix, family = gaussian(), method = "ML"))
  }) %>% unlist()

  # return
  message("Dropped ", nrow(dt) - nrow(dt_fix), " observations.")
  data.table(AIC = vec_aic, formula = all_formulas, vars = all_combinations) %>%
    setkey("AIC")
}

# Plots --------------------------------------------------------------------------------------------

## Comparing model coefficients --------------------------------------------------------------------

ggcross <- function(m1, m2, trans = identity, intercept = FALSE, splines = FALSE) {

  # extract all stuff
  coef1 <- m1$coefficients
  if (!intercept) coef1 <- coef1[names(coef1) != "(Intercept)"]
  if (!splines) coef1 <- coef1[!grepl("^s\\(", names(coef1))]
  coef2 <- m2 %$% coefficients[names(coefficients) %in% names(coef1)]
  sd1 <- sqrt(diag(vcov(m1)))[names(coef2)]
  sd2 <- sqrt(diag(vcov(m2)))[names(coef2)]
  coef1 <- coef1[names(coef2)]

  # prepare data for ggplot
  dots_coef <- data.table(coef1 = trans(coef1), coef2 = trans(coef2), name = names(coef1))
  dots_sd1 <- data.table(m1 = trans(c(coef1 - 2 * sd1, coef1 + 2 * sd1)),
                         m2 = trans(rep(coef2, 2)),
                         dot = factor(rep(seq_along(coef1), 2)))
  dots_sd2 <- data.table(m1 = trans(rep(coef1, 2)),
                         m2 = trans(c(coef2 - 2 * sd2, coef2 + 2 * sd2)),
                         dot = factor(rep(seq_along(coef2) + length(coef2), 2)))

  # plot
  ggplot() +
    geom_vline(xintercept = trans(0), color = "grey", size = 1.3) +
    geom_hline(yintercept = trans(0), color = "grey", size = 1.3) +
    geom_abline(intercept = 0, slope = 1, color = "grey", size = 1.3) +
    geom_line(aes(x = m1, y = m2, group = dot), data = rbind(dots_sd1, dots_sd2)) +
    geom_line(aes(x = coef1, y = coef2), data = dots_coef, color = "#0075be", size = 1.3)
}

ggcross_sd <- function(m1, m2, trans = identity, intercept = FALSE, splines = FALSE) {

  # extract all stuff
  coef1 <- m1$coefficients
  if (!intercept) coef1 <- coef1[names(coef1) != "(Intercept)"]
  if (!splines) coef1 <- coef1[!grepl("^s\\(", names(coef1))]
  coef2 <- m2 %$% coefficients[names(coefficients) %in% names(coef1)]
  sd1 <- sqrt(diag(vcov(m1)))[names(coef2)]
  sd2 <- sqrt(diag(vcov(m2)))[names(coef2)]

  # plot
  data.table(coef1 = trans(sd1), coef2 = trans(sd2), name = names(coef1)) %>%
    ggplot() +
    geom_vline(xintercept = trans(0), color = "grey", size = 1.3) +
    geom_hline(yintercept = trans(0), color = "grey", size = 1.3) +
    geom_abline(intercept = 0, slope = 1, color = "grey", size = 1.3) +
    geom_point(aes(x = sd1, y = sd2), size = 1.3)
}

## Absolute frequencies ----------------------------------------------------------------------------

ggtable <- function(dt, cols) {
  dt[, .SD, .SDcols = cols] %>%
    lapply(table, useNA = "always") %>%
    lapply(names(.), function(name, ls) {
      tab <- ls[[name]]
      data.table(var = name, level = names(tab), n = as.numeric(tab))
    }, ls = .) %>%
    do.call(what = rbind) %>%
    ggplot(aes(factor(level, levels = unique(level)), n)) +
    geom_col(position = "dodge") +
    labs(x = "Level of Variable", y = "Count") +
    coord_flip() +
    facet_wrap(vars(var), scales = "free_y", drop = TRUE, nrow = 2)
}

## Model coefficients ------------------------------------------------------------------------------

ggcoef <- function(m1, id = NULL, intercept = FALSE, splines = FALSE) {
  coef1 <- m1$coefficients
  if (!intercept) coef1 <- coef1[names(coef1) != "(Intercept)"]
  if (!splines) coef1 <- coef1[!grepl("^s\\(", names(coef1))]
  if (is.null(id)) id <- seq_along(coef1)
  sd1 <- sqrt(diag(vcov(m1)))[names(coef1)]

  dt_coef <- data.table(coef = names(coef1), value = coef1, sd = sd1) %>%
    .[, c("lower", "upper") := .(value - 2 * sd, value + 2 * sd)] %>%
    .[, sig := !(lower < 0 & 0 < upper)] %>%
    setorder(-coef) %>%
    .[, coef := factor(coef, levels = coef)]

  ggplot(dt_coef[id,], aes(x = coef, y = value, color = factor(sig))) +
    geom_hline(yintercept = 0) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    geom_point() +
    ylab("Estimate") + xlab("Covariate") + ylim(-0.1, 0.1) +
    coord_flip() +
    scale_color_manual(values = c("#a7aaac", "black")) +
    theme(legend.position = "none",
          axis.text.y = element_text(face = ifelse(dt_coef[id, sig], "bold", "plain")))
}

