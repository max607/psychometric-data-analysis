# Eloquence score ----------------------------------------------------------------------------------

p1 <- ggplot(dt_bigf, aes(x = eloquence)) +
  geom_bar(color = "#606161", fill = "#606161") +
  scale_x_continuous(breaks = seq(0, 16, 4)) +
  labs(x = "Eloquence score", y = "Number of observations")

# Openness score -----------------------------------------------------------------------------------

p2 <- ggplot(dt_bigf, aes(x = openness)) +
  geom_histogram(aes(y = ..density..), breaks = seq(0, 1, length = 100), color = "white",
                 fill = "#606161") +
  geom_function(fun = dnorm, args = dnorm_args(dt_bigf$openness), color = "#0075be", size = 1) +
  geom_density(size = 1) +
  labs(x = "Openness score", y = "Density")

# Frequencies --------------------------------------------------------------------------------------

tmpp3 <- ggplot(dt_bigf, aes(age)) +
  geom_histogram(color = "white", fill = "#606161", bins = 50) +
  # geom_vline(xintercept = mean(dt_bigf$age, na.rm = TRUE)) +
  labs(x = paste0("Age, NA: ", sum(is.na(dt_bigf$age))), y = "Count") +
  xlim(0, 80)

tmpp4 <- ggplot(dt_bigf, aes(familysize)) +
  geom_bar(color = "white", fill = "#606161") +
  # geom_vline(xintercept = mean(dt_bigf$familysize, na.rm = TRUE)) +
  labs(x = paste0("Family size, NA: ", sum(is.na(dt_bigf$familysize))), y = "Count") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(1, 20, 6))

# Continuous pair-wise associations ----------------------------------------------------------------

p3 <- ggplot(dt_bigf, aes(x = eloquence, y = openness)) +
  geom_jitter(height = 0, width = 0.3, alpha = 0.2) +
  geom_smooth(method = gam, formula = y ~ s(x, bs = 'cr', k = 10), color = "#0075be", size = 1) +
  labs(x = "Eloquence score", y = "Openness score") +
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, 4))

p4 <- ggplot(dt_bigf, aes(x = age, y = openness)) +
  geom_jitter(height = 0, width = 0.1, alpha = 0.2) +
  geom_smooth(method = gam, formula = y ~ s(x, bs = 'cr', k = 10), color = "#0075be", size = 1) +
  labs(x = "Age", y = "Openness score") +
  xlim(0, 80)

p5 <- ggplot(dt_bigf[familysize < 20,], aes(x = familysize, y = openness)) +
  geom_jitter(height = 0, width = 0.3, alpha = 0.2) +
  geom_smooth(method = gam, formula = y ~ s(x, bs = 'cr', k = 10), color = "#0075be", size = 1) +
  labs(x = "Family size", y = "Openness score") +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, 6))

# A posteriori inclusion probabilities -------------------------------------------------------------

p6 <- res[, sapply(vars_full, function(v) sum(probs[sapply(vars, `%in%`, x = v)]))] %>%
  data.table(var = names(.), prob = .) %>%
  setorder(-var) %>%
  .[, var := factor(var, levels = var)] %>%
  ggplot(aes(var, prob)) +
  geom_col() +
  geom_hline(yintercept = 0.5) +
  xlab("Covariate") + ylab("A posteriori inclusion probability") +
  coord_flip()

# Model diagnostics --------------------------------------------------------------------------------

# data.table(resid_student = residuals(m.lam.fin, "scaled.pearson")) %>%
#   ggplot(aes(resid_student)) +
#   geom_histogram(aes(y = ..density..), color = "black", bins = 100) +
#   geom_function(fun =  dnorm) +
#   xlim(-6, 6)

p9.1 <- data.table(fitted = fitted(m.lam.fin), resid = residuals(m.lam.fin, "response")) %>%
  ggplot(aes(fitted, resid)) +
  geom_point()

# Coefficients -------------------------------------------------------------------------------------

## Classic effects ---------------------------------------------------------------------------------

p7.1 <- ggcoef(m.lam.fin, id = c(1, 17:29))
p7.2 <- ggcoef(m.lam.fin, id = 2:16)

## Smooth effects ----------------------------------------------------------------------------------

p10.1 <- predict(m.lam.fin, type = "terms", terms = "s(age)", se.fit = TRUE) %$%
  data.table(age = m.lam.fin$model$age, s_age = fit[, 1], sd = se.fit[, 1]) %>%
  .[, c("lower", "upper") := .(s_age - 2 * sd, s_age + 2 * sd)] %>%
  ggplot(aes(age, s_age)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey") +
  geom_hline(yintercept = 0) +
  geom_line(size = 1, color = "#0075be") +
  labs(x = paste0("Age, edf: ", round(summary(m.lam.fin)$edf[[1]], 3)), y = "s(Age)") +
  xlim(0, 80) +
  scale_y_continuous(limits = c(-0.15, 0.15), breaks = round(seq(-0.15, 0.15, 0.05), 2))

p10.2 <- predict(m.lam.fin, type = "terms", terms = "s(eloquence)", se.fit = TRUE) %$%
  data.table(elo = m.lam.fin$model$eloquence, s_elo = fit[, 1], sd = se.fit[, 1]) %>%
  .[, c("lower", "upper") := .(s_elo - 2 * sd, s_elo + 2 * sd)] %>%
  ggplot(aes(elo, s_elo)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey") +
  geom_hline(yintercept = 0) +
  geom_line(size = 1, color = "#0075be") +
  labs(x = paste0("Eloquence score, edf: ", round(summary(m.lam.fin)$edf[[2]], 3)),
       y = "s(Eloquence score)") +
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, 4)) +
  scale_y_continuous(limits = c(-0.15, 0.15), breaks = round(seq(-0.15, 0.15, 0.05), 2))

# Visual Sensitivity analysis ----------------------------------------------------------------------

p8 <- ggcross(m.lam.fin.sel, m.lam.fin) +
  theme(legend.position = "none") +
  lims(x = c(-0.1, 0.1), y = c(-0.1, 0.1)) +
  labs(x = "Selected model", y = "Refitted model")

# ggcross_sd(m.lam.fin.sel, m.lam.fin) +
#   theme(legend.position = "none") +
#   lims(x = c(0, 0.04), y = c(0, 0.04)) +
#   labs(x = "Selected model", y = "Refitted model")


# data.table(lam = residuals(m.lam.fin), gam = residuals(m.gam.fin, type = "response")) %>%
#   ggplot(aes(x = lam, y = gam)) +
#   geom_point(alpha = 0.3) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_smooth()

# data.table(lam = predict(m.lam.fin), gam = predict(m.gam.fin, type = "response")) %>%
#   ggplot(aes(x = lam, y = gam)) +
#   geom_jitter(height = 0.001, width = 0.001, alpha = 0.3) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_smooth()

# pred_lam <- predict(m.lam.fin)
# rbind(
#   data.table(pred = pred_lam, model = "lam",
#              truth = dt_bigf[as.integer(names(pred_lam)), openness]),
#   data.table(pred = predict(m.gam.fin, type = "response"), model = "gam",
#              truth = dt_bigf[as.integer(names(pred_lam)), openness])
# ) %>%
#   ggplot(aes(x = truth, y = pred, color = model)) +
#   geom_point(alpha = 0.3) +
#   geom_abline(intercept = 0, slope = 1) +
#   geom_smooth(se = FALSE) +
#   ylim(0, 1)

# ggcross(m.gam.fin, m.gam.fin.quasi)  # just splines differ
# ggcross(m.lam.fin, m.lam.fin.efs)

