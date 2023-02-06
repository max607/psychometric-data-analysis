# Plots for Eloquence score ------------------------------------------------------------------------

p1 <- ggplot(dt_bigf, aes(x = eloquence)) +
  geom_bar(color = "#606161", fill = "#606161") +
  scale_x_continuous(breaks = seq(0, 16, 4)) +
  labs(x = "Eloquence score", y = "Number of observations")

# Plots for Openness score -------------------------------------------------------------------------

p2 <- ggplot(dt_bigf, aes(x = openness)) +
  geom_histogram(aes(y = ..density..), breaks = seq(0, 1, length = 200), color = "#606161",
                 fill = "#606161") +
  geom_function(fun = dnorm, args = dnorm_args(dt_bigf$openness), color = "#0075be", size = 1) +
  geom_density(size = 1) +
  labs(x = "Openness score", y = "Density")

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

# Visual Sensitivity analysis ----------------------------------------------------------------------

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

