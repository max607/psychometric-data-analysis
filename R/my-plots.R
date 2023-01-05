library(ggplot2)
library(data.table)
library(magrittr)

ggcross <- function(m1, m2, trans = identity, intercept = FALSE) {

  # extract all stuff
  coef1 <- m1$coefficients
  if (!intercept) coef1 <- coef1[names(coef1) != "(Intercept)"]
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
    geom_vline(xintercept = trans(0)) +
    geom_hline(yintercept = trans(0)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_line(aes(x = m1, y = m2, group = dot), data = rbind(dots_sd1, dots_sd2)) +
    geom_point(aes(x = coef1, y = coef2, color = name), data = dots_coef, size = 2)
}

