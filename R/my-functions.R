dnorm_args <- function(x) list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))

used_obs <- function(model, n) seq_len(n) %in% as.integer(names(fitted(model)))
