library(boot)

# funktion der beregner sparede dage
saved_days_fun <- function(data, indices) {
  d <- data[indices, ]
  sum(d$'Dage i isolation'[d$CT_value > 31], na.rm = TRUE)
}

set.seed(42)

boot_res <- boot(
  data = Datafile,
  statistic = saved_days_fun,
  R = 10000
)

# estimat
boot_res$t0

# 95% CI
boot.ci(boot_res, type = "perc")
#----------------------------------------------------------------------------------------
library(boot)

# funktion der beregner andelen af sparede dage for Culture == "Negative"
saved_prop_fun_neg <- function(data, indices) {
  d <- data[indices, ]
  
  # filtrer til kun Negative
  d <- d[d$Culture == "Negative", ]
  
  # andel sparede dage
  sum(d$`Dage i isolation`[d$CT_value > 31], na.rm = TRUE) /
    sum(d$`Dage i isolation`, na.rm = TRUE)
}

set.seed(42)

boot_res_prop_neg <- boot(
  data = Datafile,
  statistic = saved_prop_fun_neg,
  R = 10000
)

# estimeret andel af sparede dage
boot_res_prop_neg$t0

# 95% CI
boot.ci(boot_res_prop_neg, type = "perc")



