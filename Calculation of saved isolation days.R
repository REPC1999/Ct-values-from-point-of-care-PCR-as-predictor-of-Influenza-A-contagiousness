library(boot)

# Number of saved isolation days
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

# estimate 
boot_res$t0

# 95% CI
boot.ci(boot_res, type = "perc")
#----------------------------------------------------------------------------------------
# number of saved isolation days for Culture
saved_prop_fun_neg <- function(data, indices) {
  d <- data[indices, ]
  
  # number of saved isolation days
  sum(d$`Dage i isolation`[d$CT_value > 31], na.rm = TRUE) /
    sum(d$`Dage i isolation`, na.rm = TRUE)
}

set.seed(42)

boot_res_prop_neg <- boot(
  data = Datafile,
  statistic = saved_prop_fun_neg,
  R = 10000
)

# estimated proportion of saved isolation days
boot_res_prop_neg$t0

# 95% CI
boot.ci(boot_res_prop_neg, type = "perc")

#----------------------------------------------------------------------------------------
# number of saved isolation days for Culture == "Negative"
saved_prop_fun_neg <- function(data, indices) {
  d <- data[indices, ]
  
  # filter only Negative
  d <- d[d$Culture == "Negative", ]
  
  # number of saved isolation days
  sum(d$`Dage i isolation`[d$CT_value > 31], na.rm = TRUE) /
    sum(d$`Dage i isolation`, na.rm = TRUE)
}

set.seed(42)

boot_res_prop_neg <- boot(
  data = Datafile,
  statistic = saved_prop_fun_neg,
  R = 10000
)

# estimated proportion of saved isolation days
boot_res_prop_neg$t0

# 95% CI
boot.ci(boot_res_prop_neg, type = "perc")



