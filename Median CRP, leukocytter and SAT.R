crp_col  <- "CRP"
leuk_col <- "Leukocyttes"
sat_col  <- "Saturation"

median_iqr <- function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  x <- as.numeric(x)
  med <- median(x, na.rm = TRUE)
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  paste0(round(med,1), " (", round(q1,1), "–", round(q3,1), ")")
}

resultat <- data.frame(
  Variabel = c("CRP", "Leukocytes", "Saturation"),
  Median_IQR = c(
    median_iqr(Datafile[[crp_col]]),
    median_iqr(Datafile[[leuk_col]]),
    median_iqr(Datafile[[sat_col]])
  )
)

print(resultat)
