library(tidyverse)
library(pROC)
library(scales)
library(grid)      
library(cowplot)
library(patchwork)


# 1) ROC-analyse

# Settings
use_auto_direction <- TRUE           
direction_override <- ">"            
cutoffs <- c(27, 28, 29, 30, 31,32)

# Making data ready
df <- Datafile %>%
  filter(!is.na(Culture), !is.na(CT_value)) %>%
  mutate(
    Culture = factor(tolower(trimws(Culture)),
                     levels = c("negative", "positive")),
    CT_value = as.numeric(CT_value)
  )

# Direction 
direction <- ">"

# ROC + AUC (DeLong CI)
roc_ct <- pROC::roc(response = df$Culture,
                    predictor = df$CT_value,
                    levels   = c("negative","positive"),
                    direction = direction,
                    quiet = TRUE)

auc_val <- as.numeric(pROC::auc(roc_ct))
auc_ci  <- as.numeric(pROC::ci.auc(roc_ct, method = "delong"))

auc_table <- data.frame(
  AUC = auc_val,
  CI_low = auc_ci[1],
  CI_high = auc_ci[3],
  Direction_used = direction
)

# Performance at cutoffs
cp_ci <- function(x, n) {
  if (is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  as.numeric(binom.test(x, n)$conf.int)
}


perf_table <- lapply(cutoffs, function(t) {
  pred_pos <- df$CT_value <= t
  
  TP <- sum(pred_pos & df$Culture == "positive")
  FN <- sum(!pred_pos & df$Culture == "positive")
  TN <- sum(!pred_pos & df$Culture == "negative")
  FP <- sum(pred_pos & df$Culture == "negative")
  
  se_den  <- TP + FN
  sp_den  <- TN + FP
  ppv_den <- TP + FP
  npv_den <- TN + FN
  n_tot   <- se_den + sp_den
  
  sens <- if (se_den  > 0) TP / se_den  else NA_real_
  spec <- if (sp_den  > 0) TN / sp_den  else NA_real_
  ppv  <- if (ppv_den > 0) TP / ppv_den else NA_real_
  npv  <- if (npv_den > 0) TN / npv_den else NA_real_
  acc  <- if (n_tot   > 0) (TP + TN) / n_tot else NA_real_
  
  se_ci  <- cp_ci(TP, se_den)
  sp_ci  <- cp_ci(TN, sp_den)
  ppv_ci <- cp_ci(TP, ppv_den)
  npv_ci <- cp_ci(TN, npv_den)
  acc_ci <- cp_ci(TP + TN, n_tot)
  
  data.frame(
    Cutoff = t,
    Sensitivity = sens,  Sens_low = se_ci[1],  Sens_high = se_ci[2],
    Specificity = spec,  Spec_low = sp_ci[1],  Spec_high = sp_ci[2],
    PPV = ppv,          PPV_low  = ppv_ci[1], PPV_high  = ppv_ci[2],
    NPV = npv,          NPV_low  = npv_ci[1], NPV_high  = npv_ci[2],
    Accuracy = acc,     Acc_low  = acc_ci[1], Acc_high  = acc_ci[2],
    TP = TP, FP = FP, TN = TN, FN = FN,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# Print results
auc_table
perf_table



# ROC-plot 
auc_val <- pROC::auc(roc_ct)

p_roc <- ggroc(roc_ct, legacy.axes = TRUE) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.02, 0.02))) +
  
  theme_classic(base_size = 13, base_family = "Arial") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "plain"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  labs(x = "1 - Specificity", y = "Sensitivity") +
  
  
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey60", linewidth = 0.4) +
  
  
  annotate("text", x = 1.00, y = 0.02,
           label = sprintf("AUC = %.3f", auc_val),
           hjust = 1, vjust = 0, size = 3.5)

# Show the figure
print(p_roc)


# 2) Sensitivity and specificity curve

# Metrics over cutoffs
ct_min <- floor(min(df$CT_value))
ct_max <- ceiling(max(df$CT_value))
thresholds <- seq(ct_min, ct_max, by = 0.1)

metrics <- map_dfr(thresholds, function(t) {
  pred_pos <- df$CT_value <= t  # lav Ct = positiv
  TP <- sum(pred_pos & df$Culture == "positive")
  FN <- sum(!pred_pos & df$Culture == "positive")
  TN <- sum(!pred_pos & df$Culture == "negative")
  FP <- sum(pred_pos & df$Culture == "negative")
  tibble(
    Ct = t,
    Sensitivity = if ((TP + FN) > 0) TP / (TP + FN) else NA_real_,
    Specificity = if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
  )
})

plot_df <- metrics |>
  pivot_longer(c(Sensitivity, Specificity),
               names_to = "Measure", values_to = "Percent")

# Making the plot

plot_df <- metrics |>
  tidyr::pivot_longer(c(Sensitivity, Specificity),
                      names_to = "Measure", values_to = "Percent")

p <- ggplot(plot_df, aes(x = Ct, y = Percent, linetype = Measure)) +
  geom_line(linewidth = 0.6, color = "black", na.rm = TRUE) + 
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  scale_x_continuous(
    breaks = pretty(c(ct_min, ct_max), n = 6),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_linetype_manual(
    values = c("Sensitivity" = "solid", "Specificity" = "dashed"),
    labels = c("Sensitivity", "Specificity"),
    guide = guide_legend(title = NULL)
  ) +
  labs(x = "Cut-off (Ct value)", y = "") +
  theme_classic(base_size = 13, base_family = "Arial") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.text = element_text(size = 12),
    axis.ticks.length = unit(3, "pt")
  )

print(p)


print(p_roc+p)
