#code for influenza A
#Data for code is the document 'Datafile'
set.seed(42)
library(tidyverse)
library(patchwork)
library(rstatix)
#---------------------------------------------------------------------------------------------
p1_inf <- ggplot(data=Datafile, aes(x = Culture, y = CT_value)) +
  geom_jitter(width = 0.05, size = 1.8, color = "grey20", alpha=0.7) +
  geom_boxplot(fill='grey90', color='black', alpha = 0.5, width = 0.4, outlier.shape = NA) +
  labs(
    title = "A",
    x = "Culture",
    y = "Ct value from POCT*"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
#----------------------------------------------------------------------------------------------
p2_inf <- ggplot(data=Datafile, aes(x = Culture, y = Temperature)) +
  geom_jitter(width = 0.05, size = 1.8, color = "grey20", alpha=0.7) +
  geom_boxplot(fill='grey90', color='black', alpha = 0.5, width = 0.4, outlier.shape = NA) +
  labs(
    title = "B",
    x = "Culture",
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#---------------------------------------------------------------------------------------------
p3_inf <- ggplot(data=Datafile, aes(x = Culture, y = Saturation)) +
  geom_jitter(width = 0.05, size = 1.8, color = "grey20", alpha=0.7) +
  geom_boxplot(fill='grey90', color='black', alpha = 0.5, width = 0.4, outlier.shape = NA) +
  labs(
    title = "C",
    x = "Culture",
    y = "Saturation (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
#-----------------------------------------------------------------------------------------------
p4_inf <- ggplot(data=Datafile, aes(x = Culture, y = Oxygen)) +
  geom_jitter(width = 0.05, size = 1.8, color = "grey20", alpha=0.7) +
  geom_boxplot(fill='grey90', color='black', alpha = 0.5, width = 0.4, outlier.shape = NA) +
  labs(
    title = "D",
    x = "Culture",
    y = "Oxygen flow rate (L/min)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
#-----------------------------------------------------------------------------------------------
p5_inf <- ggplot(data=Datafile, aes(x = Culture, y = Leukocyttes)) +
  geom_jitter(width = 0.05, size = 1.8, color = "grey20", alpha=0.7) +
  geom_boxplot(fill='grey90', color='black', alpha = 0.5, width = 0.4, outlier.shape = NA) +
  labs(
    title = "E",
    x = "Culture",
    y = "Leukocytes (mia/L)  "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
#----------------------------------------------------------------------------------------------
p6_inf <- ggplot(data=Datafile, aes(x = Culture, y = CRP)) +
  geom_jitter(width = 0.05, size = 1.8, color = "grey20", alpha=0.7) +
  geom_boxplot(fill='grey90', color='black', alpha = 0.5, width = 0.4, outlier.shape = NA) +
  labs(
    title = "F",
    x = "Culture",
    y = "C-reactive protein (mg/L)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

#----------------------------------------------------------------------------------------------

#combine grafs
p1_inf+p2_inf+p3_inf+p4_inf+p5_inf+p6_inf
#-----------------------------------------------------------------------------------------



vars <- c("CT_value", "Temperature", "Oxygen", "Saturation", "Leukocyttes", "CRP")

results <- lapply(vars, function(v) {
  Datafile %>%
    select(Culture, all_of(v)) %>%
    drop_na() %>%
    wilcox_test(reformulate("Culture", v)) %>%
    add_significance() %>%
    mutate(variable = v) %>%
    select(variable, statistic, p, p.signif)
}) %>%
  bind_rows()

results




