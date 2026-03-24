#Test for normal destribution
library(tidyverse)
#---------------------------------------------------------------------------------
#Descriptiv statistics
Datafile %>%
  group_by(Culture) %>%
  summarise(
    median_Age = median(Age, na.rm = TRUE),
    Q1 = quantile(Age, 0.25, na.rm = TRUE),
    Q3 = quantile(Age, 0.75, na.rm = TRUE))

Datafile %>%
  summarise(
    median_Age = median(Age, na.rm = TRUE),
    Q1 = quantile(Age, 0.25, na.rm = TRUE),
    Q3 = quantile(Age, 0.75, na.rm = TRUE))

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture) %>%
  summarise(
    median_isolation_time = median(`Dage i isolation`, na.rm = TRUE),
    Q1 = quantile(`Dage i isolation`, 0.25, na.rm = TRUE),
    Q3 = quantile(`Dage i isolation`, 0.75, na.rm = TRUE))

Datafile %>%
  summarise(
    median_isolation_time = median(`Dage i isolation`, na.rm = TRUE),
    Q1 = quantile(`Dage i isolation`, 0.25, na.rm = TRUE),
    Q3 = quantile(`Dage i isolation`, 0.75, na.rm = TRUE))
#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture) %>%
  summarise(
    median_BMI = median(BMI, na.rm = TRUE),
    Q1 = quantile(BMI, 0.25, na.rm = TRUE),
    Q3 = quantile(BMI, 0.75, na.rm = TRUE))

Datafile %>%
  summarise(
    median_BMI = median(BMI, na.rm = TRUE),
    Q1 = quantile(BMI, 0.25, na.rm = TRUE),
    Q3 = quantile(BMI, 0.75, na.rm = TRUE))

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture) %>%
  summarise(
    median_Days_from_symptom_onset_to_first_sampling = median(Days_from_symptom_onset_to_first_sampling, na.rm = TRUE),
    Q1 = quantile(Days_from_symptom_onset_to_first_sampling, 0.25, na.rm = TRUE),
    Q3 = quantile(Days_from_symptom_onset_to_first_sampling, 0.75, na.rm = TRUE))

Datafile %>%
  summarise(
    median_Days_from_symptom_onset_to_first_sampling = median(Days_from_symptom_onset_to_first_sampling, na.rm = TRUE),
    Q1 = quantile(Days_from_symptom_onset_to_first_sampling, 0.25, na.rm = TRUE),
    Q3 = quantile(Days_from_symptom_onset_to_first_sampling, 0.75, na.rm = TRUE))
#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture) %>%
  summarise(
    median_Symptoms_duration = median(Symptoms_duration, na.rm = TRUE),
    Q1 = quantile(Symptoms_duration, 0.25, na.rm = TRUE),
    Q3 = quantile(Symptoms_duration, 0.75, na.rm = TRUE))

Datafile %>%
  summarise(
    median_Symptoms_duration = median(Symptoms_duration, na.rm = TRUE),
    Q1 = quantile(Symptoms_duration, 0.25, na.rm = TRUE),
    Q3 = quantile(Symptoms_duration, 0.75, na.rm = TRUE))
#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture) %>%
  summarise(
    median_Admission_time = median(Admission_time, na.rm = TRUE),
    Q1 = quantile(Admission_time, 0.25, na.rm = TRUE),
    Q3 = quantile(Admission_time, 0.75, na.rm = TRUE))

Datafile %>%
  summarise(
    median_Admission_time = median(Admission_time, na.rm = TRUE),
    Q1 = quantile(Admission_time, 0.25, na.rm = TRUE),
    Q3 = quantile(Admission_time, 0.75, na.rm = TRUE))
#---------------------------------------------------------------------------------

Datafile %>%
  group_by(Culture) %>%
  summarise(
    median_Time_since_last_infvac = median(Days_since_vaccination, na.rm = TRUE),
    Q1 = quantile(Days_since_vaccination, 0.25, na.rm = TRUE),
    Q3 = quantile(Days_since_vaccination, 0.75, na.rm = TRUE))

Datafile %>%
  summarise(
    median_Time_since_lastinfvac = median(Days_since_vaccination, na.rm = TRUE),
    Q1 = quantile(Days_since_vaccination, 0.25, na.rm = TRUE),
    Q3 = quantile(Days_since_vaccination, 0.75, na.rm = TRUE))

#---------------------------------------------------------------------------------

Datafile %>%
  group_by(Culture, Gender) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Gender) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Gender, n, prop)

#---------------------------------------------------------------------------------

Datafile %>%
  group_by(Culture, `Antiviral treatment`) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(`Antiviral treatment`) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, `Antiviral treatment`, n, prop)

#---------------------------------------------------------------------------------

Datafile %>%
  group_by(Culture, `Antibiotic treatment`) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(`Antibiotic treatment`) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, `Antibiotic treatment`, n, prop)

#---------------------------------------------------------------------------------

Datafile %>%
  group_by(Culture, Cough) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Cough) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Cough, n, prop)

#---------------------------------------------------------------------------------

Datafile %>%
  mutate(Fever = ifelse(Temperature >= 37.5, "≥37.5", "<37.5")) %>%
  group_by(Culture, Fever) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      mutate(Fever = ifelse(Temperature >= 37.5, "≥37.5", "<37.5")) %>%
      group_by(Fever) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Fever, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, Myalgia) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Myalgia) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Myalgia, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, Sore_throat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Sore_throat) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Sore_throat, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, Fatigue) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Fatigue) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Fatigue, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, Headache) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Headache) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Headache, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, Rhinorrhoea) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Rhinorrhoea) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Rhinorrhoea, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, Dyspnea) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Dyspnea) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Dyspnea, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, Mortality) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(Mortality) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, Mortality, n, prop)

#---------------------------------------------------------------------------------
Datafile %>%
  group_by(Culture, ICU) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  bind_rows(
    Datafile %>%
      group_by(ICU) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(Culture = "Total",
             prop = n / sum(n))
  ) %>%
  select(Culture, ICU, n, prop)
