
library(readr)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", repos = "https://cloud.r-project.org")

setwd("/Users/pranjalpruthi/Documents/GitHub/R-Programming-101/R2/")
getwd() 


#Answer 1:

patient_data <- read_tsv("/Users/pranjalpruthi/Documents/GitHub/R-Programming-101/R2/data_clinical_patient.txt")
exp_data <- read_tsv("/Users/pranjalpruthi/Documents/GitHub/R-Programming-101/R2/sample_data_mrna_seq_rpkm.txt")



dim(patient_data)
dim(exp_data)
spec(patient_data)
spec(exp_data)


head(patient_data, 10)
head(exp_data, 10)


colnames(exp_data)[1:6]

#Answer 2

patient_data_clean <- patient_data %>%  mutate(
  RACE = str_trim(str_to_lower(RACE)),
  ETHNICITY = str_trim(str_to_lower(ETHNICITY)))


patient_data %>% count(RACE)
patient_data %>% count(ETHNICITY)

patient_data_clean %>% count(RACE)
patient_data_clean %>% count(ETHNICITY)


patient_data_clean <- patient_data %>%  mutate(AGE_num = as.numeric(AGE))

dim(patient_data_clean)

 
patient_data_cleaner <- patient_data_clean %>%
  mutate(age_group = case_when(
    is.na(AGE_num) ~ "unknown",
    AGE_num < 18 ~ "<18",
    AGE_num >= 18 & AGE_num <= 39 ~ "18-39",
    AGE_num >= 40 & AGE_num <= 64 ~ "40-64",
    AGE_num >= 65 ~ "65+",
    .default = "unknown"
  ))          

patient_data_cleaner %>% count(age_group)

patient_data_cleaner %>%  count(DISEASE_ONTOLOGY, sort = TRUE)

patient_data_cleaner <- patient_data_cleaner %>%  replace_na(list(DISEASE_ONTOLOGY = "unknown"))


patient_data_cleaner %>%  count(DISEASE_ONTOLOGY, sort = TRUE)
    

#Answer 3 

#Create a subset of all lung-related cases
lung_cases <- patient_data_cleaner %>%
  filter(str_detect(DISEASE_ONTOLOGY, regex("lung", ignore_case = TRUE)))


nrow(lung_cases)

lung_cases %>% count(DISEASE_ONTOLOGY)




patient_data_cleaner %>%  filter(str_detect(DISEASE_ONTOLOGY, "lung|pulmonary")) %>%
  distinct(DISEASE_ONTOLOGY)

dim(lung_cases)

lung_cases %>% count(SEX)
#•	Find the average age (ignoring NA) by DISEASE_ONTOLOGY.

age_by_disease <- patient_data_cleaner %>%
  group_by(DISEASE_ONTOLOGY) %>%
  summarise(
    avg_age = mean(AGE_num, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(count))

# View the results
print(age_by_disease, n = 20)

# Or save and view
View(age_by_disease)


#Answer 4
#	From the clinical file, select two columns that identify the sample (PATIENT_ID, DISEASE_ONTOLOGY).

patient_metadata <- patient_data_cleaner %>%  select(PATIENT_ID, DISEASE_ONTOLOGY)

View(patient_metadata)
dim(patient_metadata)

#	Perform a left_join() to add disease info to each expression row by matching the sample name to PATIENT_ID.


dim(exp_data)
colnames(exp_data)[1:10]

exp_data_long <- exp_data %>%
  pivot_longer(
    cols = -Hugo_Symbol,           
    names_to = "PATIENT_ID",       
    values_to = "expression_value" 
  )


head(exp_data_long, 10)
dim(exp_data_long) 


exp_with_disease <- exp_data_long %>%
  left_join(patient_metadata, by = "PATIENT_ID")

matched_rows <- sum(!is.na(exp_with_disease$DISEASE_ONTOLOGY))
total_rows <- nrow(exp_with_disease)


cat("Total rows after join:", total_rows, "\n")
cat("Rows with successful match:", matched_rows, "\n")
cat("Percentage matched:", round(matched_rows/total_rows * 100, 2), "%\n")


#answer 5

ggplot(patient_data_cleaner, aes(x = DISEASE_ONTOLOGY)) +
  geom_bar() +
  labs(
    title = "Number of Patients by Disease Ontology",
    x = "Disease Ontology",
    y = "Number of Patients"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability


library(dplyr)

patient_data_filtered <- patient_data_cleaner %>%
  filter(DISEASE_ONTOLOGY != "unknown")

ggplot(patient_data_filtered, aes(x = DISEASE_ONTOLOGY)) +
  geom_bar() +
  labs(
    title = "Number of Patients by Disease Ontology",
    x = "Disease Ontology",
    y = "Number of Patients"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

