library("readr")
library("dplyr")
brca=read_tsv("~/Downloads/assignment_1/brca_tcga_clinical_data.tsv")
dim(brca)
colnames(brca)
table(brca$`Diagnosis Age`)
brca_primary=brca %>% filter(`Sample Type` == "Primary")
brca_primary_age_60=brca %>% filter(`Sample Type` == "Primary",`Diagnosis Age` >= 60)
dim(brca_primary)
dim(brca_primary_age_60)
brca$`Neoplasm Disease Stage American Joint Committee on Cancer Code`
table(brca$`Diagnosis Age` >=60)

brca %>% filter(`Sample Type` == "Primary") %>% 
  filter(!is.na(`Neoplasm Disease Stage American Joint Committee on Cancer Code`))  %>% 
  count(`Neoplasm Disease Stage American Joint Committee on Cancer Code`) %>% 
  mutate(prop=n/sum(n)) %>% arrange(desc(prop))


brca %>% filter(`Sample Type` == "Primary") %>% 
  filter(!is.na(`Neoplasm Disease Stage American Joint Committee on Cancer Code`)) %>% #dim()
  count(`Neoplasm Disease Stage American Joint Committee on Cancer Code`) %>% 
  mutate(prop=n/sum(n)) %>% 
  arrange(desc(prop))
brca$`IHC-HER2`
brca %>% filter(`Sample Type` == "Primary",!is.na(`ER Status By IHC`)) %>% count(`ER Status By IHC`)
brca %>% filter(`Sample Type` == "Primary",!is.na(`PR status by ihc`)) %>% count(`PR status by ihc`)
brca %>% filter(`Sample Type` == "Primary",!is.na(`IHC-HER2`)) %>% count(`IHC-HER2`)
brca_prim_na_rem=brca %>% filter(`Sample Type` == "Primary",!is.na(`ER Status By IHC`)) 
table(brca_prim_na_rem$`IHC-HER2`)

###
table(brca$`ER Status By IHC`)
  tnbc=brca %>% 
  filter(`Sample Type` == "Primary") %>% 
  filter(`ER Status By IHC` == "Negative" & `PR status by ihc` == "Negative" & `IHC-HER2` == "Negative") %>% 
  count()  
  primary_count=brca %>% 
    filter(`Sample Type` == "Primary") %>% 
    count()

  perc=   (tnbc/primary_count) *100
  perc
  