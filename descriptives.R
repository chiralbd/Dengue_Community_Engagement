library(tidyverse)
library(tidyr)
library(stringr)
library(janitor)
library(gtsummary)
library(readxl)
library(flextable)

# read data 
data <- read_excel("data/Dengue_Community_No_Code.xlsx")

# Table 1: Demographic information
table1 <- data %>% 
  select(1:4) %>% 
  tbl_summary(missing = "no", 
              type = all_categorical() ~ "categorical") %>% 
  as_flex_table() 
table1 
save_as_docx(table1, path = "tables/Table1.docx")


# Table 2: Frequencies and percentage distribution of respondents regarding their perception to control
# dengue through community participation
table2 <- data %>% 
  select(6:13) %>% 
  tbl_summary(missing = "no", 
              type = all_categorical() ~ "categorical") %>% 
  as_flex_table() 
table2 

save_as_docx(table2, path = "tables/Table2.docx")

# Table 3: Frequencies and percentage distribution of respondents regarding their practice to control
# dengue through community participation
table3 <- data %>% 
  select(14:23) %>%
  tbl_summary(missing = "no", 
              type = all_categorical() ~ "categorical") %>% 
  as_flex_table() 
table3 

save_as_docx(table3, path = "tables/Table3.docx")


# Coding and categorizing 
recode_val <- function(x){
  ifelse(x=="Yes", 1,0)
}

data2 <- data %>% 
  select(14:23) %>% 
  mutate_all(recode_val) %>% 
  mutate(Total_Practice_Score = rowSums(across(where(is.numeric)))) %>% 
  mutate(Practice_Status = case_when(
    Total_Practice_Score <= median(Total_Practice_Score) ~ "Poor", # 1 = Poor  
    Total_Practice_Score > median(Total_Practice_Score) ~ "Good"  # 2 = Good 
  )) 

# Combine data 
df_combined <- cbind(data, data2)

# Table 4: Association between community participation and dengue control practices
table4 <- df_combined %>% 
  select(6:13, Practice_Status) %>% 
  tbl_summary(missing = "no", 
              by = Practice_Status) %>% 
  add_p() %>% 
  bold_p(t = 0.05) %>% 
  as_flex_table()
table4

save_as_docx(table4, path = "tables/Table4.docx")


