library(magrittr)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(forcats)
library(latex2exp)


df <- read_dta("../data/complete_data_DS.dta") %>% select(-c(starts_with("ProfessionalID"))) # all who submitted at least one task, including DS

# Calibration correct answers
correct_answers <- c(100, 0, 0, 100, 100, 0, 50)
df_questions <- df %>% select(starts_with("GenAICalPre")) %>% 
  mutate(GenAICalPre1_cor = ifelse(GenAICalPre1_5 > 50,1,0),
         GenAICalPre2_cor = ifelse(GenAICalPre2_5 < 50,1,0),
         GenAICalPre3_cor = ifelse(GenAICalPre3_5 > 50,1,0),
         GenAICalPre4_cor = ifelse(GenAICalPre4_5 < 50,1,0),
         GenAICalPre5_cor = ifelse(GenAICalPre5_5 < 50,1,0),
         GenAICalPre6_cor = ifelse(GenAICalPre6_5 < 50,1,0),
         GenAICalPre7_cor = ifelse(GenAICalPre7_5 < 50,1,0)
  ) %>% 
  rowwise() %>% 
  mutate(GenAICalPre_all = sum(GenAICalPre1_cor, GenAICalPre2_cor, GenAICalPre3_cor, GenAICalPre4_cor, GenAICalPre5_cor, GenAICalPre6_cor,GenAICalPre7_cor, na.rm = TRUE))

  df_questions <- df_questions %>%
  mutate(GenAICalPre1_over = ifelse(GenAICalPre1_5 > 50,1,0),
         GenAICalPre2_over = ifelse(GenAICalPre2_5 > 50,1,0),
         GenAICalPre3_over = ifelse(GenAICalPre3_5 > 50,1,0),
         GenAICalPre4_over = ifelse(GenAICalPre4_5 > 50,1,0),
         GenAICalPre5_over = ifelse(GenAICalPre5_5 > 50,1,0),
         GenAICalPre6_over = ifelse(GenAICalPre6_5 > 50,1,0),
         GenAICalPre7_over = ifelse(GenAICalPre7_5 > 50,1,0)
  ) %>% 
  rowwise() %>% 
  mutate(GenAICalPre_all_over = sum(GenAICalPre2_over,GenAICalPre4_over,GenAICalPre5_over,GenAICalPre6_over, GenAICalPre7_over, na.rm = TRUE)) %>%
  select(GenAICalPre_all, GenAICalPre_all_over)

df_questions %<>% 
  mutate(GenAICalPreOver = ifelse(GenAICalPre_all_over >= 4, 1,0),
    high_score = ifelse(GenAICalPre_all >= 4,1,0)) # Vast majority of people get 3/7
           
                     
df <- df %>% cbind(df_questions) %>%
  mutate(treatment = ifelse(Group == "Test",1, ifelse(Group == "Control",0,NA))) %>% 
  mutate(female = ifelse(Gender ==1,1,0)) %>%
  mutate(europe = ifelse(OfficeLocation ==4,1,0)) %>%
  mutate(native.english = ifelse(EnglishProficiency ==4,1,0)) %>%
  mutate(low.tenure = ifelse(TenureYears ==1,1,0),
         code_never = ifelse(FrequencyCodeWork == 1,1,0),
         chatgptdaily = ifelse(ChatGPT4Work == 5,1,0))%>%
  mutate(know_code = ifelse(CodingPre == 3, 2, CodingPre), 
         know_code = ifelse(know_code == -1, "No coding experience", 
                            ifelse(know_code == 1, "Coding basics", 
                                   ifelse(know_code ==2, "Competent coder", NA)))) %>%
  mutate(minutesStats = TimeTakenStats/60,
         minutesPS = TimeTakenPS/60,
         minutesCoding = TimeTakenCoding/60) %>%
  filter(treatment_arm != "" | !is.na(treatment_arm)) 

df$know_code <- fct_relevel(df$know_code, "Competent coder", "Coding basics", "No coding experience")

write.csv(df, "../computed_objects/experimental_data.csv")
