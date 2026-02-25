library(haven)
library(dplyr)
library(tidyr)

df <- read_dta("../data/complete_data_all.dta")

# Calibration correctness summary used in downstream heterogeneity controls.
df_questions <- df %>%
  select(starts_with("GenAICalPre")) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(
    GenAICalPre1_cor = ifelse(GenAICalPre1_5 > 50, 1, 0),
    GenAICalPre2_cor = ifelse(GenAICalPre2_5 < 50, 1, 0),
    GenAICalPre3_cor = ifelse(GenAICalPre3_5 < 50, 1, 0),
    GenAICalPre4_cor = ifelse(GenAICalPre4_5 > 50, 1, 0),
    GenAICalPre5_cor = ifelse(GenAICalPre5_5 > 50, 1, 0),
    GenAICalPre6_cor = ifelse(GenAICalPre6_5 < 50, 1, 0),
    GenAICalPre7_cor = ifelse(GenAICalPre7_5 > 25 & GenAICalPre7_5 < 75, 1, 0),
    GenAICalPre_all = GenAICalPre1_cor + GenAICalPre2_cor + GenAICalPre3_cor +
      GenAICalPre4_cor + GenAICalPre5_cor + GenAICalPre6_cor + GenAICalPre7_cor
  ) %>%
  select(GenAICalPre_all)

df <- df %>%
  cbind(df_questions) %>%
  mutate(
    treatment = ifelse(Group == "Test", 1, ifelse(Group == "Control", 0, NA)),
    female = ifelse(Gender == 1, 1, 0),
    europe = ifelse(OfficeLocation == 4, 1, 0),
    native.english = ifelse(EnglishProficiency == 4, 1, 0),
    low.tenure = ifelse(TenureYears == 1, 1, 0),
    code_never = ifelse(FrequencyCodeWork == 1, 1, 0),
    chatgptdaily = ifelse(ChatGPT4Work == 5, 1, 0),
    prompt_familiarity = ifelse(PromptEngFamiliarity > 4, 1, 0),
    prof_coder = ifelse(CodingPre == 2, 1, 0),
    never_coded = ifelse(CodingPre == -1, 1, 0),
    coding_languages = ifelse(ProgrammingLanguagesNo > 2, 1, 0),
    know_code = ifelse(CodingPre == 3, 2, CodingPre),
    know_code = ifelse(
      know_code == -1, "No coding experience",
      ifelse(know_code == 1, "Coding basics", ifelse(know_code == 2, "Proficient coder", NA))
    ),
    minutesStats = TimeTakenStats / 60,
    minutesPS = TimeTakenPS / 60,
    minutesCoding = TimeTakenCoding / 60,
    dont_answer = ifelse(is.na(StatsMCCorrectnessScore), 1, 0),
    high_score = ifelse(GenAICalPre_all > 3, 1, 0),
    ps_score = PSMAE_score_adjusted * -1,
    StatsMCCorrectnessPercent = StatsMCCorrectnessScore / 34.5
  ) %>%
  filter(treatment_arm != "" | !is.na(treatment_arm)) %>%
  filter(treatment_arm != 2) %>%
  mutate(
    Task1Complete = ifelse(is.na(Task1Complete), 0, Task1Complete),
    Task2Complete = ifelse(is.na(Task2Complete), 0, Task2Complete),
    attrit = ifelse(
      (is.na(CodingProcessGradePercent) & is.na(StatsMCCorrectnessPercent)) |
        (is.na(ps_score) & is.na(CodingProcessGradePercent)) |
        (is.na(ps_score) & is.na(StatsMCCorrectnessPercent)),
      1, 0
    )
  )

write.csv(df, "../computed_objects/experimental_data_all.csv", row.names = FALSE)
