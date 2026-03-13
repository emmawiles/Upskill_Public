library(dplyr)
library(tidyr)
library(stargazer)
library(lfe)
library(forcats)
library(sandwich)
library(lmtest)
library(haven)

df <- read_dta("../data/complete_data_DS.dta")

df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("0" = "Control", "1" = "Treatment", "2" = "DS")

# drop DS arm (same as python script)
df <- df %>% 
  filter(treatment_arm != "DS")

# convert likert responses to numeric
likert_map <- c(
  "Strongly Disagree" = 1,
  "Somewhat Disagree" = 2,
  "Neutral" = 3,
  "Somewhat Agree" = 4,
  "Strongly Agree" = 5
)

df <- df %>%
  mutate(
    Pre2_8_num = likert_map[ProfessionalIdPre2_8],
    Post2_8_num = likert_map[ProfessionalIdPost2_8],
    Pre2_10_num = likert_map[ProfessionalIdPre2_10],
    Post2_10_num = likert_map[ProfessionalIdPost2_10]
  )

# drop missing observations
df <- df %>% 
  drop_na(Pre2_8_num, Post2_8_num, Pre2_10_num, Post2_10_num)

# compute belief changes (post - pre)
df <- df %>%
  mutate(
    PSI_Change = Post2_8_num - Pre2_8_num,
    PE_Change = Post2_10_num - Pre2_10_num
  )

# regressions
m.1 <- felm(PSI_Change ~ treatment_arm, df)
m.2 <- felm(PE_Change ~ treatment_arm, df)

# robust SEs
ct.1 <- coeftest(m.1, vcov = vcovHC(m.1, type = "HC0"))
ct.2 <- coeftest(m.2, vcov = vcovHC(m.2, type = "HC0"))

stargazer(
  m.1, m.2,
  type = "text",
  se = list(
    sqrt(diag(vcovHC(m.1, type = "HC0"))),
    sqrt(diag(vcovHC(m.2, type = "HC0")))
  )
)

ct.1["treatment_armTreatment", "Pr(>|t|)"]
ct.2["treatment_armTreatment", "Pr(>|t|)"]
