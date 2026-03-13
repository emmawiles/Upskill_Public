library(dplyr)
library(tidyr)
library(stargazer)
library(lfe)
library(forcats)
library(sandwich)
library(lmtest)
library(haven)
library(JJHmisc)

table_path <- "../writeup/tables/belief_chatgpt.tex"

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
mods <- list(m.1, m.2)

se.list <- list(
  sqrt(diag(vcovHC(m.1, type = "HC0"))),
  sqrt(diag(vcovHC(m.2, type = "HC0")))
)
p_values <- c(
  coeftest(m.1, vcov = vcovHC(m.1, type = "HC0"))["treatment_armTreatment", "Pr(>|t|)"],
  coeftest(m.2, vcov = vcovHC(m.2, type = "HC0"))["treatment_armTreatment", "Pr(>|t|)"]
)

stargazer(
  m.1, m.2,
  type = "text",
  se = se.list
)

p_values

out.file <- table_path
sink("/dev/null")
s <- stargazer(
  m.1, m.2,
  type = "latex",
  title = "Effects of GenAI on Self-Reported Beliefs in ChatGPT's Impact on Career Development, Before and After Experiment",
  label = "tab:belief_chatgpt",
  dep.var.labels = c(
    "$\\Delta$in Beliefs about ChatGPT's Usefullness for Problem Solving",
    "$\\Delta$ in Beliefs about ChatGPT's Usefullness for Practicality \\& Effectiveness"
  ),
  covariate.labels = c("GenAI Treatment Assigned (Trt)", "Constant"),
  se = se.list,
  omit.stat = c("adj.rsq", "ser", "f"),
  no.space = TRUE,
  star.cutoffs = c(0.10, 0.05, 0.01),
  star.char = c("*", "**", "***"),
  font.size = "small",
  column.sep.width = "-5pt",
  add.lines = list(
    c(
      "p-value (Trt)",
      sprintf("%.3f", p_values[1]),
      sprintf("%.3f", p_values[2])
    )
  ),
  header = FALSE
)
sink()

note <- c(
  "\\\\",
  "\\begin{minipage}{\\textwidth}",
  "{\\footnotesize \\emph{Notes}: ",
  "This table analyzes the effect of treatment on beliefs about how sustained use of ChatGPT for data science would impact career development.",
  "Column (1) measures the change in agreement with the problem solving and insights statement. Column (2) measures the analogous change for practicality and effectiveness.",
  "Both questions used a 5-point Likert scale, and changes are measured as post-experiment score minus pre-experiment score.",
  "Reported entries are coefficient estimates with two-sided 95\\% standard errors computed using Huber--White (HC0) robust variance.",
  "Significance stars and p-values indicate two-sided tests using Huber--White (HC0) robust variance:",
  "* p \\textless 0.10, ** p \\textless 0.05, *** p \\textless 0.01.",
  "\\starlanguage}",
  "\\end{minipage}"
)
JJHmisc::AddTableNote(s, out.file, note)
