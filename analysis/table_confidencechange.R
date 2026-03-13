library(dplyr)
library(stargazer)
library(lfe)
library(sandwich)
library(lmtest)
library(haven)
library(JJHmisc)

table_path <- "../writeup/tables/professional_identity.tex"

df <- read_dta("../data/complete_data_DS.dta")

# Keep control and treatment only
df <- df %>%
  filter(treatment_arm != 2)

df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("Control","Treatment")

# ---- Create change variables ----
df <- df %>%
  mutate(
    
    # Column 1
    delta_analytics =
      GenAICalPost1_1 - GenAICalPre1_5,
    
    # Column 2
    delta_ds_conf =
      DSConfidencePost_1 - DSConfidence_1,
    
    # Column 3 composite confidence
    pre_conf =
      ProfessionalIdPre1_4 +
      ProfessionalIdPre1_5 +
      ProfessionalIdPre1_9,
    
    post_conf =
      ProfessionalIdPost1_4 +
      ProfessionalIdPost1_5 +
      ProfessionalIdPost1_9,
    
    delta_genai_conf =
      post_conf - pre_conf
  )

# ---- Models ----

m.1 <- felm(delta_analytics ~ treatment_arm, df)
m.2 <- felm(delta_ds_conf ~ treatment_arm, df)
m.3 <- felm(delta_genai_conf ~ treatment_arm, df)

mods <- list(m.1, m.2, m.3)

se.list <- list(
  sqrt(diag(vcovHC(m.1, type = "HC0"))),
  sqrt(diag(vcovHC(m.2, type = "HC0"))),
  sqrt(diag(vcovHC(m.3, type = "HC0")))
)
stargazer(
  m.1, m.2, m.3,
  type = "text",
  se = se.list
)

p_values <- c(
  coeftest(m.1, vcov = vcovHC(m.1, type = "HC0"))["treatment_armTreatment", "Pr(>|t|)"],
  coeftest(m.2, vcov = vcovHC(m.2, type = "HC0"))["treatment_armTreatment", "Pr(>|t|)"],
  coeftest(m.3, vcov = vcovHC(m.3, type = "HC0"))["treatment_armTreatment", "Pr(>|t|)"]
)
p_values

out.file <- table_path
sink("/dev/null")
s <- stargazer(
  m.1, m.2, m.3,
  type = "latex",
  title = "Effects of GenAI on Self-Reported Capabilities and Professional Confidence, Before and After Experiment",
  label = "tab:professional_identity",
  dep.var.labels = c(
    "$\\Delta$in Beliefs in Data Analytics Abilities",
    "$\\Delta$in Confidence in Data Science Skills",
    "$\\Delta$ in Confidence with GenAI"
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
      sprintf("%.3f", p_values[2]),
      sprintf("%.3f", p_values[3])
    )
  ),
  header = FALSE
)
sink()

note <- c(
  "\\\\",
  "\\begin{minipage}{\\textwidth}",
  "{\\footnotesize \\emph{Notes}: ",
  "This table analyzes the effect of treatment on self-reported capabilities and professional confidence before and after the experiment.",
  "Column (1) measures change in beliefs about data analytics abilities, Column (2) measures change in confidence in data science skills, and Column (3) measures change in confidence when using GenAI.",
  "Reported entries are coefficient estimates with two-sided 95\\% standard errors computed using Huber--White (HC0) robust variance.",
  "Significance stars and p-values indicate two-sided tests using Huber--White (HC0) robust variance:",
  "* p \\textless 0.10, ** p \\textless 0.05, *** p \\textless 0.01.",
  "\\starlanguage}",
  "\\end{minipage}"
)
JJHmisc::AddTableNote(s, out.file, note)
