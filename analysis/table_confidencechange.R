library(dplyr)
library(stargazer)
library(lfe)
library(sandwich)
library(lmtest)
library(haven)

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

# ---- Robust SE ----

se.list <- list(
  sqrt(diag(vcovHC(m.1, type="HC0"))),
  sqrt(diag(vcovHC(m.2, type="HC0"))),
  sqrt(diag(vcovHC(m.3, type="HC0")))
)
stargazer(
  m.1, m.2, m.3,
  type = "text",
  se = list(
    sqrt(diag(vcovHC(m.1, type = "HC0"))),
    sqrt(diag(vcovHC(m.2, type = "HC0"))),
    sqrt(diag(vcovHC(m.3, type="HC0")))
  )
)

ct.1 <- coeftest(m.1, vcov = vcovHC(m.1, type = "HC0"))
ct.2 <- coeftest(m.2, vcov = vcovHC(m.2, type = "HC0"))
ct.3 <- coeftest(m.3, vcov = vcovHC(m.3, type = "HC0"))

ct.1["treatment_armTreatment", "Pr(>|t|)"]
ct.2["treatment_armTreatment", "Pr(>|t|)"]
ct.3["treatment_armTreatment", "Pr(>|t|)"]

