library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(lfe)
library(magrittr)
library(msm)
library(forcats)

df <- read.csv("../computed_objects/experimental_data.csv")
df$know_code <- fct_relevel(df$know_code, "No coding experience")
df$PSMAEGradeAdjusted <- -1* df$PSMAEGradeAdjusted

variables <- c("PSMAEGradeAdjusted", "StatsOverallRelativeNorm", "CodingProcessGradeRelativeNorm")

m.1 <- felm(PSMAEGradeAdjusted~treatment, df)
m.2 <- felm(PSMAEGradeAdjusted~treatment+ europe + female + low.tenure + native.english, df)
m.3 <- felm(PSMAEGradeAdjusted~treatment+ europe + female + low.tenure + native.english + factor(Education) + factor(StatsEducation) + factor(STEM) + factor(PredAnalyticsExperience) + factor(DataVizExperience), df)
m.4 <- felm(PSMAEGradeAdjusted~treatment*know_code+ europe + female + low.tenure + native.english, df)
m.5 <- felm(PSMAEGradeAdjusted~treatment*know_code+ europe + female + low.tenure + native.english + factor(Education)  + factor(StatsEducation) + factor(STEM) + factor(PredAnalyticsExperience) + factor(DataVizExperience), df)

m.6 <- felm(StatsOverallRelativeNorm~treatment, df)
m.7 <- felm(StatsOverallRelativeNorm~treatment+ europe + female + low.tenure + native.english, df)
m.8 <- felm(StatsOverallRelativeNorm~treatment+ europe + female + low.tenure + native.english + factor(Education) + factor(StatsEducation) + factor(STEM) + factor(PredAnalyticsExperience) + factor(DataVizExperience), df)
m.9 <- felm(StatsOverallRelativeNorm~treatment*know_code+ europe + female + low.tenure + native.english, df)
m.10 <- felm(StatsOverallRelativeNorm~treatment*know_code+ europe + female + low.tenure + native.english + factor(Education)  + factor(StatsEducation) + factor(STEM) + factor(PredAnalyticsExperience) + factor(DataVizExperience), df)

m.11 <- felm(CodingProcessGradeRelativeNorm~treatment, df)
m.12 <- felm(CodingProcessGradeRelativeNorm~treatment+ europe + female + low.tenure + native.english, df)
m.13 <- felm(CodingProcessGradeRelativeNorm~treatment+ europe + female + low.tenure + native.english + factor(Education) + factor(StatsEducation) + factor(STEM) + factor(PredAnalyticsExperience) + factor(DataVizExperience), df)
m.14 <- felm(CodingProcessGradeRelativeNorm~treatment*know_code+ europe + female + low.tenure + native.english, df)
m.15 <- felm(CodingProcessGradeRelativeNorm~treatment*know_code+ europe + female + low.tenure + native.english + factor(Education)  + factor(StatsEducation) + factor(STEM) + factor(PredAnalyticsExperience) + factor(DataVizExperience), df)

stargazer(m.6, m.7, m.8, m.9, m.10, type = "text")
stargazer(m.11, m.12, m.13, m.14, m.15, type = "text")

# helper to grab the treatment row robust p-value (matches any coef containing "treat")
get_treat_p <- function(m) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC0"))
  rn <- rownames(ct)
  idx <- grep("treat", rn, ignore.case = TRUE)[1]
  if (is.na(idx)) return(NA_real_)
  unname(ct[idx, "Pr(>|t|)"])
}


mods <- list(m.1, m.2, m.3, m.4, m.5,)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)


out.file <- "../writeup/tables/robust_ps.tex" 
sink("/dev/null")
ps1 <- stargazer( m.1, m.2, m.3, m.4, m.5,
                dep.var.labels = c("Mean Absolute Error"),
                title = "Effects of AI to workers performance on a prediction task, relative to data scientists",
                label = "tab:robust_ps",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics","Competent coder",  "Europe", "Female", "Low tenure",
                "Native English speaker",  "Masters degree", "PhD", "Degree in quantitative field", 
                "STEM", "Minimal experience with prediction", "Some experience with prediction", 
                "Data visualization experience",
                "Coding basics x Trt","Competent coder x Trt",  "Constant"),
                omit.stat = c("adj.rsq", "ser", "f"),
                #order=paste0("^", vars.order , "$"),
                no.space = TRUE,
                star.cutoffs = c(0.10, 0.05, 0.01),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the worker's score on the prediction problem, -1 times their mean absolute error from the correct answer, where 0 is a perfect score.
          The first specification is the effect of treatment on their score. 
          The second specification adds controls for gender, office location, native english status, and prior coding ability.
          The third specification is the same as column (2) but with additional controls for different measures of the workers prior technical experience.
         The fourth specification is the same as column (2), where the treatment is interacted with the workers prior coding experience.
          The fifth specification is the same as column (3), where the treatment is interacted with the workers prior coding experience.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(ps1, out.file, note)


stargazer(m.6, m.7, m.8, m.9, m.10, type = "text")
mods <- list(m.6, m.7, m.8, m.9, m.10)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)

out.file <- "../writeup/tables/robust_stats.tex" 
sink("/dev/null")
stats <- stargazer( m.6, m.7, m.8, m.9, m.10,
                  dep.var.labels = c("Statistics Score"),
                  title = "Effects of AI to workers performance on statistics task, relative to data scientists",
                  label = "tab:robust_stats",
                  #column.separate = c(2),
                  covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics","Competent coder",  "Europe", "Female", "Low tenure",
                                       "Native English speaker",  "Masters degree", "PhD", "Degree in quantitative field", 
                                       "STEM", "Minimal experience with prediction", "Some experience with prediction", 
                                       "Data visualization experience",
                                       "Coding basics x Trt","Competent coder x Trt",  "Constant"),
                  omit.stat = c("adj.rsq", "ser", "f"),
                  #order=paste0("^", vars.order , "$"),
                  no.space = TRUE,
                  star.cutoffs = c(0.10, 0.05, 0.01),
                  star.char = c( "*", "**", "***"),
                  font.size = "small",
                  column.sep.width = "-5pt",
                  header = FALSE,
                  type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the worker's score on the statistcis task relative to the data scientists benchmark, where 0 is a perfect score.
          The first specification is the effect of treatment on their score. 
          The second specification adds controls for gender, office location, native english status, and prior coding ability.
          The third specification is the same as column (2) but with additional controls for different measures of the workers prior technical experience.
         The fourth specification is the same as column (2), where the treatment is interacted with the workers prior coding experience.
          The fifth specification is the same as column (3), where the treatment is interacted with the workers prior coding experience.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(stats, out.file, note)

stargazer(m.11, m.12, m.13, m.14, m.15, type = "text")

mods <- list(m.11, m.12, m.13, m.14, m.15,)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)

out.file <- "../writeup/tables/robust_coding.tex" 
sink("/dev/null")
stats <- stargazer( m.11, m.12, m.13, m.14, m.15,
                    dep.var.labels = c("Coding Score"),
                    title = "Effects of AI to workers performance on coding task, relative to data scientists",
                    label = "tab:robust_coding",
                    #column.separate = c(2),
                    covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics","Competent coder",  "Europe", "Female", "Low tenure",
                                         "Native English speaker",  "Masters degree", "PhD", "Degree in quantitative field", 
                                         "STEM", "Minimal experience with prediction", "Some experience with prediction", 
                                         "Data visualization experience",
                                         "Coding basics x Trt","Competent coder x Trt",  "Constant"),
                    omit.stat = c("adj.rsq", "ser", "f"),
                    #order=paste0("^", vars.order , "$"),
                    no.space = TRUE,
                    star.cutoffs = c(0.10, 0.05, 0.01),
                    star.char = c( "*", "**", "***"),
                    font.size = "small",
                    column.sep.width = "-5pt",
                    header = FALSE,
                    type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the worker's score on the coding task relative to the data scientists benchmark, where 0 is a perfect score.
          The first specification is the effect of treatment on their score. 
          The second specification adds controls for gender, office location, native english status, and prior coding ability.
          The third specification is the same as column (2) but with additional controls for different measures of the workers prior technical experience.
         The fourth specification is the same as column (2), where the treatment is interacted with the workers prior coding experience.
          The fifth specification is the same as column (3), where the treatment is interacted with the workers prior coding experience.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(stats, out.file, note)


