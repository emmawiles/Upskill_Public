library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(lfe)
library(magrittr)
library(msm)
library(forcats)
library(sandwich)
library(lmtest)
library(haven)

df <- read_dta("../data/complete_data_DS.dta")
df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("0" = "Control", "1" = "Treatment", "2" = "DS")
df$treatment_arm <- fct_relevel(df$treatment_arm, "DS")

df$PSMAEGradeAdjusted <- -1* df$PSMAEGradeAdjusted

m.1 <- felm(CodingProcessGradeRelativeNorm~treatment_arm, df)
m.2 <- felm(StatsOverallRelativeNorm~treatment_arm, df)
m.3 <- felm(PSMAEGradeAdjusted~treatment_arm, df)

ct.1 <- coeftest(m.1, vcov = vcovHC(m.1, type = "HC0"))
ct.2 <- coeftest(m.2, vcov = vcovHC(m.2, type = "HC0"))
ct.3 <- coeftest(m.3, vcov = vcovHC(m.3, type = "HC0"))

stargazer(m.1, m.2, m.3, type = "text", 
          se = list(sqrt(diag(vcovHC(m.1, type = "HC0"))),
                    sqrt(diag(vcovHC(m.2, type = "HC0"))),
                    sqrt(diag(vcovHC(m.3, type = "HC0")))))

get_arm_p <- function(m, arm_name) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC0"))
  
  coef_name <- paste0("treatment_arm", arm_name)
  idx <- which(rownames(ct) == coef_name)
  
  if (length(idx) == 0) return(NA_real_)
  
  unname(ct[idx, "Pr(>|t|)"])
}

mods <- list(m.1, m.2, m.3)

out.file <- "../writeup/performanceDSBaseline.tex" 
sink("/dev/null")
s <- stargazer( m.1, m.2, m.3,
                dep.var.labels = c("Coding Task Score", "Stats Task Score" ,"Prediction Task Score"),
                title = "Effects of AI to workers performance on tasks, relative to data scientists",
                label = "tab:benchmark",
                #column.separate = c(2),
                covariate.labels = c( "Assigned Control", "GenAI Treatment Assigned (Trt)"),# "Knowledge of GPT's strengths", "Knowledge of GPT's strengths x Trt"),
                #star.cutoffs = c(0.10, 0.05, 0.01, 0.001),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('Constant'),
                add.lines = list(
                  c("P-value (Assigned Control)",
                      sprintf("%.3f", sapply(mods, get_arm_p, arm_name = "Control"))
                    ),
                  c("P-value (GenAI Treatment)",
                      sprintf("%.3f", sapply(mods, get_arm_p, arm_name = "Treatment"))
                    )
                  ),
                se = list(sqrt(diag(vcovHC(m.1, type = "HC0"))),
                          sqrt(diag(vcovHC(m.2, type = "HC0"))),
                          sqrt(diag(vcovHC(m.3, type = "HC0")))),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
            This table illustrates the performance of workers in the control and treatment arms compared to data scientists'.
           The first outcome is the percentage of correct steps they took on the coding problem, weighted by difficulty.
            The second outcome is the points the worker got on the statistics question, divided by the maximum points. 
            The third outcome is their mean absolute error of the distance from their answer to the data scientist's answer.
            Exact definition of grading for each problem can be found in Appendix Section~\ref{sec:tasks}.
            All standard errors are Huber White robust SEs.* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)
