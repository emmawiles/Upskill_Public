library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(lfe)
library(magrittr)
library(msm)
library(forcats)

df <- read.csv("../computed_objects/experimental_data_all.csv") %>%
  mutate(finished_stats = ifelse(!is.na(StatsMCCorrectnessPercent) & !is.na(StatsTaskOrder),1,ifelse(!is.na(StatsTaskOrder),0,NA)),
         Task1Complete = ifelse(is.na(Task1Complete),0,Task1Complete),
         Task2Complete = ifelse(is.na(Task2Complete),0,Task2Complete),
         TasksComplete = ifelse(Task1Complete ==1 & Task2Complete ==1,1,0),
         finished_ps = ifelse(!is.na(ps_score) & !is.na(PSTaskOrder),1, ifelse(!is.na(PSTaskOrder),0,NA)),
         finished_coding = ifelse(!is.na(CodingProcessGradePercent) & !is.na(CodingTaskOrder),1, ifelse(!is.na(CodingTaskOrder),0,NA)),
         finished_both = !attrit) %>% filter(treatment_arm != 2)

# If Task1 is completed in no grade, give it a zero
# and if PS_order ==1 , then give their 
chisq.test(table(df$treatment_arm, df$finished_coding))
chisq.test(table(df$treatment_arm, df$finished_ps))
chisq.test(table(df$treatment_arm, df$finished_stats))



m.1 <- felm(Task1Complete~treatment + europe + female + low.tenure + native.english, df)
m.2 <- felm(Task2Complete~treatment + europe + female + low.tenure + native.english, df)
m.3 <- felm(TasksComplete~treatment+ europe + female + low.tenure + native.english, df)
m.4 <- felm(finished_both~treatment+ europe + female + low.tenure + native.english, df)
m.5 <- felm(finished_stats~treatment+ europe + female + low.tenure + native.english, df)
m.6 <- felm(finished_ps~treatment+ europe + female + low.tenure + native.english, df)
m.7 <- felm(finished_coding~treatment+ europe + female + low.tenure + native.english, df)

mean_control1 <- mean(df$Task1Complete[df$treatment == 0], na.rm = TRUE)
mean_control2 <- mean(df$TasksComplete[df$treatment == 0], na.rm = TRUE)
mean_control5 <- mean(df$finished_stats[df$treatment == 0], na.rm = TRUE)
mean_control6 <- mean(df$finished_ps[df$treatment == 0], na.rm = TRUE)
mean_control7 <- mean(df$finished_coding[df$treatment == 0], na.rm = TRUE)


mods <- list(m.1, m.2)

# helper to grab the treatment row robust p-value (matches any coef containing "treat")
get_treat_p <- function(m) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC0"))
  rn <- rownames(ct)
  idx <- grep("treat", rn, ignore.case = TRUE)[1]
  if (is.na(idx)) return(NA_real_)
  unname(ct[idx, "Pr(>|t|)"])
}

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)

out.file <- "../writeup/tables/task_finish2.tex" 
sink("/dev/null")
s <- stargazer( m.1, m.2,
                dep.var.labels = c("Task 1 Submitted", "Task 2 Submitted"),
                title = "Effects of AI to whether or not they get submit any answer on each task",
                label = "tab:task_finish2",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)"),# "Knowledge of GPT's strengths", "Knowledge of GPT's strengths x Trt"),
                #star.cutoffs = c(0.10, 0.05, 0.01, 0.001),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.10, 0.05, 0.01),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('europe', 'female', 'low.tenure', 'native.english', 'Constant'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_control1), 
                    sprintf("%.2f",mean_control2)
                  ), treat_p_row
                ),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants submitting any answer to each question.
            Text of problems can be found in Appendix Section~\\ref{sec:tasks}.
           All regressions include controls for gender, location, native english status, and low tenure.
              Reported entries are coefficient estimates with two-sided 95\\% standard errors computed using Huber--White (HC0) robust variance.
            Significance stars and p-values indicate two-sided tests using Huber–White (HC0) robust variance:
* p \\textless 0.10, ** p \\textless 0.05, *** p \\textless 0.01.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)


mods <- list(m.5, m.6, m.7)
get_treat_p <- function(m) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC0"))
  rn <- rownames(ct)
  idx <- grep("treat", rn, ignore.case = TRUE)[1]
  if (is.na(idx)) return(NA_real_)
  unname(ct[idx, "Pr(>|t|)"])
}

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)

stargazer::stargazer(m.5, m.6, m.7, type = "text")
out.file <- "../writeup/tables/task_finish.tex" 
sink("/dev/null")
s <- stargazer( m.5, m.6, m.7,
                dep.var.labels = c("Stats Submitted", "Prediction Submitted" ,"Coding Submitted"),
                title = "Effects of AI to whether or not they get submit any answer on each task",
                label = "tab:task_finish",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)"),# "Knowledge of GPT's strengths", "Knowledge of GPT's strengths x Trt"),
                #star.cutoffs = c(0.10, 0.05, 0.01, 0.001),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.10, 0.05, 0.01),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('europe', 'female', 'low.tenure', 'native.english', 'Constant'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_control5), 
                    sprintf("%.2f",mean_control6), 
                    sprintf("%.2f",mean_control7)
                  ), treat_p_row 
                ),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants submitting any answer to each question.
            Text of problems can be found in Appendix Section~\\ref{sec:tasks}.
           All regressions include controls for gender, location, native english status, and low tenure.
              Reported entries are coefficient estimates with two-sided 95\\% standard errors computed using Huber--White (HC0) robust variance.
            Significance stars and p-values indicate two-sided tests using Huber–White (HC0) robust variance:
* p \\textless 0.10, ** p \\textless 0.05, *** p \\textless 0.01.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)