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

df <- read.csv("../computed_objects/experimental_data.csv")
df$know_code <- fct_relevel(df$know_code, "No coding experience")
df$ps_score <- -1* df$PSMAEGradeAdjusted

#StatsClass1RelativeNorm: identifying and correct process errors in modeling problems
#StatsClass2RelativeNorm: data visualization questions
#StatsClass3RelativeNorm: probability questions

m.1 <- felm(StatsOverallRelativeNorm~treatment + europe + female + low.tenure + native.english, df)
m.2 <- felm(ps_score~treatment+ europe + female + low.tenure + native.english, df)
m.3 <- felm(CodingProcessGradeRelativeNorm~treatment+ europe + female + low.tenure + native.english, df)

m.1a <- felm(StatsClass1RelativeNorm~treatment + europe + female + low.tenure + native.english, df)
m.1b <- felm(StatsClass2RelativeNorm~treatment + europe + female + low.tenure + native.english, df)
m.1c <- felm(StatsClass3RelativeNorm~treatment + europe + female + low.tenure + native.english, df)

stargazer( m.1a, m.1b, m.1c,type = "text")
           
m.2a <- felm(ps_score~treatment+ europe + female + low.tenure + native.english, df)
m.2b <- felm(PSMethodologyRelativeNorm~treatment+ europe + female + low.tenure + native.english, df)

m.3a <- felm(CodingDataCleanRelativeNorm~treatment+ europe + female + low.tenure + native.english, df)
m.3b <- felm(CodingDataCorrectRelativeNorm~treatment+ europe + female + low.tenure + native.english, df)
m.3c <- felm(CodingMergeFilterRelativeNorm~treatment+ europe + female + low.tenure + native.english, df)

mean_control1 <- mean(df$StatsClass1RelativeNorm[df$treatment == 0], na.rm = TRUE)
mean_control2 <- mean(df$StatsClass2RelativeNorm[df$treatment == 0], na.rm = TRUE)
mean_control3 <- mean(df$StatsClass3RelativeNorm[df$treatment == 0], na.rm = TRUE)

mean_control4 <- mean(df$ps_score[df$treatment == 0], na.rm = TRUE)
mean_control5 <- mean(df$PSMethodologyRelativeNorm[df$treatment == 0], na.rm = TRUE)

mean_control6 <- mean(df$CodingDataCleanRelativeNorm[df$treatment == 0], na.rm = TRUE)
mean_control7 <- mean(df$CodingDataCorrectRelativeNorm[df$treatment == 0], na.rm = TRUE)
mean_control8 <- mean(df$CodingMergeFilterRelativeNorm[df$treatment == 0], na.rm = TRUE)

mods <- list(m.1a, m.1b, m.1c)

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

out.file <- "../writeup/tables/grades_detail_stats.tex" 
sink("/dev/null")
s1 <- stargazer( m.1a, m.1b, m.1c,
                dep.var.labels = c("Identifying Errors", "Visual ML Questions" ,"Probability Questions"),
                title = "Effect of AI treatment on workers' statistics performance, relative to data scientists",
                label = "tab:grades_detail_stats",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)"),
                #covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics", "Proficient coder", "Coding basics x Trt", "Proficient coder x Trt"),
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
                    sprintf("%.2f",mean_control2), 
                    sprintf("%.2f",mean_control3)
                  ), treat_p_row
                ),
          
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table reports the effect of the treatment on the consultants grades on each step of the statistics task. 
  Each outcome is the worker's grade on each set of problems, where 0 is a perfect score and -1 is the lowest possible score. 
  The first outcome is the workers' score on a problem where they apply statistical metrics and interpret their graphs.
   The second outcome is their score on questions where they look at data plotted and decide what types of classifier to use on it.
    The third outcome is their score on probability questions.
  The sample includes all experimental participants who submitted something for grading on the statistics task.
   Text of problems can be found in Appendix Section~\ref{sec:tasks}.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
            Reported entries are coefficient estimates with two-sided 95\\% standard errors computed using Huber--White (HC0) robust variance.
            Significance stars and p-values indicate two-sided tests using Huber–White (HC0) robust variance:
* p \\textless 0.10, ** p \\textless 0.05, *** p \\textless 0.01.
           All regressions include controls for gender, location, native english status, and low tenure.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s1, out.file, note)



mods <- list(m.2a, m.2b)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)
out.file <- "../writeup/tables/grades_detail_ps.tex" 
sink("/dev/null")
s2 <- stargazer( m.2a, m.2b, 
                dep.var.labels = c("Mean Absolute Error", "Process Score"),
                title = "Effect of AI treatment on workers' prediction scores, relative to data scientists",
                label = "tab:grades_detail_ps",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)"),
                #covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics", "Proficient coder", "Coding basics x Trt", "Proficient coder x Trt"),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.10, 0.05, 0.01),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('europe', 'female', 'low.tenure', 'native.english', 'Constant'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_control4), 
                    sprintf("%.2f",mean_control5)
                  ) , treat_p_row
                ),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table reports the effect of the treatment on the consultants performance on the prediction task. 
  Each outcome is the worker's grade on each set of problems, where 0 is a perfect score and -1 is the lowest possible score. 
            The first outcome is the score they got on the prediction problem, which is -1 times their mean absolute error.
   The second outcome is the percentage of ``correct'' steps they took.
  The sample includes all experimental participants who submitted something for grading on the prediction task.
   Text of problems can be found in Appendix Section~\ref{sec:tasks}.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
           All regressions include controls for gender, location, native english status, and low tenure.
                Reported entries are coefficient estimates with two-sided 95\\% standard errors computed using Huber--White (HC0) robust variance.
            Significance stars and p-values indicate two-sided tests using Huber–White (HC0) robust variance:
* p \\textless 0.10, ** p \\textless 0.05, *** p \\textless 0.01.
           All regressions include controls for gender, location, native english status, and low tenure.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s2, out.file, note)

mods <- list(m.3a, m.3b, m.3c)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)
out.file <- "../writeup/tables/grades_detail_coding.tex" 
sink("/dev/null")
s3 <- stargazer( m.3a, m.3b, m.3c, 
                dep.var.labels = c("Data Cleaning", "Data Correct", "Merge \\& Filter"),
                title = "Effect of AI treatment on workers' coding performance, relative to data scientists",
                label = "tab:grades_detail_coding",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)"),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.10, 0.05, 0.01),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('europe', 'female', 'low.tenure', 'native.english', 'Constant'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_control6), 
                    sprintf("%.2f",mean_control7),
                    sprintf("%.2f",mean_control8)
                  ) , treat_p_row
                ),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table reports the effect of the treatment on the consultants performance on the coding task. 
  The y-axis is the worker's binary grade on each set of actions in python, where 0 is a perfect score and -1 is the lowest possible score. 
  The first outcome is whether or not the worker correctly cleaned the data.
  The second outcome is whether or not they correctly handle nulls and duplicates.
  The third outcome is whether or not they correctly merge and then filter a dataset. 
  The sample includes all experimental participants who submitted something for grading on the prediction task.
   Text of problems can be found in Appendix Section~\ref{sec:tasks}.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
           All regressions include controls for gender, location, native english status, and low tenure.      Reported entries are coefficient estimates with two-sided 95\\% standard errors computed using Huber--White (HC0) robust variance.
            Significance stars and p-values indicate two-sided tests using Huber–White (HC0) robust variance:
* p \\textless 0.10, ** p \\textless 0.05, *** p \\textless 0.01.
         ",
          "\\end{minipage}")
JJHmisc::AddTableNote(s3, out.file, note)