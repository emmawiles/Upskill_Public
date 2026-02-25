
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(lfe)

df <- read.csv("../computed_objects/experimental_data_all.csv")

# First pass
# TasksComplete
# TimeTakenStats, TimeTakenCoding, TimeTakenPS

m.finish <- felm(TasksComplete~treatment+ europe + female + low.tenure + native.english, df)
m.timeS <- felm(minutesStats~treatment + europe + female + low.tenure + native.english, df %>% filter(!is.na(StatsMCCorrectnessPercent)))
m.timePS <- felm(minutesPS~treatment + europe + female + low.tenure + native.english, df %>% filter(!is.na(ps_score)))
m.timeC <- felm(minutesCoding~treatment + europe + female + low.tenure + native.english, df %>% filter(!is.na(CodingProcessGradePercent)))
stargazer(m.finish, m.timeS, m.timePS, m.timeC, type = "text")

mean_finish_control <- mean(df$TasksComplete[df$treatment == 0], na.rm = TRUE)
mean_timeS_control <- mean(df$minutesStats[df$treatment == 0], na.rm = TRUE)
mean_timePS_control <- mean(df$minutesPS[df$treatment == 0], na.rm = TRUE)
mean_timeC_control <- mean(df$minutesCoding[df$treatment == 0], na.rm = TRUE)

get_treat_p <- function(m) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC0"))
  rn <- rownames(ct)
  idx <- grep("treat", rn, ignore.case = TRUE)[1]
  if (is.na(idx)) return(NA_real_)
  unname(ct[idx, "Pr(>|t|)"])
}


mods <- list(m.timeS, m.timePS, m.timeC)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)

out.file <- "../writeup/time.tex" 
sink("/dev/null")
s <- stargazer( m.timeS, m.timePS, m.timeC,
                dep.var.labels = c("Mins on Stats", "Mins on Prediction", "Mins on Coding"),
                title = "Effects of AI on number of minutes to complete each task",
                label = "tab:time",
                covariate.labels = c("GenAI Treatment Assigned (Trt)"),
                #star.cutoffs = c(0.10, 0.05, 0.01, 0.001),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                keep = c('treatment'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    #sprintf("%.2f", mean_finish_control), 
                    sprintf("%.2f", mean_timeS_control), 
                    sprintf("%.2f", mean_timePS_control), 
                    sprintf("%.2f", mean_timeC_control)
                    ),
                c("P-value (Trt)",
                  sprintf("%.3f", sapply(mods, get_treat_p))
                )
                ),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the length of time it took for consultants to finish each task, conditional on completion.
           The outcome in Column (1) is the number of minutes they spent on the Statistics task. The outcome in Column (2) is the number of minutes spent on the Problem Solving and Prediction task.
           And the outcome in Column (3) is the number of minutes spent on the Coding task.
           Consultants were randomly assigned two of the three tasks, and given 90 minutes maximum to complete each.
           All regressions include controls for gender, location, native english status, and low tenure.
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)
