library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(lfe)
library(magrittr)
library(msm)
library(forcats)
library(lmtest)
library(sandwich)


df <- read.csv("../computed_objects/experimental_data.csv") %>% filter(treatment_arm != 2)

df$know_code <- fct_relevel(df$know_code, "No coding experience")
df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("0" = "Control", "1" = "Treatment", "2" = "DS")
df$treatment_arm <- fct_relevel(df$treatment_arm, "DS")

df$PSMAEGradeAdjusted <- -1* df$PSMAEGradeAdjusted

m.1 <- felm(CodingProcessGradeRelativeNorm~treatment, df)
m.2 <- felm(StatsOverallRelativeNorm~treatment, df)
m.3 <- felm(PSMAEGradeAdjusted~treatment, df)

ct.1 <- coeftest(m.1, vcov = vcovHC(m.1, type = "HC0"))
ct.2 <- coeftest(m.2, vcov = vcovHC(m.2, type = "HC0"))
ct.3 <- coeftest(m.3, vcov = vcovHC(m.3, type = "HC0"))

stargazer(m.1, m.2, m.3, type = "text", 
          se = list(sqrt(diag(vcovHC(m.1, type = "HC0"))),
                    sqrt(diag(vcovHC(m.2, type = "HC0"))),
                    sqrt(diag(vcovHC(m.3, type = "HC0")))))


# Standard deviation of scores in control group
sd_control1 <- sd(df$CodingProcessGradeRelativeNorm[df$treatment == 0], na.rm = TRUE)
sd_control2 <- sd(df$StatsOverallRelativeNorm[df$treatment == 0], na.rm = TRUE)
sd_control3 <- sd(df$PSMAEGradeAdjusted[df$treatment == 0], na.rm = TRUE)

# Print the results
sd_control1
sd_control2
sd_control3

mean_control1 <- mean(df$CodingProcessGradeRelativeNorm[df$treatment == 0], na.rm = TRUE)
mean_control2 <- mean(df$StatsOverallRelativeNorm[df$treatment == 0], na.rm = TRUE)
mean_control3 <- mean(df$PSMAEGradeAdjusted[df$treatment == 0], na.rm = TRUE)

# helper to grab the treatment row robust p-value (matches any coef containing "treat")
get_treat_p <- function(m) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC0"))
  rn <- rownames(ct)
  idx <- grep("treat", rn, ignore.case = TRUE)[1]
  if (is.na(idx)) return(NA_real_)
  unname(ct[idx, "Pr(>|t|)"])
}


mods <- list(m.1, m.2, m.3)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)

out.file <- "../writeup/task_correct.tex" 
sink("/dev/null")
s <- stargazer(
  m.1, m.2, m.3,
  dep.var.labels = c("Coding Task Score", 
                     "Stats Task Score", 
                     "Prediction Task Score"),
  title = "Effects of AI to workers performance on data science tasks, relative to data scientists",
  label = "tab:task_correct",
  covariate.labels = c("GenAI Treatment Assigned (Trt)"),
  omit.stat = c("adj.rsq", "ser", "f"),
  no.space = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  star.char = c("*", "**", "***"),
  font.size = "small",
  column.sep.width = "-5pt",
  omit = c("Constant"),
  
  add.lines = list(
    c("Mean Y in Control Group", 
      sprintf("%.2f", mean_control1), 
      sprintf("%.2f", mean_control2), 
      sprintf("%.2f", mean_control3)
    ),
    c("P-value (Trt)",
      sprintf("%.3f", sapply(mods, get_treat_p))
    )
  ),
  
  se = list(
    sqrt(diag(vcovHC(m.1, type = "HC0"))),
    sqrt(diag(vcovHC(m.2, type = "HC0"))),
    sqrt(diag(vcovHC(m.3, type = "HC0")))
  ),
  
  header = FALSE,
  type = "latex"
)
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to correctly answer questions.
           Each outcome is normalized relative to the performance of BCG data scientists.
          The first outcome is the percentage of correct steps they take in answering the coding question.
            The second outcome is the sum of the consultant's score on each statistics question, divided by the total number of possible points. 
            The third outcome is the score they got on the prediction problem, which is -1 times their mean absolute error from the correct answer.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
           All standard errors are huber white robust. 
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)
#            ``Knowledge of GPT's strengths'' is 1 if the consultant got 4 out of 7, or more of questions correct on the pre-experiment survey asking about their guesses of whether or not GPT-4 can correctly answer a question.

## Het TEs
m.stats1 <- felm(StatsOverallRelativeNorm~treatment*know_code, df)
m.ps1 <- felm(PSMAEGradeAdjusted~treatment*know_code, df)
m.code1 <- felm(CodingProcessGradeRelativeNorm~treatment*know_code, df)
stargazer(m.stats1, m.ps1, m.code1, type = "text")

out.file <- "../writeup/task_correct_het1.tex" 
sink("/dev/null")
s <- stargazer(  m.code1, m.stats1, m.ps1,
                dep.var.labels = c("Coding Task Score", "Stats Task Score", "Prediction Task Score"),
                title = "Effects of AI to workers performance on data science tasks, relative to data scientists",
                label = "tab:task_correct_het1",
                #column.separate = c(2),
                #covariate.labels = c("GenAI Treatment Assigned (Trt)"),# "Knowledge of GPT's strengths", "Knowledge of GPT's strengths x Trt"),
                covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics", "Competent coder", "Coding basics x Trt", "Competent coder x Trt"),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('Constant'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_control1), 
                    sprintf("%.2f",mean_control2), 
                    sprintf("%.2f",mean_control3)
                  ) )  ,
                se = list(sqrt(diag(vcovHC(m.code1, type = "HC0"))),
                          sqrt(diag(vcovHC(m.stats1, type = "HC0"))),
                          sqrt(diag(vcovHC(m.ps1, type = "HC0")))),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to correctly answer questions, by their pre-experiment coding knowledge.
           The omitted variable is ``No prior coding experience.'' Workers who report having basic coding ability are classified as ``Coding basics'' and those who report knowing how to code are classifed as ``competent coders.''
            Each outcome is normalized relative to the performance of BCG data scientists.
            The first outcome is the percentage of correct steps they take in answering the coding question.
            The second outcome is the sum of the consultant's score on each statistics question, divided by the total number of possible points. 
            The third outcome is the score they got on the prediction problem, which is 1- their mean absolute error.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
           All standard errors are huber white robust.
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)

m.stats2 <- felm(StatsOverallRelativeNorm~treatment*high_score, df)
m.ps2 <- felm(PSMAEGradeAdjusted~treatment*high_score, df)
m.code2 <- felm(CodingProcessGradeRelativeNorm~treatment*high_score, df)
stargazer(m.stats2, m.ps2, m.code2, type = "text")

out.file <- "../writeup/task_correct_het2.tex" 
sink("/dev/null")
s <- stargazer( m.code2, m.stats2, m.ps2, 
                dep.var.labels = c("Coding Task Score", "Stats Task Score", "Prediction Task Score"),
                title = "Effects of AI to workers performance on data science tasks, relative to data scientists",
                label = "tab:task_correct_het2",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)", "Knowledge of GPT's strengths", "Knowledge of GPT's strengths x Trt"),
                #covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics", "Proficient coder", "Coding basics x Trt", "Proficient coder x Trt"),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('Constant'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_control1), 
                    sprintf("%.2f",mean_control2), 
                    sprintf("%.2f",mean_control3)
                  ) ),             
                se = list(sqrt(diag(vcovHC(m.code2, type = "HC0"))),
                         sqrt(diag(vcovHC(m.stats2, type = "HC0"))),
                         sqrt(diag(vcovHC(m.ps2, type = "HC0")))),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to correctly answer questions, by their pre-experiment coding knowledge.
           ``Knowledge of GPT's strengths'' is 1 if the consultant got 4 out of 7, or more of questions correct on the pre-experiment survey asking about their guesses of whether or not GPT-4 can correctly answer a question.
            The omitted variable is consultants who got fewer tha 4 out of 7 correct.
                       Each outcome is normalized relative to the performance of BCG data scientists.
            The first outcome is the percentage of correct steps they take in answering the coding question.
            The second outcome is the sum of the consultant's score on each statistics question, divided by the total number of possible points. 
            The third outcome is the score they got on the prediction problem, which is -1 times their mean absolute error.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
            All standard errors are huber white robust.
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)
#            

# By overconfidence level
m.stats3 <- felm(StatsOverallRelativeNorm~treatment*GenAICalPreOver, df)
m.ps3 <- felm(PSMAEGradeAdjusted~treatment*GenAICalPreOver, df)
m.code3 <- felm(CodingProcessGradeRelativeNorm~treatment*GenAICalPreOver, df)
stargazer( m.code3, m.stats3, m.ps3, type = "text", 
           se = list(sqrt(diag(vcovHC(m.code3, type = "HC0"))),
                     sqrt(diag(vcovHC(m.stats3, type = "HC0"))),
                     sqrt(diag(vcovHC(m.ps3, type = "HC0")))))
out.file <- "../writeup/task_correct_het3.tex" 
sink("/dev/null")
s <- stargazer( m.code3, m.stats3, m.ps3, 
                dep.var.labels = c("Coding Task Score", "Stats Task Score", "Prediction Task Score"),
                title = "Effects of AI to workers performance on data science tasks, relative to data scientists",
                label = "tab:task_correct_het3",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)", "Overconfident", "Overconfident x Trt"),
                #covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics", "Proficient coder", "Coding basics x Trt", "Proficient coder x Trt"),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
                omit = c('Constant'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_control1), 
                    sprintf("%.2f",mean_control2), 
                    sprintf("%.2f",mean_control3)
                  ) ),
                se = list(sqrt(diag(vcovHC(m.code3, type = "HC0"))),
                          sqrt(diag(vcovHC(m.stats3, type = "HC0"))),
                          sqrt(diag(vcovHC(m.ps3, type = "HC0")))),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to correctly answer questions, by their pre-experiment estimates of GPT-4's current capabilities.
           ``Overconfident'' is 1 if the consultant believed that GPT-4 could correctly answer at least three of the five questions which GPT-4 could not do.
                      Each outcome is normalized relative to the performance of BCG data scientists.
            The first outcome is the percentage of correct steps they take in answering the coding question.
            The second outcome is the sum of the consultant's score on each statistics question, divided by the total number of possible points. 
            The third outcome is the score they got on the prediction problem, which is -1 times their mean absolute error.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
            All standard errors are huber white robust.
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)

m.stats4 <- felm(StatsOverallRelativeNorm~treatment*StatsEducation, df)
m.ps4 <- felm(PSMAEGradeAdjusted~treatment*StatsEducation, df)
m.code4 <- felm(CodingProcessGradeRelativeNorm~treatment*StatsEducation, df)


out.file <- "../writeup/task_correct_het4.tex" 
sink("/dev/null")
s <- stargazer(m.code4, m.stats4, m.ps4, 
                dep.var.labels = c("Coding Task Score", "Stats Task Score", "Prediction Task Score"),
                title = "Effects of AI to workers performance on data science tasks, relative to data scientists",
                label = "tab:task_correct_het4",
                #column.separate = c(2),
                covariate.labels = c("GenAI Treatment Assigned (Trt)", "Quant Degree", "Quant Degree x Trt"),
                #covariate.labels = c("GenAI Treatment Assigned (Trt)", "Coding basics", "Proficient coder", "Coding basics x Trt", "Proficient coder x Trt"),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-5pt",
               omit = c('Constant'),
               add.lines = list(
                 c("Mean Y in Control Group", 
                   sprintf("%.2f",mean_control1), 
                   sprintf("%.2f",mean_control2), 
                   sprintf("%.2f",mean_control3)
                 ) ),
               se = list(sqrt(diag(vcovHC(m.code4, type = "HC0"))),
                         sqrt(diag(vcovHC(m.stats4, type = "HC0"))),
                         sqrt(diag(vcovHC(m.ps4, type = "HC0")))),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to correctly answer questions, by their pre-experiment estimates of GPT-4's current capabilities.
           ``Quant Degree'' is 1 if the worker reports having any formal degree in Statistics, Economics, Mathematics or Data Science.
             Each outcome is normalized relative to the performance of BCG data scientists.
                       The first outcome is the percentage of correct steps they take in answering the coding question.
            The second outcome is the sum of the consultant's score on each statistics question, divided by the total number of possible points. 
            The third outcome is the score they got on the prediction problem, which is -1 times their mean absolute error.
            Exact definition of grading for each problem can be found in Appendix Section~\\ref{sec:tasks}.
             All standard errors are huber white robust.
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)



### Plots


p1 <- ggplot(df, aes(x = CodingProcessGradeRelativeNorm, color = treatment_arm)) +
  stat_ecdf(geom = "step") +
  labs(title = "CDF of Coding Process Grade Norm",
       x = "Coding Process Grade Norm",
       y = "Empirical CDF") +
  scale_color_manual(values = c("Control" = "red", "Treatment" = "blue"))
p2 <- ggplot(df, aes(x = StatsOverallRelativeNorm, color = treatment_arm)) +
  stat_ecdf(geom = "step") +
  labs(title = "CDF of Stats Overall Relative Norm",
       x = "Stats Overall Relative Norm",
       y = "Empirical CDF") +
  scale_color_manual(values = c("Control" = "red", "Treatment" = "blue"))

# Plot CDF for PSMAEGradeAdjusted
p3 <- ggplot(df, aes(x = PSMAEGradeAdjusted, color = treatment_arm)) +
  stat_ecdf(geom = "step") +
  labs(title = "CDF of PSMAE Grade Adjusted",
       x = "PSMAE Grade Adjusted",
       y = "Empirical CDF") +
  scale_color_manual(values = c("Control" = "red", "Treatment" = "blue"))

p4 <- ggplot(df, aes(x = PSMethodologyRelativeNorm, color = treatment_arm)) +
  stat_ecdf(geom = "step") +
  labs(title = "CDF of Problem Solving Process Score",
       x = "Problem Solving Process Score",
       y = "Empirical CDF") +
  scale_color_manual(values = c("Control" = "red", "Treatment" = "blue"))


# quantile regressions

library(quantreg)


# Specify the quantiles
quantiles <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)


# Function to fit models and extract coefficients and CIs
fit_quantile_model <- function(data, formula, quantiles) {
  sapply(quantiles, function(q) {
    model <- rq(formula, data = data, tau = q)
    summary_model <- summary(model, se = "boot")
    
    # Extract coefficient and CI for the treatment effect
    c(Coefficient = summary_model$coefficients[2, 1],
      Lower = summary_model$coefficients[2, 1] - summary_model$coefficients[2, 2],
      Upper = summary_model$coefficients[2, 1] + summary_model$coefficients[2, 2])
  })
}

# Apply function to each outcome
results_Coding <- fit_quantile_model(df, CodingProcessGradeRelativeNorm ~ treatment, quantiles)
results_Stats <- fit_quantile_model(df, StatsOverallRelativeNorm ~ treatment, quantiles)
results_PSMAE <- fit_quantile_model(df, PSMAEGradeAdjusted ~ treatment, quantiles)

results_PSMAE <- fit_quantile_model(df, PSMethodologyRelativeNorm ~ treatment, quantiles)

# Combine results into a data frame
results <- data.frame(
  Quantile = rep(quantiles, 3),
  Outcome = rep(c("Coding", "Stats", "Prediction"), each = length(quantiles)),
  Coefficient = c(results_Coding[1,], results_Stats[1,], results_PSMAE[1,]),
  Lower = c(results_Coding[2,], results_Stats[2,], results_PSMAE[2,]),
  Upper = c(results_Coding[3,], results_Stats[3,], results_PSMAE[3,])
)

# Plot the results with CIs
ggplot(results, aes(x = Quantile, y = Coefficient, color = Outcome)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.02) +
  labs(title = "Quantile Regression Treatment Effects with 95% CI",
       x = "Quantile",
       y = "Treatment Coefficient",
       color = "Outcome") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +  # Set x-axis breaks
  theme_minimal()


