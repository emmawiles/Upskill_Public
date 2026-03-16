library(haven)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sandwich)
library(ggplot2)
library(stargazer)
library(lfe)
library(lmtest)
library(broom)
library(BayesFactor)

df <- read.csv("../upskill/computed_objects/experimental_data.csv") %>% filter(treatment_arm != 2)

m.1 <- lm(DSKnowledgeQ1Grade~treatment, df %>% filter (StartedCodingTask == 1))
m.2 <- lm(DSKnowledgeQ2Grade~treatment, df %>% filter (StartedStatsTask == 1))
m.3 <- lm(DSKnowledgeQ3Grade~treatment, df %>% filter (StartedPSTask == 1))
m.4 <- lm(DSKnowledgeQ4Grade~treatment, df %>% filter (StartedPSTask == 1))
m.5 <- lm(DSKnowledgeQ5Grade~treatment, df %>% filter (StartedPSTask == 1))

mods <- list(m.1, m.2, m.3, m.4, m.5)

stargazer(m.1, m.2, m.3, m.4, m.5, type = "text", 
          se = list(sqrt(diag(vcovHC(m.1, type = "HC0"))),
                    sqrt(diag(vcovHC(m.2, type = "HC0"))),
                    sqrt(diag(vcovHC(m.3, type = "HC0"))),
                    sqrt(diag(vcovHC(m.4, type = "HC0"))),
                    sqrt(diag(vcovHC(m.5, type = "HC0")))))
library(car)
model <- lm(cbind(DSKnowledgeQ1Grade, DSKnowledgeQ2Grade, DSKnowledgeQ3Grade, DSKnowledgeQ4Grade, DSKnowledgeQ5Grade) ~ treatment, data = df)
linearHypothesis(model, hypothesis.matrix = "treatment = 0")

# Robust p-values for each coefficient (used for stars)
robust_p <- lapply(mods, function(m) {
  coeftest(m, vcov = vcovHC(m, type = "HC0"))[, "Pr(>|t|)"]
})

# Robust 95% CIs for each coefficient
robust_ci <- lapply(mods, function(m, level = 0.95) {
  V   <- vcovHC(m, type = "HC0")
  se  <- sqrt(diag(V))
  est <- coef(m)
  alpha <- 1 - level
  crit  <- qt(1 - alpha/2, df = df.residual(m))
  cbind(est - crit*se, est + crit*se)  # columns: lower, upper
})

# Extract robust p-values for treatment coefficient only
treat_pvals <- sapply(robust_p, function(x) x["treatment"])
treat_pvals_fmt <- sprintf("%.3f", treat_pvals)

out.file <- "../writeup/learning_robustness.tex" 
sink("/dev/null")
s <- stargazer( m.1, m.2, m.3, m.4, m.5,
                column.labels = c("Data science or coding question"),
                dep.var.labels = rep("", 5), 
                title = "Effects of AI treatment to post experiment data science knowledge without use of AI",
                label = "tab:learning",
                covariate.labels = c("GenAI Treatment Assigned (Trt)", "Assigned Coding Task", "Assigned Coding Task x Trt", "Assigned Stats Task", "Assigned Stats Task x Trt", "Assigned Prediction Task", "Assigned Prediction Task x Trt", "Constant"),
                column.separate = c(5),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                column.sep.width = "-4pt",
                p         = robust_p,      # robust p-values (for stars)
                add.lines = list(
                  c("Robust p-value (Treatment)", treat_pvals_fmt)
                ),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to answer data science and coding questions, after the conclusion of the experiment.
           The first problem is about coding, the second is about statistics, and the final three are about machine learning and prediction.
           For each problem the treatment is interacted with an indicator for whichever task gave the worker experience in that topic.
           Full text of questions can be found in Online Appendix B.2.
            Reported entries are coefficient estimates with \\textbf{95\\% confidence intervals computed using Huber--White (HC0) robust variance}.
           Significance stars and \\textbf{p-values are based on the same HC0 robust variance}.
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)