
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

df <- read.csv("../computed_objects/experimental_data.csv") %>% filter(treatment_arm != 2)

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


out.file <- "../writeup/tables/learning_robustness.tex" 
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
                 star.cutoffs = c(0.10, 0.05, 0.01),
                 star.char = c( "*", "**", "***"),
                 font.size = "small",
                column.sep.width = "-4pt",
                ci        = TRUE,          # show CI instead of SE
                ci.custom = robust_ci,     # use robust CI
                p         = robust_p,      # robust p-values (for stars)
                single.row = TRUE,
              #   se = list(sqrt(diag(vcovHC(m.1, type = "HC0"))),
                #           sqrt(diag(vcovHC(m.2, type = "HC0"))),
                 #          sqrt(diag(vcovHC(m.3, type = "HC0"))),
                 #          sqrt(diag(vcovHC(m.4, type = "HC0"))),
                  #         sqrt(diag(vcovHC(m.5, type = "HC0")))),
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
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)

## MAIN TABLE

df <- read_dta("../data/complete_data_all.dta") %>% select(-c(starts_with("ProfessionalID"))) %>%
  mutate(treatment = ifelse(Group == "Test",1, ifelse(Group == "Control",0,NA)))  

m.1 <- lm(DSKnowledgeQ1Grade~treatment, df)
m.2 <- lm(DSKnowledgeQ2Grade~treatment, df)
m.3 <- lm(DSKnowledgeQ3Grade~treatment, df)
m.4 <- lm(DSKnowledgeQ4Grade~treatment, df)
m.5 <- lm(DSKnowledgeQ5Grade~treatment, df)

# Joint test
model <- lm(cbind(DSKnowledgeQ1Grade, DSKnowledgeQ2Grade, DSKnowledgeQ3Grade, DSKnowledgeQ4Grade, DSKnowledgeQ5Grade) ~ treatment, data = df)
linearHypothesis(model, hypothesis.matrix = "treatment = 0")

ct.1 <- coeftest(m.1, vcov = vcovHC(m.1, type = "HC0"))
ct.2 <- coeftest(m.2, vcov = vcovHC(m.2, type = "HC0"))
ct.3 <- coeftest(m.3, vcov = vcovHC(m.3, type = "HC0"))
ct.4 <- coeftest(m.4, vcov = vcovHC(m.2, type = "HC0"))
ct.5 <- coeftest(m.5, vcov = vcovHC(m.3, type = "HC0"))

vars <- c( "DSKnowledgeQ1Correct", "DSKnowledgeQ2Correct", "DSKnowledgeQ3Correct", "DSKnowledgeQ4Correct", "DSKnowledgeQ5Correct")

model_list <- list()
for (var in vars) {
  # If treatment_arm is not a factor, you should convert it before running the model
  formula <- as.formula(paste(var, "~ treatment"))
  # Run the regression model using felm with robust standard errors
  model_list[[paste0( var, "_model")]] <- lm(formula, data = df)
}

treatment_effects <- sapply(model_list, function(model) {
  robust_summary <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
  
  # Extract coefficients and their robust standard errors
  estimates <- coef(robust_summary)
  std.errors <- sqrt(diag(vcovHC(model, type = "HC0")))
  tidy_model <- tidy(model)
  tidy_model$robust_std.error <- std.errors
  
  
  # Subset the rows for the treatment effect
  treatment_row <- tidy_model[tidy_model$term == "treatment", ]
  c(est = treatment_row$estimate, 
    std.error = treatment_row$std.error)
})

#Bayes Factor
results <- list()

# Iterate through each variable in vars
for (var in vars) {
  # Filter out NAs for the current variable
  temp <- df %>% 
    filter(!is.na(.data[[var]])) %>% 
    select(treatment, !!sym(var))
  
  # Extract numeric vectors for treatment and control groups
  treat <- temp %>% filter(treatment == 1) %>% pull(!!sym(var))
  control <- temp %>% filter(treatment == 0) %>% pull(!!sym(var))
  
  # Perform Bayesian t-test
  bf <- ttestBF(x = treat, y = control)
  
  # Store Bayes Factor and variable name in the results list
  results[[var]] <- list(
    variable = var,
    bayes_factor = exp(bf@bayesFactor[["bf"]])  # Extract numeric Bayes Factor
  )
}

# Convert results list to a data frame for easier readability
bayes <- do.call(rbind, lapply(results, as.data.frame))


treatment_effects <- sapply(model_list, function(model) {
  # Use broom::tidy to get a dataframe of term, estimate, and standard error
  coefs <- broom::tidy(model, conf.int = TRUE)
  # Subset the row for the treatment effect
  treatment_row <- coefs[coefs$term == "treatment", ]
  control_row <- coefs[coefs$term == "(Intercept)", ]
  # Return a named vector with estimate and confidence interval
  c(est = treatment_row$estimate, 
    std.error = treatment_row$std.error,
    std.error.control = control_row$std.error)
})

num_vars <- 5
# Create a vector to store the mean values
mean_controls <- numeric(num_vars)

# Loop through each variable number and calculate the mean for control group
for (i in 1:num_vars) {
  var_name <- paste0("DSKnowledgeQ", i, "Correct")
  mean_controls[i] <- mean(df[[var_name]][df$treatment == 0], na.rm = TRUE)
}


out.file <- "../writeup/tables/learning.tex" 
sink("/dev/null")
s <- stargazer(  model_list[[1]], model_list[[2]], model_list[[3]], model_list[[4]], model_list[[5]],
                column.labels = c("Data science or coding question"),
                dep.var.labels = rep("", 5), 
                title = "Effects of AI treatment to post experiment data science knowledge without use of AI",
                label = "tab:learning",
                covariate.labels = c("GenAI Treatment Assigned (Trt)"),
               column.separate = c(5),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.10, 0.05, 0.01),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                keep = c('treatment'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f",mean_controls[[1]]), 
                    sprintf("%.2f", mean_controls[[2]]),
                    sprintf("%.2f", mean_controls[[3]]),
                    sprintf("%.2f", mean_controls[[4]]),
                    sprintf("%.2f", mean_controls[[5]])
                  ) 
                ),
                header = FALSE,
                type = "latex")
sink()
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to answer data science and coding questions, after the conclusion of the experiment.
           Text of questions can be found in Appendix Section~\\ref{sec:postsurvey}.
           All regressions include controls for gender, location, native english status, and low tenure.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)


# --------------
#     Plots
# --------------

treatment_effects <- as.data.frame(t(treatment_effects))
treatment_effects$mean_control <- mean_controls

treatment_effects <- treatment_effects %>% mutate(mean_treatment = mean_control + est)



labels <- list(
  'DSKnowledgeQ1Correct' = paste0('Filtering rows in python \n(select all that apply)'),
  'DSKnowledgeQ2Correct' = paste0('If a coin is tossed 3 times, \nwhat is the probability of getting heads\nevery time? (MC)'),
  'DSKnowledgeQ3Correct'= paste0('Distance-based algorithms are not \naffected by scaling (T/F)'),
  'DSKnowledgeQ4Correct' = paste0('Which of these techniques can be used to\n handle missing data in categorical features?\n (select all that apply)'),
  'DSKnowledgeQ5Correct' = paste0('Under what category of prediction problem \ndoes [this] fall? (select all that apply)')
)

treatment_effects$model <- labels

treatment_effects$model <- factor(labels, levels = rev(labels))

treatment_effects <- treatment_effects %>%
  mutate(ci.lower = mean_treatment - 1.96*std.error,
         ci.upper = mean_treatment + 1.96*std.error,
         ci.lower.c = mean_control - 1.96*std.error.control,
         ci.upper.c = mean_control + 1.96*std.error.control)

df.plot <- pivot_longer(
  treatment_effects,
  cols = c("mean_control", "mean_treatment"),
  names_to = "group",
  values_to = "mean_value"
) %>%
  mutate(
    ci.upper.total = case_when(
      group == "mean_treatment" ~ ci.upper,
      group == "mean_control" ~ ci.upper.c
    ),
    ci.lower.total = case_when(
      group == "mean_treatment" ~ ci.lower,
      group == "mean_control" ~ ci.lower.c
    )
  )

# Plotting
g<- ggplot(df.plot, aes(y = model, x = mean_value, fill = group)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, position = position_dodge(width = c(0.8))) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", size = 0.75) +
  geom_errorbarh(
    data = df.plot, 
    aes(xmin = ci.lower.total, xmax = ci.upper.total),
    height = 0.1, position = position_dodge(0.8)
  ) +
  scale_fill_manual(
    values = c("mean_control" = "white", "mean_treatment" = "grey35"),
    labels = c("Control", "Treatment")
  ) + xlim(0, 1) +
  labs(y = "", x = "Share correctly answer") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

df.plot2 <- df.plot %>% select(c(model, mean_value, ci.lower.total, ci.upper.total, group))
ggplot(df.plot2, aes(y = model, x = mean_value, fill = group)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, position = position_dodge(0.8)) +
geom_errorbarh(
  data = df.plot2, 
  aes(xmin = ci.lower.total, xmax = ci.upper.total), height = 0.5, position = position_dodge(0.8)
)

JJHmisc::writeImage(g, 
                    "learning",
                    width = 6.5, 
                    height = 4, 
                    path = "../writeup/plots/")


## With Distribution

# ============================
# RAW distribution dataset
# ============================

vars_correct <- c("DSKnowledgeQ1Correct",
                  "DSKnowledgeQ2Correct",
                  "DSKnowledgeQ3Correct",
                  "DSKnowledgeQ4Correct",
                  "DSKnowledgeQ5Correct")


df.raw <- df %>%
  select(treatment, all_of(vars_correct)) %>%
  pivot_longer(
    cols = all_of(vars_correct),
    names_to = "question",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    model = labels[question],
    group = if_else(treatment == 1,
                    "mean_treatment",
                    "mean_control")
  )

df.raw$model <- factor(df.raw$model,
                       levels = rev(labels))

df.raw$group <- factor(df.raw$group,
                       levels = c("mean_control", "mean_treatment"))

position_d <- position_dodge(width = 0.8)

g <- ggplot(df.plot,
            aes(y = model,
                x = mean_value,
                fill = group)) +
  
  geom_bar(stat = "identity",
           color = "black",
           width = 0.5,
           position = position_d) +
  
  geom_errorbarh(
    aes(xmin = ci.lower.total,
        xmax = ci.upper.total),
    height = 0.12,
    position = position_d
  ) +
  
  # ---- RAW POINTS ----
geom_point(data = df.raw %>% filter(!is.na(value)),
           aes(x = value,
               y = model,
               group = group),
           position = position_jitterdodge(
             jitter.width = 0.03,
             dodge.width = 0.8
           ),
           alpha = 0.15,
           size = 0.9,
           inherit.aes = FALSE) +
  
  geom_vline(xintercept = 0,
             color = "darkgrey",
             size = 0.75) +
  
  scale_fill_manual(
    values = c("mean_control" = "white",
               "mean_treatment" = "grey35"),
    labels = c("Control", "Treatment")
  ) +
  
  coord_cartesian(xlim = c(0, 1)) +
  
  labs(y = "",
       x = "Share correctly answered") +
  
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_blank()
  )
JJHmisc::writeImage(g, 
                    "learning_dist",
                    width = 6.5, 
                    height = 4, 
                    path = "../writeup/plots/")
