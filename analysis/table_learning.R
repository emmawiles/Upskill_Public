library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)
library(haven)
#for learning_robustness

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

# outcomes to table
vars <- c("DSKnowledgeQ1Correct","DSKnowledgeQ2Correct","DSKnowledgeQ3Correct",
          "DSKnowledgeQ4Correct","DSKnowledgeQ5Correct")

df <- read_dta("../data/complete_data_all.dta") %>% select(-c(starts_with("ProfessionalID"))) %>%
  mutate(treatment = ifelse(Group == "Test",1, ifelse(Group == "Control",0,NA)))  

# make sure treatment is 0/1 and not NA
df <- df %>% filter(!is.na(treatment))

# --- define RHS terms here (add controls if you want) ---
rhs_terms <- c("treatment") 
#rhs_terms <- c("treatment","gender","location","native_english","low_tenure")

# --- build models safely with reformulate() ---
model_list <- lapply(vars, function(v) {
  fml <- reformulate(rhs_terms, response = v)  # RHS cannot be empty now
  lm(fml, data = df)
})
names(model_list) <- vars

# --- robust p-values & 95% robust CIs ---
robust_p  <- lapply(model_list, function(m) coeftest(m, vcov = vcovHC(m, type = "HC0"))[, "Pr(>|t|)"])
robust_ci <- lapply(model_list, function(m) {
  V   <- vcovHC(m, type = "HC0")
  se  <- sqrt(diag(V))
  est <- coef(m)
  crit <- qt(.975, df.residual(m))
  cbind(est - crit*se, est + crit*se)  # lower, upper
})

# --- robust SEs and p-values ---
robust_se <- lapply(model_list, function(m) {
  sqrt(diag(vcovHC(m, type = "HC0")))
})

robust_p <- lapply(model_list, function(m) {
  coeftest(m, vcov = vcovHC(m, type = "HC0"))[, "Pr(>|t|)"]
})

# control means row
mean_controls <- sapply(vars, function(v) mean(df[[v]][df$treatment == 0], na.rm = TRUE))

treat_p_row <- c("P-value (Trt)",
                 sapply(model_list, function(m) {
                   ct <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC0"))
                   sprintf("%.3f", unname(ct["treatment", "Pr(>|t|)"]))
                 })
)

# table
out.file <- "../writeup/tables/learning.tex"
s <- stargazer(
  model_list,
  title = "Effects of AI treatment on post-experiment data science knowledge (no AI use)",
  label = "tab:learning",
  column.labels   = "Data science or coding question",
  column.separate = 5,
  dep.var.labels  = rep("", 5),
  covariate.labels = c("GenAI Treatment Assigned (Trt)"),
  keep = "treatment",
  no.space = TRUE,
  omit.stat = c("adj.rsq","ser","f"),
  star.cutoffs = c(.05,.01,.001),
  star.char = c("*","**","***"),
  font.size = "small",
  column.sep.width = "-4pt",
  header = FALSE,
  p = robust_p,
  se = robust_se,
  single.row = FALSE,
  add.lines = list(
    c("Mean Y in Control Group", sprintf("%.2f", mean_controls)),
    treat_p_row
  ),
  type = "latex"
)
note <- c("\\\\",
          "\\begin{minipage}{\\textwidth}",
          "{\\footnotesize \\emph{Notes}: 
           This table analyzes the effect of the treatment on the consultants ability to answer data science and coding questions, after the conclusion of the experiment.
            Full text of questions can be found in Online Appendix B.2.
            Reported entries are coefficient estimates with two-sided 95\\% confidence intervals computed using Huber--White (HC0) robust variance.
            Significance stars and p-values indicate two-sided tests using Huber–White (HC0) robust variance:
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          ",
          "\\end{minipage}")

JJHmisc::AddTableNote(s, out.file, note)

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


## ploints on the plot

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

g
JJHmisc::writeImage(g, 
                    "learning_dist",
                    width = 6.5, 
                    height = 4, 
                    path = "../writeup/plots/")
