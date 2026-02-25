library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(stargazer)
library(lfe)
library(magrittr)
library(msm)
library(forcats)
library(lmtest)
library(sandwich)

df <- read.csv("../computed_objects/experimental_data.csv")
df$know_code <- fct_relevel(df$know_code, "No coding experience")
df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("0" = "Control", "1" = "Treatment", "2" = "DS")
df$treatment_arm <- fct_relevel(df$treatment_arm, "DS")
df$ps_score <- -1* df$PSMAEGradeAdjusted

# --------------
#     Plots
# --------------

vars <- c("ps_score",  "StatsOverallRelativeNorm","CodingProcessGradeRelativeNorm")

model_list <- list()
for (var in vars) {
  # If treatment_arm is not a factor, you should convert it before running the model
  formula <- as.formula(paste(var, "~ treatment_arm"))
  # Run the regression model using felm with robust standard errors
  model_list[[paste0( var, "_model")]] <- lm(formula, data = df)
}

treatment_effects <- sapply(model_list, function(model) {
  robust_summary <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
  
  # Extract coefficients and their robust standard errors
  estimates <- coef(robust_summary)
  std.errors <- sqrt(diag(vcovHC(model, type = "HC0")))
  
  # Use broom::tidy to create a dataframe of coefficients
  tidy_model <- tidy(model)
  
  # Add robust standard errors
  tidy_model$robust_std.error <- std.errors
  
  # Subset the rows for the treatment effect
  treatment_row <- tidy_model[tidy_model$term == "treatment_armTreatment", ]
  control_row <- tidy_model[tidy_model$term == "treatment_armControl", ]
  
  c(mean_treatment = treatment_row$estimate, 
    treatment_std.error = treatment_row$robust_std.error,
    mean_control = control_row$estimate, 
    control_std.error = control_row$robust_std.error)
})

treatment_effects <- as.data.frame(t(treatment_effects))
#treatment_effects$ds <- ds
treatment_effects %<>% 
  mutate(control_std.error = control_std.error.treatment_armControl,
         treatment_std.error = treatment_std.error.treatment_armTreatment) %>%
  mutate(t.ci.lower = mean_treatment - 1.96*treatment_std.error,
         t.ci.upper = mean_treatment + 1.96*treatment_std.error,
         c.ci.lower = mean_control - 1.96*control_std.error,
         c.ci.upper = mean_control + 1.96*control_std.error )

labels <- list( 'ps_score' = paste0('Prediction Score'),
                'StatsOverallRelativeNorm' = paste0('Statistics Score'),
                'CodingProcessGradeRelativeNorm'= paste0('Coding Score')
)

treatment_effects$model <- labels

treatment_effects$model <- factor(labels, levels = rev(labels))

df.plot <- pivot_longer(
  treatment_effects,
  cols = c("mean_control", "mean_treatment"),
  names_to = "group",
  values_to = "mean_value"
) %>%
mutate(
  ci.lower = if_else(group == "mean_treatment", t.ci.lower, c.ci.lower),
  ci.upper = if_else(group == "mean_treatment", t.ci.upper, c.ci.upper)
) %>%
  select(-t.ci.lower, -t.ci.upper, -c.ci.lower, -c.ci.upper) 

m.1 <- felm(ps_score~treatment, df)
m.2 <- felm(StatsOverallRelativeNorm~treatment, df)
m.3 <- felm(CodingProcessGradeRelativeNorm~treatment, df)
stargazer(m.1, m.2, m.3, type = "text")

se <- sapply(list(m.1, m.2, m.3), function(model) {
  sqrt(diag(vcovHC(model, type = "HC0")))
})
se <- se[2,]

df.plot <- df.plot %>% cbind(se) %>%
  mutate(te.ci.lower = mean_value - 1.96*se,
         te.ci.upper = mean_value + 1.96*se)

df.plot$group <- factor(df.plot$group, levels = c("mean_control", "mean_treatment"))


# Plotting
g<- ggplot(df.plot, aes(y = model, x = mean_value, fill = group)) +
  geom_col(stat = "identity", color = "black", width = 0.5, position = position_dodge(width = c(0,1))) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", size = 0.75) +
  geom_errorbarh(
    data = df.plot,
  aes(xmin = ci.lower, xmax = ci.upper),
    height = 0.1, position = position_dodge(width = c(0,1))
  ) +
  scale_fill_manual(
    values = c( "mean_control" = "skyblue", "mean_treatment" = "darkred"),
    labels = c( "Control", "Treatment")
  ) +
  xlim(-1, 0.05) +
  labs(y = "", x = "Performance relative to data scientists") +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line.x = element_line(color = "darkgrey", size = 0.75),
        panel.grid.minor = element_blank()
  ) + coord_flip()

JJHmisc::writeImage(g, 
                    "grades",
                    width = 6.5, 
                    height = 3.5, 
                    path = "../writeup/plots/")


## With distribution
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(forcats)
library(lmtest)
library(sandwich)
library(magrittr)

# ============================
# Load data
# ============================

df <- read.csv("../computed_objects/experimental_data.csv")

df$know_code <- fct_relevel(df$know_code, "No coding experience")

df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("0" = "Control",
                              "1" = "Treatment",
                              "2" = "DS")

df$treatment_arm <- fct_relevel(df$treatment_arm, "DS")

df$ps_score <- -1 * df$PSMAEGradeAdjusted

# ============================
# Estimate means + robust SEs
# ============================

vars <- c("ps_score",
          "StatsOverallRelativeNorm",
          "CodingProcessGradeRelativeNorm")

model_list <- list()

for (var in vars) {
  formula <- as.formula(paste(var, "~ treatment_arm"))
  model_list[[var]] <- lm(formula, data = df)
}

treatment_effects <- lapply(model_list, function(model) {
  
  robust_vcov <- vcovHC(model, type = "HC0")
  robust_se   <- sqrt(diag(robust_vcov))
  
  tidy_model <- tidy(model)
  tidy_model$robust_se <- robust_se
  
  treatment_row <- tidy_model %>%
    filter(term == "treatment_armTreatment")
  
  control_row <- tidy_model %>%
    filter(term == "treatment_armControl")
  
  tibble(
    mean_treatment = treatment_row$estimate,
    treatment_se   = treatment_row$robust_se,
    mean_control   = control_row$estimate,
    control_se     = control_row$robust_se
  )
})

treatment_effects <- bind_rows(treatment_effects)

# Add labels
treatment_effects$model <- c("Prediction Score",
                             "Statistics Score",
                             "Coding Score")

# Compute confidence intervals
treatment_effects <- treatment_effects %>%
  mutate(
    t.ci.lower = mean_treatment - 1.96 * treatment_se,
    t.ci.upper = mean_treatment + 1.96 * treatment_se,
    c.ci.lower = mean_control - 1.96 * control_se,
    c.ci.upper = mean_control + 1.96 * control_se
  )

# ============================
# Reshape for plotting
# ============================

df.plot <- treatment_effects %>%
  pivot_longer(
    cols = c("mean_control", "mean_treatment"),
    names_to = "group",
    values_to = "mean_value"
  ) %>%
  mutate(
    ci.lower = if_else(group == "mean_treatment",
                       t.ci.lower,
                       c.ci.lower),
    ci.upper = if_else(group == "mean_treatment",
                       t.ci.upper,
                       c.ci.upper)
  ) %>%
  select(model, group, mean_value, ci.lower, ci.upper)

df.plot$group <- factor(df.plot$group,
                        levels = c("mean_control",
                                   "mean_treatment"))

# ============================
# Create RAW distribution dataset
# ============================

df.raw <- df %>%
  select(treatment_arm,
         ps_score,
         StatsOverallRelativeNorm,
         CodingProcessGradeRelativeNorm) %>%
  pivot_longer(
    cols = -treatment_arm,
    names_to = "outcome",
    values_to = "value"
  ) %>%
  mutate(
    model = recode(outcome,
                   ps_score = "Prediction Score",
                   StatsOverallRelativeNorm = "Statistics Score",
                   CodingProcessGradeRelativeNorm = "Coding Score"),
    group = if_else(treatment_arm == "Treatment",
                    "mean_treatment",
                    "mean_control")
  ) %>%
  filter(!is.na(value))

df.raw$model <- factor(df.raw$model,
                       levels = unique(df.plot$model))

df.raw$group <- factor(df.raw$group,
                       levels = levels(df.plot$group))

# ============================
# Plot
# ============================

position_d <- position_dodge(width = 0.7)

g <- ggplot(df.plot,
            aes(y = model,
                x = mean_value,
                fill = group)) +
  
  geom_col(stat = "identity",
           color = "black",
           width = 0.5,
           position = position_d) +
  
  geom_errorbarh(aes(xmin = ci.lower,
                     xmax = ci.upper),
                 height = 0.2,
                 position = position_d) +
  
  # Raw data overlay
  geom_point(data = df.raw,
             aes(x = value,
                 y = model,
                 group = group),
             position = position_jitterdodge(
               jitter.width = 0.02,
               dodge.width = 0.7
             ),
             alpha = 0.15,
             size = 1,
             inherit.aes = FALSE) +
  
  geom_vline(xintercept = 0,
             color = "darkgrey",
             size = 0.75) +
  
  scale_fill_manual(
    values = c("mean_control" = "skyblue",
               "mean_treatment" = "darkred"),
    labels = c("Control", "Treatment")
  ) +
  
 # xlim(-1, 0.05) +
  
  labs(y = "",
       x = "Performance relative to data scientists") +
  
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "darkgrey", size = 0.75),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()

# ============================
# Save
# ============================

JJHmisc::writeImage(g,
                    "grades_dist",
                    width = 6.5,
                    height = 3.5,
                    path = "../writeup/plots/")

