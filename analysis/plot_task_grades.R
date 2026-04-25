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
df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("0" = "Control", "1" = "Treatment", "2" = "DS")
df$ps_score <- -1 * df$PSMAEGradeAdjusted

vars <- c("ps_score", "StatsOverallRelativeNorm", "CodingProcessGradeRelativeNorm")

# T vs C only, for SEs
df_tc <- df %>% filter(treatment_arm != "DS")

treatment_effects <- lapply(vars, function(var) {
  
  # Means from full data (already DS-normalized)
  mean_t <- mean(df[[var]][df$treatment_arm == "Treatment"], na.rm = TRUE)
  mean_c <- mean(df[[var]][df$treatment_arm == "Control"],   na.rm = TRUE)
  
  # SE from T vs C regression
  m  <- lm(as.formula(paste(var, "~ treatment_arm")), 
           data = df_tc %>% mutate(treatment_arm = fct_relevel(treatment_arm, "Control")))
  se <- sqrt(diag(vcovHC(m, type = "HC0")))["treatment_armTreatment"]
  
  tibble(mean_treatment = mean_t, mean_control = mean_c, se = se)
})

treatment_effects <- bind_rows(treatment_effects)
treatment_effects$model <- c("Prediction Score", "Statistics Score", "Coding Score")

treatment_effects <- treatment_effects %>%
  mutate(
    t.ci.lower = mean_treatment - 1.96 * se,
    t.ci.upper = mean_treatment + 1.96 * se,
    c.ci.lower = mean_control   - 1.96 * se,
    c.ci.upper = mean_control   + 1.96 * se
  )

df.plot <- treatment_effects %>%
  pivot_longer(
    cols = c("mean_control", "mean_treatment"),
    names_to = "group",
    values_to = "mean_value"
  ) %>%
  mutate(
    ci.lower = if_else(group == "mean_treatment", t.ci.lower, c.ci.lower),
    ci.upper = if_else(group == "mean_treatment", t.ci.upper, c.ci.upper)
  ) %>%
  select(model, group, mean_value, ci.lower, ci.upper)

df.plot$group <- factor(df.plot$group, levels = c("mean_control", "mean_treatment"))
df.plot$model <- factor(df.plot$model, levels = c("Coding Score", "Statistics Score", "Prediction Score"))

g <- ggplot(df.plot, aes(y = model, x = mean_value, fill = group)) +
  geom_col(color = "black", width = 0.5, position = position_dodge(width = 0.7)) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper),
                 height = 0.2, position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 0, color = "darkgrey", size = 0.75) +
  scale_fill_manual(
    values = c("mean_control" = "skyblue", "mean_treatment" = "darkred"),
    labels = c("Control", "Treatment")
  ) +
  xlim(-1, 0.05) +
  labs(y = "", x = "Performance relative to data scientists") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "darkgrey", size = 0.75),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()

ggsave("../writeup/plots/grades.pdf", plot = g, width = 6.5, height = 3.5)

## Appendix version with distribution
df <- read.csv("../computed_objects/experimental_data.csv")
df$treatment_arm <- as.factor(df$treatment_arm)
levels(df$treatment_arm) <- c("0" = "Control", "1" = "Treatment", "2" = "DS")
df$ps_score <- -1 * df$PSMAEGradeAdjusted

df_tc <- df %>% filter(treatment_arm != "DS")

vars <- c("ps_score", "StatsOverallRelativeNorm", "CodingProcessGradeRelativeNorm")

treatment_effects <- lapply(vars, function(var) {
  mean_t <- mean(df[[var]][df$treatment_arm == "Treatment"], na.rm = TRUE)
  mean_c <- mean(df[[var]][df$treatment_arm == "Control"],   na.rm = TRUE)
  
  m  <- lm(as.formula(paste(var, "~ treatment_arm")),
           data = df_tc %>% mutate(treatment_arm = fct_relevel(treatment_arm, "Control")))
  se <- sqrt(diag(vcovHC(m, type = "HC0")))["treatment_armTreatment"]
  
  tibble(mean_treatment = mean_t, mean_control = mean_c, se = se)
})

treatment_effects <- bind_rows(treatment_effects)
treatment_effects$model <- c("Prediction Score", "Statistics Score", "Coding Score")

treatment_effects <- treatment_effects %>%
  mutate(
    t.ci.lower = mean_treatment - 1.96 * se,
    t.ci.upper = mean_treatment + 1.96 * se,
    c.ci.lower = mean_control   - 1.96 * se,
    c.ci.upper = mean_control   + 1.96 * se
  )


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

ggsave("../writeup/plots/grades_dist.pdf", plot = g, width = 6.5, height = 3.5)

