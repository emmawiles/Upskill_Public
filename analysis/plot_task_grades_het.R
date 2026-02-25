library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

df <- read.csv("../computed_objects/experimental_data.csv")
df$PSMAEGradeAdjusted <- -1 * df$PSMAEGradeAdjusted

code_levels <- c("Competent coder", "Coding basics", "No coding experience")
outcomes <- c("CodingProcessGradeRelativeNorm", "StatsOverallRelativeNorm", "PSMAEGradeAdjusted")

df.plot <- expand.grid(code = code_levels, model = outcomes, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    mean_control = mean(df[[model]][df$know_code == code & df$treatment == 0], na.rm = TRUE),
    fit = list(lm(as.formula(paste(model, "~ treatment")), data = df %>% filter(know_code == code))),
    te = coef(summary(fit))["treatment", "Estimate"],
    std.error = coef(summary(fit))["treatment", "Std. Error"],
    mean_treatment = mean_control + te
  ) %>%
  ungroup() %>%
  mutate(
    code = ifelse(code == "No coding experience", "Never coded", code),
    code = ifelse(code == "Coding basics", "Basic coding", code),
    model = dplyr::recode(
      model,
      CodingProcessGradeRelativeNorm = "Coding Score",
      StatsOverallRelativeNorm = "Statistics Score",
      PSMAEGradeAdjusted = "Prediction Score"
    )
  ) %>%
  select(code, model, mean_control, mean_treatment, std.error)

df.plot2 <- pivot_longer(
  df.plot,
  cols = c("mean_control", "mean_treatment"),
  names_to = "group",
  values_to = "mean_value"
) %>%
  mutate(
    ci.lower = mean_value - 1.96 * std.error,
    ci.upper = mean_value + 1.96 * std.error
  )

df.plot2$group <- factor(df.plot2$group, levels = c("mean_control", "mean_treatment"))
df.plot2$model <- factor(df.plot2$model, levels = c("Coding Score", "Statistics Score", "Prediction Score"))
df.plot2$code <- factor(df.plot2$code, levels = c("Never coded", "Basic coding", "Competent coder"))

g <- ggplot(df.plot2, aes(y = code, x = mean_value, fill = group)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, position = position_dodge(width = c(0, 1))) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = ci.lower, xmax = ci.upper),
    height = 0.1,
    position = position_dodge(width = c(0, 1))
  ) +
  scale_fill_manual(
    values = c("mean_control" = "skyblue", "mean_treatment" = "darkred"),
    labels = c("Control", "Treatment")
  ) +
  facet_wrap(~model, ncol = 3) +
  labs(y = "", x = "Performance relative to data scientists") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "darkgrey", linewidth = 0.75),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  coord_flip() +
  scale_x_continuous(
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2),
    limits = c(-1, 0.2)
  )

JJHmisc::writeImage(
  g,
  "grades_by_code",
  width = 6.5,
  height = 4.5,
  path = "../writeup/plots/"
)

# =====================================================
# RAW DISTRIBUTION OVERLAY VERSION
# Saves as: grades_by_code_dist
# =====================================================

df$ps_score <- df$PSMAEGradeAdjusted

df$know_code <- fct_relevel(df$know_code, "No coding experience")

vars <- c("ps_score",
          "StatsOverallRelativeNorm",
          "CodingProcessGradeRelativeNorm")

labels <- c("Prediction Score",
            "Statistics Score",
            "Coding Score")

# ============================
# Estimate means + robust SEs by coding level
# ============================

results_list <- list()

for (code_level in unique(df$know_code)) {
  
  df_sub <- df %>% filter(know_code == code_level)
  
  model_list <- lapply(vars, function(var) {
    lm(as.formula(paste(var, "~ treatment")), data = df_sub)
  })
  
  treatment_effects <- lapply(model_list, function(model) {
    
    robust_vcov <- vcovHC(model, type = "HC0")
    robust_se   <- sqrt(diag(robust_vcov))
    
    tidy_model <- tidy(model)
    tidy_model$robust_se <- robust_se
    
    treatment_row <- tidy_model %>%
      filter(term == "treatment")
    
    control_mean <- mean(model$model[[1]][model$model$treatment == 0], na.rm = TRUE)
    
    tibble(
      mean_treatment = treatment_row$estimate + control_mean,
      treatment_se   = treatment_row$robust_se,
      mean_control   = control_mean,
      control_se     = sd(model$model[[1]][model$model$treatment == 0], na.rm = TRUE) /
        sqrt(sum(model$model$treatment == 0))
    )
  })
  
  tmp <- bind_rows(treatment_effects)
  tmp$model <- labels
  tmp$know_code <- code_level
  
  results_list[[as.character(code_level)]] <- tmp
}

treatment_effects <- bind_rows(results_list)

# Clean coding labels
treatment_effects$know_code <- recode(treatment_effects$know_code,
                                      "No coding experience" = "Never coded",
                                      "Coding basics" = "Basic coding")

# ============================
# Confidence intervals
# ============================

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

df.plot.dist <- treatment_effects %>%
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
  select(model, know_code, group, mean_value, ci.lower, ci.upper)

df.plot.dist$group <- factor(df.plot.dist$group,
                             levels = c("mean_control",
                                        "mean_treatment"))

df.plot.dist$model <- factor(df.plot.dist$model,
                             levels = labels)

df.plot.dist$know_code <- factor(df.plot.dist$know_code,
                                 levels = c("Never coded",
                                            "Basic coding",
                                            "Competent coder"))

# ============================
# RAW distribution dataset
# ============================

df.raw.dist <- df %>%
  select(know_code,
         treatment,
         ps_score,
         StatsOverallRelativeNorm,
         CodingProcessGradeRelativeNorm) %>%
  pivot_longer(
    cols = c(ps_score,
             StatsOverallRelativeNorm,
             CodingProcessGradeRelativeNorm),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  mutate(
    model = recode(outcome,
                   ps_score = "Prediction Score",
                   StatsOverallRelativeNorm = "Statistics Score",
                   CodingProcessGradeRelativeNorm = "Coding Score"),
    know_code = recode(know_code,
                       "No coding experience" = "Never coded",
                       "Coding basics" = "Basic coding"),
    group = if_else(treatment == 1,
                    "mean_treatment",
                    "mean_control")
  ) %>%
  filter(!is.na(value))

df.raw.dist$model <- factor(df.raw.dist$model,
                            levels = levels(df.plot.dist$model))

df.raw.dist$group <- factor(df.raw.dist$group,
                            levels = levels(df.plot.dist$group))

df.raw.dist$know_code <- factor(df.raw.dist$know_code,
                                levels = levels(df.plot.dist$know_code))

position_d <- position_dodge(width = 0.7)

g.dist <- ggplot(df.plot.dist,
                 aes(y = know_code,
                     x = mean_value,
                     fill = group)) +
  
  geom_col(color = "black",
           width = 0.5,
           position = position_d) +
  
  geom_errorbarh(aes(xmin = ci.lower,
                     xmax = ci.upper),
                 height = 0.2,
                 position = position_d) +
  
  geom_point(data = df.raw.dist,
             aes(x = value,
                 y = know_code,
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
  
  facet_wrap(~model, ncol = 3) +
  
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

JJHmisc::writeImage(g.dist,
                    "grades_by_code_dist",
                    width = 6.5,
                    height = 4.5,
                    path = "../writeup/plots/")

