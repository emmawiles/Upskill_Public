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

