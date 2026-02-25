
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
library(lfe)
library(magrittr)
library(msm)

df <- read.csv("../computed_objects/experimental_data.csv") %>% mutate(treatment = as.factor(treatment))


m1 <- felm(GenAICalPost1_1 ~ treatment + europe + female + low.tenure + native.english, df)
m2 <- felm(GenAICalPost2_1 ~ treatment + europe + female + low.tenure + native.english, df)
m3 <- felm(GenAICalPost3_1 ~ treatment + europe + female + low.tenure + native.english, df)
m4 <- felm(GenAICalPost4_1 ~ treatment + europe + female + low.tenure + native.english, df)
m5 <- felm(GenAICalPost5_1 ~ treatment + europe + female + low.tenure + native.english, df)
m6 <- felm(GenAICalPost6_1 ~ treatment + europe + female + low.tenure + native.english, df)
m7 <- felm(GenAICalPost7_1 ~ treatment + europe + female + low.tenure + native.english, df)


num_vars <- 7
# Create a vector to store the mean values
mean_controls <- numeric(num_vars)

# Loop through each variable number and calculate the mean for control group
for (i in 1:num_vars) {
  var_name <- paste0("GenAICalPost", i, "_1")
  mean_controls[i] <- mean(df[[var_name]][df$treatment == 0], na.rm = TRUE)
}

get_treat_p <- function(m) {
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC0"))
  rn <- rownames(ct)
  idx <- grep("treat", rn, ignore.case = TRUE)[1]
  if (is.na(idx)) return(NA_real_)
  unname(ct[idx, "Pr(>|t|)"])
}


mods <- list(m1, m2, m3, m4, m5, m6, m7)

treat_p_row <- c(
  "P-value (Trt)",
  sprintf("%.3f", sapply(mods, get_treat_p))
)

stargazer(m1, m2, m3, m4, m5, m6, m7, type = "text")
mean_controls
out.file <- "../writeup/callibration.tex" 
sink("/dev/null")
s <- stargazer(m1, m2, m3, m4, m5, m6, m7,
               title = "Effects of AI treatment to post experiment questions about GPT-4's capabilities",
               label = "tab:callibration",
               column.labels = rep(" ``Can GPT-4 answer [this question] correctly?''"),
               dep.var.labels = rep("", 7),  # to remove dependent variable labels
               covariate.labels = c("GenAI Treatment Assigned (Trt)"),
                column.separate = c(7),
                omit.stat = c("adj.rsq", "ser", "f"),
                no.space = TRUE,
                star.cutoffs = c(0.05, 0.01, 0.001),
                star.char = c( "*", "**", "***"),
                font.size = "small",
                keep = c('treatment'),
                add.lines = list(
                  c("Mean Y in Control Group", 
                    sprintf("%.2f", mean_controls[1]), 
                    sprintf("%.2f", mean_controls[2]),
                    sprintf("%.2f", mean_controls[3]),
                    sprintf("%.2f", mean_controls[4]),
                    sprintf("%.2f", mean_controls[5]),
                    sprintf("%.2f", mean_controls[6]),
                    sprintf("%.2f", mean_controls[7])
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
           This table analyzes the effect of the treatment on the consultants confidence in GPT-4's ability to get the right answer on various questions, after the conclusion of the experiment.
           For each question, the consultant gave a percentage confidence in GPT-4's ability to answer the question correctly.
           The question in Columns 1 and 4, and 5 GPT-4 usually get correct. Questions 2,3,6 and7, GPT-4 almost never get correct.
           Text of questions can be found in Appendix Section~\\ref{sec:postsurvey}.
           All regressions include controls for gender, location, native english status, and low tenure.
* p \\textless 0.05, ** p \\textless 0.01, *** p \\textless 0.001.
          \\starlanguage}",
          "\\end{minipage}")
JJHmisc::AddTableNote(s, out.file, note)



treatment_effects <- sapply(model_list, function(model) {
  coefs <- broom::tidy(model, conf.int = TRUE)
  treatment_row <- coefs[coefs$term == "treatment1", ]
  c(est = treatment_row$estimate, 
    std.error = treatment_row$std.error)
})

treatment_effects <- as.data.frame(t(treatment_effects))
treatment_effects$mean_control <- mean_controls
treatment_effects %<>% mutate(mean_treatment = mean_control + est,
                              ci.lower = mean_treatment - 1.96*std.error,
                              ci.upper = mean_treatment + 1.96*std.error)

custom_labels <- c(
  "Q1: Develop an HTML page with JavaScript\n and canvas to draw a US flag that rotates\n 90 clockwise each time it is clicked.\n (GPT-4 can do)",
  "Q2: Based on the following data, which \n cities have an even-numbered population? \n (GPT-4 cannot do) ",
  "Q3: Splitting a $10 bill evenly based on \n items with different prices  \n (GPT-4 cannot do)",
  "Q4: Help on crossword clues \n (GPT-4 can do)",
  "Q5: Who lost the Super Bowl two years \n after Pan-Am filed for bankruptcy? \n (GPT-4 can do)",
  "Q6: Write out the word ''hello'' as an \n ascii art drawing with # and _  \n (GPT-4 cannot do) ",
  "Q7: What is the best next move for O \n in the following game of Tic Tac Toe?  \n (GPT-4 cannot do) "
)

treatment_effects$model <- custom_labels

treatment_effects$model <- factor(custom_labels, levels = rev(custom_labels))

df.plot <- pivot_longer(
  treatment_effects,
  cols = c("mean_control", "mean_treatment"),
  names_to = "group",
  values_to = "mean_value"
)


g<- ggplot(df.plot, aes(y = model, x = est)) +
  geom_point(size = 1.5) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", size = 0.75) +  # Add vertical line at x = 0
  geom_errorbarh(aes(xmin = est - std.error*1.96, xmax = est + std.error*1.96), height = 0.1) +
  labs(y = "", x = "Treatment effect, in percentage points") +
  theme_minimal() #+
  #theme(axis.text.y = element_text(face = "bold"))

JJHmisc::writeImage(g,
                    "callibration_te",
                    width = 6, 
                    height = 4.5,
                    path = "../writeup/plots/"
)


g <- ggplot(df.plot, 
       aes(y = model, x = mean_value, fill = group, width = 0.6)
           ) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = c(0,1.6))) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey", size = 0.75) +
  geom_errorbarh(
    data = df.plot, 
    aes(xmin = ci.lower, xmax = ci.upper),
    height = 0.1
  ) +
  scale_fill_manual(
    values = c("mean_control" = "white", "mean_treatment" = "grey35"),
    labels = c("Control", "Treatment")
  ) + xlim(0, 100) +
  labs(y = "", x = "Confidence that GPT-4 can correctly answer") +
  theme_minimal() +
  theme(legend.position = "top", 
        legend.title = element_blank())

JJHmisc::writeImage(g,
                    "callibration",
                    width = 6, 
                    height = 5,
                    path = "../writeup/plots/"
)

