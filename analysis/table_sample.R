library(haven)
library(dplyr)

df <- read_dta("../data/complete_data_all.dta")
df_main <- read.csv("../computed_objects/experimental_data.csv")
df_all <- read.csv("../computed_objects/experimental_data_all.csv")

# Keep only randomized treatment/control allocation for Table 1.
base <- df %>% filter(Group %in% c("Test", "Control"))

is_treat <- base$Group == "Test"
is_ctrl <- base$Group == "Control"

fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_p <- function(p) ifelse(is.na(p), "", formatC(p, format = "f", digits = 3))
fmt_r <- function(x) formatC(x, format = "f", digits = 3)

prop_p <- function(success_t, n_t, success_c, n_c) {
  if (n_t == 0 || n_c == 0) return(NA_real_)
  suppressWarnings(prop.test(c(success_t, success_c), c(n_t, n_c))$p.value)
}

stage_balance_p <- function(n_t, n_c) {
  if ((n_t + n_c) == 0) return(NA_real_)
  suppressWarnings(chisq.test(c(n_t, n_c), p = c(0.5, 0.5))$p.value)
}

# Panel A: flow from allocation into analysis sample (cumulative counts).
n_alloc_t <- sum(is_treat)
n_alloc_c <- sum(is_ctrl)
n_alloc <- n_alloc_t + n_alloc_c

began_t <- sum(!is.na(base$treatment_arm) & is_treat)
began_c <- sum(!is.na(base$treatment_arm) & is_ctrl)
began <- began_t + began_c

task1_t <- sum(base$Task1Complete == 1 & is_treat, na.rm = TRUE)
task1_c <- sum(base$Task1Complete == 1 & is_ctrl, na.rm = TRUE)
task1 <- task1_t + task1_c

task2_t <- sum(base$Task1Complete == 1 & base$Task2Complete == 1 & is_treat, na.rm = TRUE)
task2_c <- sum(base$Task1Complete == 1 & base$Task2Complete == 1 & is_ctrl, na.rm = TRUE)
task2 <- task2_t + task2_c

p_began <- stage_balance_p(began_t, began_c)
p_task1 <- stage_balance_p(task1_t, task1_c)
p_task2 <- stage_balance_p(task2_t, task2_c)

# Panel B: task completion counts in the main analysis dataset.
panel_b <- bind_rows(
  {
    tdat <- df_main %>% filter(treatment_arm == 1)
    cdat <- df_main %>% filter(treatment_arm == 0)
    completed_t <- sum(!is.na(tdat$CodingProcessGradePercent))
    completed_c <- sum(!is.na(cdat$CodingProcessGradePercent))
    started_t <- sum(!is.na(df_all$CodingTaskOrder) & df_all$treatment_arm == 1)
    started_c <- sum(!is.na(df_all$CodingTaskOrder) & df_all$treatment_arm == 0)
    data.frame(
      task = "Coding",
      completed_total = completed_t + completed_c,
      completed_t = completed_t,
      completed_c = completed_c,
      p = prop_p(completed_t, started_t, completed_c, started_c),
      stringsAsFactors = FALSE
    )
  },
  {
    tdat <- df_main %>% filter(treatment_arm == 1)
    cdat <- df_main %>% filter(treatment_arm == 0)
    completed_t <- sum(!is.na(tdat$StatsOverallRelativeNorm))
    completed_c <- sum(!is.na(cdat$StatsOverallRelativeNorm))
    started_t <- sum(!is.na(df_all$StatsTaskOrder) & df_all$treatment_arm == 1)
    started_c <- sum(!is.na(df_all$StatsTaskOrder) & df_all$treatment_arm == 0)
    data.frame(
      task = "Statistics",
      completed_total = completed_t + completed_c,
      completed_t = completed_t,
      completed_c = completed_c,
      p = prop_p(completed_t, started_t, completed_c, started_c),
      stringsAsFactors = FALSE
    )
  },
  {
    tdat <- df_main %>% filter(treatment_arm == 1)
    cdat <- df_main %>% filter(treatment_arm == 0)
    completed_t <- sum(!is.na(tdat$PSMAEGradeAdjusted))
    completed_c <- sum(!is.na(cdat$PSMAEGradeAdjusted))
    started_t <- sum(!is.na(df_all$PSTaskOrder) & df_all$treatment_arm == 1)
    started_c <- sum(!is.na(df_all$PSTaskOrder) & df_all$treatment_arm == 0)
    data.frame(
      task = "Prediction",
      completed_total = completed_t + completed_c,
      completed_t = completed_t,
      completed_c = completed_c,
      p = prop_p(completed_t, started_t, completed_c, started_c),
      stringsAsFactors = FALSE
    )
  }
)

out_file <- "../writeup/stable/balance_table_small.tex"
con <- file(out_file, "w")

writeLines("\\\\", con)
writeLines("  \\multicolumn{3}{l}{\\emph{Flow from initial allocation into analysis sample}} \\\\", con)
writeLines("  \\hline", con)
writeLines("   && \\emph{Total (N)} & \\emph{Treatment (N)} & \\emph{Control (N)} & \\emph{P-value} & \\\\", con)
writeLines("", con)
writeLines(paste0("  &Total workers allocated &", fmt_n(n_alloc), " & ", fmt_n(n_alloc_t), " & ", fmt_n(n_alloc_c), " &  & \\\\"), con)
writeLines(paste0("  & \\hspace*{0.4cm} $\\hookrightarrow$ began survey &", fmt_n(began), " & ", fmt_n(began_t), " & ", fmt_n(began_c), " & ", fmt_p(p_began), " & \\\\"), con)
writeLines(paste0("  & \\hspace*{0.8cm} $\\hookrightarrow$ completed first task &", fmt_n(task1), " & ", fmt_n(task1_t), " & ", fmt_n(task1_c), " & ", fmt_p(p_task1), " & \\\\"), con)
writeLines(paste0("  & \\hspace*{1.2cm} $\\hookrightarrow$ completed second task &", fmt_n(task2), " & ", fmt_n(task2_t), " & ", fmt_n(task2_c), " & ", fmt_p(p_task2), " & \\\\"), con)
writeLines("\\\\", con)
 writeLines(" \\multicolumn{3}{l}{\\emph{Task completion counts in the main analysis sample}}\\\\", con)
 writeLines(" \\hline", con)
 writeLines(" & & \\emph{Completed (N)} & \\emph{Treatment (N)} & \\emph{Control (N)} & \\emph{P-value} & \\\\", con)
 writeLines("", con)

for (i in seq_len(nrow(panel_b))) {
  r <- panel_b[i, ]
  writeLines(
    paste0(
      " & ", r$task, " task completed & ", fmt_n(r$completed_total), " & ",
      fmt_n(r$completed_t), " & ", fmt_n(r$completed_c), " &  & \\\\"
    ),
    con
  )
  writeLines(paste0(" & \\hspace*{0.4cm} treatment-control test &  &  &  & ", fmt_p(r$p), " & \\\\"), con)
}

writeLines("\\\\", con)
close(con)

message("Wrote: ", out_file)
