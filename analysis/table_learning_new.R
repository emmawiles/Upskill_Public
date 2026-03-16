library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)
library(haven)
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


