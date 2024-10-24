# Clear workspace
rm(list=ls())
gc()

# Required libraries
library(haven)   
library(dplyr)   
library(AER)
library(ggplot2)

# Load the AK91 dataset
getwd()
#setwd("C:/Users/chadi/Dropbox/Applied-Economics")
data <- read_dta("./AK91/Raw/NEW7080.dta")

# Data Prep
data <- data |>
  rename(
    AGE = v1,
    AGEQ = v2,
    EDUC = v4,
    ENOCENT = v5,
    ESOCENT = v6,
    LWKLYWGE = v9,
    MARRIED = v10,
    MIDATL = v11,
    MT = v12,
    NEWENG = v13,
    CENSUS = v16,
    QOB = v18,
    RACE = v19,
    SMSA = v20,
    SOATL = v21,
    WNOCENT = v24,
    WSOCENT = v25,
    YOB = v27
  )

data <- data |>
  select(-v8)

data$COHORT <- 20.29
data <- data |>
  mutate(
    COHORT = ifelse(YOB >= 30 & YOB <= 39, 30.39, COHORT),  # Replace where YOB is between 30 and 39
    COHORT = ifelse(YOB >= 40 & YOB <= 49, 40.49, COHORT),  # Replace where YOB is between 40 and 49
    AGEQ = ifelse(CENSUS == 80, AGEQ - 1900, AGEQ),
    AGEQSQ = AGEQ * AGEQ
  )

for (i in 20:29) {
  col_name <- paste0("YR", i)
  data[[col_name]] <- ifelse(data$YOB == (1900 + i) | data$YOB == (10 + i) | data$YOB == (20 + i), 1, 0)
}

for (i in 1:4) {
  col_name <- paste0("QTR", i)
  data[[col_name]] <- ifelse(data$QOB == i, 1, 0)
}

for (q in 1:4) {
  for (y in 20:29) {
    interaction_col <- paste0("QTR", q, y)
    data[[interaction_col]] <- data[[paste0("QTR", q)]] * data[[paste0("YR", y)]]
  }
}

summary(data$COHORT)
names(data)

# Save the new dataset as Rdata
save(data, file = "AK91/Table_data.Rdata")

# Define control variables and instrumental variables
controls <- c("RACE", "MARRIED", "SMSA", "NEWENG", "MIDATL", "ENOCENT", "WNOCENT", "SOATL", "ESOCENT", "WSOCENT", "MT")
year_vars <- paste0("YR", 20:28)
qtr_vars <- paste0("QTR", rep(1:4, each = 10), rep(20:29, times = 4))

# Function to create formulas and run OLS and IV regressions
run_regressions <- function(filtered_data, cohort_name) {

  # Create formulas for OLS and IV regressions
  base_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(year_vars, collapse = " + "))
)
control_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(year_vars, collapse = " + "), "+", 
        paste(controls, collapse = " + "))
)
ageq_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(year_vars, collapse = " + "), 
        "+ AGEQ + AGEQSQ")
)
control_ageq_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(year_vars, collapse = " + "), 
        "+ AGEQ + AGEQSQ +", paste(controls, collapse = " + "))
)
 iv_base_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(year_vars, collapse = " + "), "|",
        paste(c(year_vars, qtr_vars), collapse = " + "))
)
iv_control_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(c(year_vars, controls), collapse = " + "), "|",
        paste(c(year_vars, controls, qtr_vars), collapse = " + "))
)
iv_ageq_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(c(year_vars, "AGEQ", "AGEQSQ"), collapse = " + "), 
        "|", paste(c("AGEQ", "AGEQSQ", year_vars, qtr_vars), collapse = " + "))
)
iv_control_ageq_formula <- as.formula(
  paste("LWKLYWGE ~ EDUC +", paste(c(year_vars, controls, "AGEQ", "AGEQSQ"), collapse = " + "), 
        "|", paste(c("AGEQ", "AGEQSQ", year_vars, controls, qtr_vars), collapse = " + "))
)

  # OLS regressions
  ols1 <- lm(base_formula, data = filtered_data)
  ols2 <- lm(ageq_formula, data = filtered_data)
  ols3 <- lm(control_formula, data = filtered_data)
  ols4 <- lm(control_ageq_formula, data = filtered_data)

  # IV regressions
  iv1 <- ivreg(iv_base_formula, data = filtered_data)
  iv2 <- ivreg(iv_ageq_formula, data = filtered_data)
  iv3 <- ivreg(iv_control_formula, data = filtered_data)
  iv4 <- ivreg(iv_control_ageq_formula, data = filtered_data)

  # Save the results to a log file
  log_file <- paste0(cohort_name, ".txt")
  cat("OLS Regressions\n", file = log_file)
  capture.output(summary(ols1), file = log_file, append = TRUE)
  capture.output(summary(ols2), file = log_file, append = TRUE)
  capture.output(summary(ols3), file = log_file, append = TRUE)
  capture.output(summary(ols4), file = log_file, append = TRUE)
  cat("\nIV Regressions\n", file = log_file, append = TRUE)
  capture.output(summary(iv1), file = log_file, append = TRUE)
  capture.output(summary(iv2), file = log_file, append = TRUE)
  capture.output(summary(iv3), file = log_file, append = TRUE)
  capture.output(summary(iv4), file = log_file, append = TRUE)
}

# Table IV: Filter for COHORT < 20.30 and run regressions
data |>
  filter(COHORT < 20.30) |>
  run_regressions("AK91/TableIV")

# Table V: Filter for 30.00 < COHORT < 30.40 and run regressions
data |>
  filter(COHORT > 30.00 & COHORT < 30.40) |>
  run_regressions("AK91/TableV")

# Table VI: Filter for COHORT > 40.00 and run regressions
data |>
  filter(COHORT > 40.00) |>
  run_regressions("AK91/TableVI")


# Figure V
  filtered_data <- data |>
  filter(YOB >= 30 & YOB <50)

  plot_data <- filtered_data |>
  group_by(YOB, QOB) |>
  summarize(wages = mean(LWKLYWGE, na.rm = TRUE))

  ggplot(plot_data, aes(x = YOB, y = wages, group = QOB, shape = factor(QOB))) +
    geom_line() +
    geom_point(size = 3) +
    scale_shape_manual(values = c(16, 17, 18, 15),
                       labels = c("Q1", "Q2", "Q3", "Q4")) +
    labs(
      title = "Mean Log Weekly Wage by Year and Quarter of Birth",
      x = "Year of Birth",
      y = "Log Weekly Earnings",
      shape = "Quarter of Birth"
    ) +
    scale_x_continuous(breaks = seq(30, 49, by = 1), limits = c(30, 49)) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5)
    )

# Save the plot
ggsave("AK91/FigureV.pdf")


