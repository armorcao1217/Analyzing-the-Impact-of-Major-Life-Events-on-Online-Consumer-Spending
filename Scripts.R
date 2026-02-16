# Reading CSV Files in R
purch <- read.csv("amazon-purchases.csv")
survey <- read.csv("survey.csv")

# ------------------------------------------------------------------------------
# Data Cleaning
# ------------------------------------------------------------------------------
library(dplyr)

## MANIPULATING THE PURCHASES DATASET
# For purchases data, multiply to get the total price
purch$total_price <- purch$Purchase.Price.Per.Uni * purch$Quantity
# Aggregate total price by Survey.ResponseID and year with Order.Date
purch$Order.Year <- format(as.Date(purch$Order.Date), "%Y")
purch_agg <- aggregate(total_price ~ Survey.ResponseID + Order.Year, 
                       data = purch, 
                       sum, 
                       na.action = na.omit)

# Limit the Order.Year to 2020 to 2022
# Only keep Survey.ResponseID that have data across 2020, 2021, 2022
purch_agg_filt <- purch_agg %>%
  filter(Order.Year %in% c("2020", "2021", "2022")) %>%
  add_count(Survey.ResponseID) %>%  # Count records per ResponseID
  filter(n == 3) %>%  # Must have exactly 3 records (one for each year)
  select(-n)


## MANIPULATING THE SURVEY DATASET
# Select relevant columns from survey data
survey_sel <- survey[, c("Survey.ResponseID", "Q.demos.gender", "Q.demos.state",
                         "Q.demos.income", "Q.amazon.use.hh.size","Q.life.changes")]
# Rename columns for clarity
colnames(survey_sel) <- c("Survey.ResponseID", "Gender", "State", 
                          "Income","Household", "Life.Changes")
# For income variable, recode using midpoints of ranges
survey_sel <- survey_sel %>%
  mutate(Income = recode(Income,
                         "Less than $25,000" = 25000,
                         "$25,000 - $49,999" = 37500,
                         "$50,000 - $74,999" = 62500,
                         "$75,000 - $99,999" = 87500,
                         "$100,000 - $149,999" = 125000,
                         "$150,000 or more" = 175000,
                         "Prefer not to say" = NA_real_
  ))
# Recode Household variable, 
survey_sel$Household[survey_sel$Household == "1 (just me!)"] <- "1"
# Mutate Life.Changes into dummy variables
survey_sel <- survey_sel %>%
  mutate(
    LifeChange_LostJob = as.numeric(grepl("Lost a job", Life.Changes)),
    LifeChange_Divorce = as.numeric(grepl("Divorce", Life.Changes)),
    LifeChange_Moved = as.numeric(grepl("Moved place of residence", Life.Changes)),
    LifeChange_Pregnant = as.numeric(grepl("Became pregnant", Life.Changes)),
    LifeChange_HadChild = as.numeric(grepl("Had a child", Life.Changes))
  )
# Remove the Life.Changes column
survey_sel <- survey_sel %>% select(-Life.Changes)


## MERGING DATASETS
# Merge the cleaned purchases and survey datasets by Survey.ResponseID
data <- merge(purch_agg_filt, survey_sel, by = "Survey.ResponseID")
# For the LifeChange variables, recode 2020 and 2022 as 0
data <- data %>%
  mutate(
    LifeChange_LostJob = ifelse(Order.Year %in% c("2020", "2022"), 0, LifeChange_LostJob),
    LifeChange_Divorce = ifelse(Order.Year %in% c("2020"), 0, LifeChange_Divorce),
    LifeChange_Moved = ifelse(Order.Year %in% c("2020","2022"), 0, LifeChange_Moved),
    LifeChange_Pregnant = ifelse(Order.Year %in% c("2020"), 0, LifeChange_Pregnant),
    LifeChange_HadChild = ifelse(Order.Year %in% c("2020"), 0, LifeChange_HadChild)
  )

# Mutate the spending
data <- data %>%
  mutate(
    log_spend = log(total_price + 1)   # log(Spend_it + 1)
  )

## DESCRIPTIVE STATISTICS AND MISSING VALUES
# Create custom descriptive statistics table
descriptive_stats <- function(df) {
  results <- data.frame(
    Variable = names(df),
    Mean = sapply(df, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 2) else NA),
    SD = sapply(df, function(x) if(is.numeric(x)) round(sd(x, na.rm = TRUE), 2) else NA),
    Missing = sapply(df, function(x) sum(is.na(x))),
    Obs = sapply(df, function(x) length(na.omit(x))))
  return(results)
}
# Generate and display the table
summary_table <- descriptive_stats(data)
# Use stargazer to format the custom table nicely
library(stargazer)
stargazer(summary_table, 
          type = "html",
          title = "Descriptive Statistics with Missing Values",
          summary = FALSE,
          rownames = FALSE,
          digits = 2,
          out = "descriptive_statistics.doc")
# Drop rows with NA values
data <- na.omit(data)


# ------------------------------------------------------------------------------
# Fixed Effects Regression Analysis
# ------------------------------------------------------------------------------
library(fixest)

# Fixed effects model with individual and time fixed effects
fe_model <- feols(
  log_spend~ 
    LifeChange_LostJob +
    LifeChange_Divorce +
    LifeChange_Moved +
    LifeChange_Pregnant +
    LifeChange_HadChild
  | Survey.ResponseID + Order.Year, 
  data = data
)
summary(fe_model)
# Fixed effects model with clustered standard errors at the state level 
fe_model_clus <- feols(
  log_spend ~ 
    LifeChange_LostJob +
    LifeChange_Divorce +
    LifeChange_Moved +
    LifeChange_Pregnant +
    LifeChange_HadChild
  | Survey.ResponseID + Order.Year, 
  data = data,
  cluster = ~State
)
summary(fe_model_clus)

# Little within-state correlation in standard errors
# The results remain robust to different specifications of standard errors

## GRAPHICAL REPRESENTATION OF RESULTS
library(ggplot2)
# Extract results directly from the model
results <- data.frame(
  Life_Event = c("Lost Job", "Divorce", "Moved", "Pregnant", "Had Child"),
  Estimate = coef(fe_model_clus),
  SE = sqrt(diag(vcov(fe_model_clus)))
)

# Calculate confidence intervals
results$CI_lower <- results$Estimate - 1.96 * results$SE
results$CI_upper <- results$Estimate + 1.96 * results$SE

# Create coefficient plot with reverse order
# Reverse the factor levels
results$Life_Event <- factor(results$Life_Event, 
                             levels = rev(c("Lost Job", "Divorce", "Moved", "Pregnant", "Had Child")))
# Create coefficient plot
ggplot(results, aes(x = Estimate, y = Life_Event)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), 
                 height = 0.2, color = "steelblue", linewidth = 1) +
  labs(
    title = "Effects of Life Changes on Spending",
    x = "Effect on Log Spending",
    y = NULL
  ) +
  theme_minimal()
