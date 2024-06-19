setwd("~/Downloads/github/statistics_project/")
library(dplyr)

# Reading Sampling and Adjusting Database
df = read.csv("loan_approval_dataset.csv")
set.seed(301512)

loan_sample = df[sample(nrow(df), 500), ]
loan_sample = loan_sample[ , -1]

loan_sample$education = as.character(loan_sample$education)
loan_sample$self_employed = as.character(loan_sample$self_employed)

loan_sample$loan_status = trimws(loan_sample$loan_status)
loan_sample$education = trimws(loan_sample$education)
loan_sample$self_employed = trimws(loan_sample$self_employed)

loan_sample$loan_status = dplyr::recode(loan_sample$loan_status,
                                        "Approved"="1","Rejected"="0",
                                        .default = NULL)

unique(loan_sample$education)
unique(loan_sample$self_employed)
unique(loan_sample$loan_status)

loan_sample$loan_status = as.numeric(loan_sample$loan_status)
# Applying Binary Logistic Regression
# Null Model
m0 = glm(loan_status ~ 1, data = loan_sample, family = binomial)

#Complete Model
m1 = glm(loan_status ~ ., data = loan_sample, family = binomial)

# Full Scope
scope_model <- list(
  lower = ~1,
  upper = ~ no_of_dependents + education + 
    self_employed + income_annum + 
    loan_amount + loan_term + cibil_score +
    residential_assets_value + 
    commercial_assets_value +
    luxury_assets_value +
    bank_asset_value)

# Selecting Variables using forward method
forward_model = step(m0, scope = scope_model, direction = "forward")
summary(forward_model)

# Selecting Variables using step-wise method
stepwise_model = step(m1,direction = "both")
summary(stepwise_model)

OR = data.frame(exp(forward_model$coefficients))
IC = data.frame(exp(confint(forward_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
print(IC_OR)

OR = data.frame(exp(stepwise_model$coefficients))
IC = data.frame(exp(confint(stepwise_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
print(IC_OR)
