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

# Applying Binary Logistic Regression
# Null Model
loan_sample$loan_status = as.numeric(loan_sample$loan_status)
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

# Selecting Variables using Forward method
forward_model = step(m0, scope = scope_model, direction = "forward")
summary(forward_model)

# Selecting Variables using Step-Wise method
stepwise_model = step(m1,direction = "both")
summary(stepwise_model)

OR = data.frame(exp(forward_model$coefficients))
IC = data.frame(exp(confint(forward_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
print(IC_OR)
# Forward Selected: cibil_score | loan_term | no_of_dependents

# Although, due to it having a Confidence Interval 
# that includes 1, 'no_of_dependents' isn't statistically significant.

OR = data.frame(exp(stepwise_model$coefficients))
IC = data.frame(exp(confint(stepwise_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
print(IC_OR)
# Step-Wise Selected: cibil_score | loan_term | no_of_dependents | loan_amount |
#                      income_annum | luxury_assets_value | bank_asset_value

# Although, due to the following variables having a Confidence Interval that 
# includes 1, 'no_of_dependents'; 'luxury_assets_value'; 'bank_asset_value' aren't
# statistically significant

# Making Predictions based on both Methods of Selection
# Forward
novo = data.frame(sexo="Fem", peso=50, tabag="Nao")
predict(modelo, novo, type = "response")
fwr_probs = data.frame(probs = predict(forward_model, type = "response"))
head(fwr_probs)


# Categorization of 'cibil_score'
df$cibil_cat = cut(df$cibil_score,
                    breaks=c(300, 580, 670, 740, 799, 850),
                    labels=c("Poor","Fair","Good", "Great", "Excellent"))

sketch_model = glm(loan_status ~ cibil_cat, family = binomial(), data = df)
summary(sketch_model)

# Prediction using only 'cibil_score'
prob = predict(sketch_model, df, type = "response")
prob_sketch_df = cbind(df, prob)