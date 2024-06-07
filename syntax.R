setwd("~/Downloads/github/final")

df = read.csv("loan_approval_dataset.csv")
df = df[ , -1]

set.seed(301512)
loan_sample = df[sample(nrow(df), 500), ]

# Linear Regression Model with every variable

model = lm(loan_status ~  no_of_dependents + education + self_employed + 
           income_annum + loan_amount + loan_term + cibil_score + 
           residential_assets_value + commercial_assets_value + 
           luxury_assets_value + bank_asset_value, 
           data = loan_sample)

summary(model)