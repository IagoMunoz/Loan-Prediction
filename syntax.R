################################################################################
setwd("~/Downloads/github/statistics_project/")
library(dplyr)

# Reading Sampling and Adjusting Database
df = read.csv("loan_approval_dataset.csv")
set.seed(301512)

loan_sample = df[sample(nrow(df), 1000), ]
loan_sample = loan_sample[ , -1]

loan_sample$education = as.character(loan_sample$education)
loan_sample$self_employed = as.character(loan_sample$self_employed)

loan_sample$loan_status = trimws(loan_sample$loan_status)
loan_sample$education = trimws(loan_sample$education)
loan_sample$self_employed = trimws(loan_sample$self_employed)

loan_sample$loan_status = dplyr::recode(loan_sample$loan_status,
                                        "Approved"="1","Rejected"="0",
                                        .default = NULL)

loan_sample$loan_status = as.factor(loan_sample$loan_status)
################################################################################
sketch_sample = loan_sample
# First Categorization of 'cibil_score' dividing it in 5 categories
sketch_sample$cibil_cat = cut(sketch_sample$cibil_score,
                            breaks=c(300, 580, 670, 740, 799, 850),
                            labels=c("Poor","Fair","Good", "Great", "Excellent"))

# Model using only 'cibil_cat'
sketch_model = glm(loan_status ~ cibil_cat, family = binomial(), data = sketch_sample)
summary(sketch_model)

# Prediction using only 'cibil_cat'
prob = predict(sketch_model, sketch_sample, type = "response")
prob_sketch_df = cbind(sketch_sample, prob)

# New Odd Rates considering those changes
OR = data.frame(exp(sketch_model$coefficients))
IC = data.frame(exp(confint(sketch_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")

# Second categorization of 'cibil_score'
sketch_sample$cibil_cat = cut(sketch_sample$cibil_score,
                            breaks=c(300, 579, 740, 900),
                            labels=c("Poor","Good", "Great"))

# Redoing the Model using only 'cibil_cat'
sketch_model = glm(loan_status ~ cibil_cat, family = binomial(), data = sketch_sample)
summary(sketch_model)

# Redoing the Prediction using only 'cibil_cat'
prob = predict(sketch_model, sketch_sample, type = "response")
prob_sketch_df = cbind(sketch_sample, prob)

# More New Odd Rates considering those previous changes
OR = data.frame(exp(sketch_model$coefficients))
IC = data.frame(exp(confint(sketch_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
print(IC_OR)
################################################################################
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

# Selecting Variables using Forward method
forward_model = step(m0, scope = scope_model, direction = "forward")
summary(forward_model)

# Odd Rates - Forward
OR = data.frame(exp(forward_model$coefficients))
IC = data.frame(exp(confint(forward_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")

# Forward Selected: cibil_score | loan_term | commercial_assets_value
# Although, due to it having a Confidence Interval 
# that includes 1, 'commercial_assets_value' isn't statistically significant.
################################################################################
# Third Categorization
loan_sample$cibil_cat = cut(loan_sample$cibil_score,
                            breaks=c(300, 579, 740, 900),
                            labels=c("Poor","Good", "Great"))

# Based on variables selected by Forward Method we create a new model with "cibil_cat"
new_fwr_model = glm(loan_status ~ cibil_cat + loan_term + 
                      commercial_assets_value , data = loan_sample,
                       family = binomial)

# Prediction using forward-selected variables alongside categorized "cibil_score"
prob = predict(new_fwr_model, loan_sample, type = "response")
prob_fwr_df = cbind(loan_sample, prob)

# New Odd Rates considering those changes
OR = data.frame(exp(new_fwr_model$coefficients))
IC = data.frame(exp(confint(new_fwr_model)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
################################################################################
# Making Predictions for one single point
newdf = data.frame(cibil_cat="Good", loan_term=4, commercial_assets_value=3900000)
predict(new_fwr_model, newdf, type = "response")
################################################################################
