################################################################################
setwd("~/Downloads/github/statistics_project/")
library(dplyr)

# Reading Sampling and Adjusting Database
df = read.csv("loan_approval_dataset.csv")
set.seed(301512)

df = df[ , -1]

df$education = as.character(df$education)
df$self_employed = as.character(df$self_employed)

df$loan_status = trimws(df$loan_status)
df$education = trimws(df$education)
df$self_employed = trimws(df$self_employed)

df$loan_status = dplyr::recode(df$loan_status,
                                        "Approved"="1","Rejected"="0",
                                        .default = NULL)

df$loan_status = as.factor(df$loan_status)
################################################################################
# Understanding the database
# Making a Correlation Heatmap to better understand the interactions between
# the variables
library(ggplot2)
library(ggcorrplot)
library(dplyr)

ggplot(df, aes(x = loan_status)) +
  geom_bar(fill = c("red", "green")) +
  labs(title = "Distribution", x = "Loan Status", y = "Ammounts") +
  scale_x_discrete(labels = c("Rejected", "Approved")) +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  theme_minimal(base_size = 15)

sketch_df = df

my_labels = c("Poor","Fair","Good", "Great", "Excellent")

sketch_df$cibil_cat = cut(sketch_df$cibil_score,
                              breaks=c(300, 580, 670, 740, 799, 900),
                              labels= my_labels, include.lowest = TRUE, right = TRUE)

ggplot(sketch_df, aes(x = cibil_cat, fill = cibil_cat)) +
  geom_bar() +
  labs(title = "Distribution", x = "Cibil Scores", y = "Amounts") +
  scale_x_discrete(labels = my_labels) +
  scale_y_continuous(breaks = seq(0, 2000, by = 400)) +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue")) +
  theme_minimal(base_size = 15)

numeric_columns = df %>% select_if(is.numeric)

correlation_matrix = cor(numeric_columns, use = "complete.obs")

# Heatmap
ggcorrplot(correlation_matrix, 
           method = "square", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"),
           title = "Correlation Heatmap",
           legend.title = "Correlation") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

df$loan_status = as.character(df$loan_status)
ggplot(df, aes(x = cibil_score, fill = loan_status)) +
  geom_histogram(bins = 20, position = "stack", alpha = 0.7) +
  labs(title = "Distribution of CIBIL Score by Loan Status", x = "CIBIL Score", y = "Ammount")

################################################################################
loan_sample = df[sample(nrow(df), 500), ]
loan_sample$loan_status = as.factor(loan_sample$loan_status)
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

# Don't know if we should keep this
ggplot(loan_sample, aes(x = no_of_dependents, y = loan_term, color = loan_status)) +
  geom_point() +
  labs(title = "Scatter Plot", x = "Numb. of Dependents", y = "Loan Terms") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# Forward Selected: cibil_score | loan_term | no_of_dependents
# Although, due to it having a Confidence Interval 
# that includes 1, 'no_of_dependents' isn't statistically significant.
################################################################################
prob = predict(forward_model, loan_sample, type = "response")
prob_fwr_df = cbind(loan_sample, prob)
################################################################################
# Making Predictions for one single point
newdf = data.frame(cibil_score=600, loan_term=4, no_of_dependents=3)
predict(forward_model, newdf, type = "response")
################################################################################
