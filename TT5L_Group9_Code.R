library(readxl)
library(writexl)
library(ggplot2)
data <- read_xlsx("C:/Users/terzh/Downloads/1211103705_STATISTICAL DATA ANALYSIS/ASSIGNMENT 2/dataset.xlsx")

#------------------------------------ Question 1 ------------------------------------
#------------------------------------Question (a)------------------------------------
reaction_time_s <- data$`Reaction Time (ms)` / 1000 # Convert reaction time from milliseconds to seconds
mean_reaction_time <- mean(reaction_time_s)
std_reaction_time <- sd(reaction_time_s)
n_samples <- length(reaction_time_s)

# Calculate 95% confidence interval
alpha <- 0.05
z_score <- qnorm(1 - alpha / 2)
margin_error <- z_score * (std_reaction_time / sqrt(n_samples))
confidence_interval <- c(mean_reaction_time - margin_error, mean_reaction_time + margin_error)

# Print the results
print(paste("Mean reaction time: ", mean_reaction_time))
print(paste("95% Confidence Interval: ", confidence_interval[1], " to ", confidence_interval[2]))

#------------------------------------Question (b)------------------------------------

researcher_claim <- 0.28

if(researcher_claim >= confidence_interval[1] && researcher_claim <= confidence_interval[2]) {
  justification <- "Agree, The researcher's claim is supported by our sample data, as it falls within our calculated 95% confidence interval."
} else {
  justification <- "Disagree, The researcher's claim is not supported by our sample data, as it does not fall within our calculated 95% confidence interval."
}

cat("Justification: ", justification, "\n")

#------------------------------------Question (c)------------------------------------

E <- 0.05 # Margin of error
alpha <- 0.10 # for 90% confidence
z_score_90 <- qnorm(1 - alpha / 2)
required_n <- (z_score_90 * std_reaction_time / E)^2

cat("Required sample size for 90% confidence with a margin of error of 0.05 seconds: ", ceiling(required_n), "\n")

#------------------------------------ Question 2 ------------------------------------
#------------------------------------Question (a)------------------------------------
left_stats <- summary(data$`Reaction Time (ms)`[data$`Hand Dominance` == "Left"])
right_stats <- summary(data$`Reaction Time (ms)`[data$`Hand Dominance` == "Right"])
data$`Hand Dominance` <- as.factor(data$`Hand Dominance`)
data$`Reaction Time (ms)` <- as.numeric(as.character(data$`Reaction Time (ms)`))

# Create the plot
ggplot(data, aes(x = `Hand Dominance`, y = `Reaction Time (ms)`, fill = `Hand Dominance`)) +
  geom_boxplot() +
  labs(title = "Reaction Time by Hand Dominance", x = "Hand Dominance", y = "Reaction Time (ms)") +
  theme_minimal()

print(left_stats)
print(right_stats)

#------------------------------------Question (b)------------------------------------
# Calculate variances for both groups
var_left <- var(data$`Reaction Time (ms)`[data$`Hand Dominance` == "Left"])
var_right <- var(data$`Reaction Time (ms)`[data$`Hand Dominance` == "Right"])

print(var_left)
print(var_right)

# Calculate degrees of freedom for both groups
df_left <- length(data$`Reaction Time (ms)`[data$`Hand Dominance` == "Left"]) - 1
df_right <- length(data$`Reaction Time (ms)`[data$`Hand Dominance` == "Right"]) - 1

print(df_left)
print(df_right)

# Calculate F statistic
f_value <- var_left / var_right
print(f_value)

# Determine the critical values for 97% confidence level
alpha <- 0.03
ci_low <- f_value / qf(1 - alpha / 2, df_left, df_right)
ci_high <- f_value * qf(1 - alpha / 2, df_right, df_left)
print(ci_low)
print(ci_high)

# Print the confidence interval
cat("97% Confidence Interval for the ratio of variances: [", ci_low, ", ", ci_high, "]\n")

#------------------------------------Question (c)------------------------------------
reaction_time_left <- data$`Reaction Time (ms)`[data$`Hand Dominance` == "Left"]
reaction_time_right <- data$`Reaction Time (ms)`[data$`Hand Dominance` == "Right"]

mean_left_hand <- mean(reaction_time_left)
mean_right_hand <- mean(reaction_time_right)

# Calculate variances for each group
var_left_hand <- var(reaction_time_left)
var_right_hand <- var(reaction_time_right)

# Calculate sample sizes
n_left_hand <- length(reaction_time_left)
n_right_hand <- length(reaction_time_right)

print(var_left_hand)
print(var_right_hand)
print(n_left_hand)
print(n_right_hand)
print(mean_left_hand)
print(mean_right_hand)
# Calculate the test statistic (Z-test)
z_statistic <- (mean_left_hand - mean_right_hand) / sqrt((var_left_hand / n_left_hand) + (var_right_hand / n_right_hand))

# Set alpha for 3% significance level
alpha <- 0.03

# Determine the critical value for a two-tailed test
z_critical <- qnorm(1 - alpha / 2)

# Print calculated values
cat("Mean Left Hand:", mean_left_hand, "\n")
cat("Mean Right Hand:", mean_right_hand, "\n")
cat("Z-Statistic:", z_statistic, "\n")
cat("Critical Value:", z_critical, "\n")

# Make a decision
if (abs(z_statistic) > z_critical) {
  cat("Reject the null hypothesis: There is a significant difference in reaction times.\n")
} else {
  cat("Do not reject the null hypothesis: There is no significant difference in reaction times.\n")
}

#------------------------------------ Question 3 ------------------------------------
#------------------------------------Question (a)------------------------------------
# Build the linear regression model
data$`Hand Dominance` <- as.factor(data$`Hand Dominance`)
data$Gender <- as.factor(data$Gender)
data$`Physical Activity Frequency` <- as.factor(data$`Physical Activity Frequency`)

model <- lm(`Reaction Time (ms)` ~ Age + Gender + `Hand Dominance` + `Physical Activity Frequency`, data = data)

summary(model)

#------------------------------------Question (b)------------------------------------
age_coef <- coef(model)["Age"]

# Calculate the change in reaction time for an additional 10 years in age
change_in_reaction_time <- age_coef * 10
cat("Change in reaction time for an additional 10 years in age: ", change_in_reaction_time, " ms\n")
#------------------------------------Question (c)------------------------------------
# Perform ANOVA on the model
anova_result <- anova(model)
anova_result
#------------------------------------Question (d)------------------------------------
# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)
