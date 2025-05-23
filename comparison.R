library(readxl)
library(tidyverse)
library(lme4)
library(ggplot2)

data <- read_excel("/Users/admin/Downloads/Volga_beh.xlsx")
str(data)
summary(data)


#normality check
shapiro.test(data$HE.HR)
shapiro.test(data$HE.LR)
shapiro.test(data$LE.HR)
shapiro.test(data$LE.LR)
shapiro.test(data$CRT)
shapiro.test(data$NFCS)
shapiro.test(data$CS)

#Friedman test
friedman.test(as.matrix(data[2:5]))

#Statistical tests
p_values <- c(
  t.test(data$HE.HR, data$LE.LR, paired = TRUE)$p.value,
  wilcox.test(data$HE.HR, data$HE.LR, paired = TRUE)$p.value,
  t.test(data$LE.HR, data$LE.LR, paired = TRUE)$p.value,
  wilcox.test(data$LE.HR, data$HE.LR, paired = TRUE)$p.value
)
print(p_values)

# Bonferroni correction
p.adjusted <- p.adjust(p_values, method = "bonferroni")
print(p.adjusted)


# Regression (FAIL)
# First, let's fix the data structure
long_data <- data %>%
  # Only use the experiment conditions in the pivot_longer
  pivot_longer(cols = c("HE.HR", "HE.LR", 
                        "LE.HR", "LE.LR"), 
               names_to = "condition", 
               values_to = "result") %>%
  # Add separate factors for expertise and rating
  mutate(
    expertise = ifelse(grepl("High Expertise", condition), "High", "Low"),
    rating = ifelse(grepl("High Rating", condition), "High", "Low"),
    # Convert factors
    expertise = factor(expertise),
    rating = factor(rating),
    # Ensure participant IDs are factors
    participants = factor(participants)
  )


# Now fit a more appropriate model
model2 <- lmer(result ~ expertise * rating + CRT + NFCS + CS + (1 | participants), 
               data = long_data)
summary(model2)

#Regression (NEW)
data$participants <- as.factor(data$participants)
sum(is.na(data))

long_data <- data %>%
  pivot_longer(
    cols = c(HE.HR, HE.LR, LE.HR, LE.LR),
    names_to = "condition", 
    values_to = "opinion_change"
  )

long_data <- long_data %>%
  mutate(
    expertise = case_when(
      grepl("HE", condition) ~ "High",
      grepl("LE", condition) ~ "Low",
      TRUE ~ NA_character_
    ),
    rating = case_when(
      grepl("HR", condition) ~ "High",
      grepl("LR", condition) ~ "Low",
      TRUE ~ NA_character_
    )
  )

long_data$expertise <- factor(long_data$expertise)
long_data$rating <- factor(long_data$rating)

long_data <- long_data %>%
  mutate(
    CRT_c = scale(CRT, center = TRUE, scale = TRUE)[,1],
    NFCS_c = scale(NFCS, center = TRUE, scale = TRUE)[,1],
    CS_c = scale(CS, center = TRUE, scale = TRUE)[,1]
  )

head(long_data)

# Model 1: Basic experimental factors only
model1 <- lmer(opinion_change ~ expertise * rating + (1|participants), 
               data = long_data)
summary(model1)

# Model 2: Add individual difference measures
model2 <- lmer(opinion_change ~ expertise * rating + CRT_c + NFCS_c + CS_c + 
                 (1|participants), data = long_data)
summary(model2)

coef_table <- summary(model2)$coefficients

results_table <- data.frame(
  Predictor = c("Intercept", 
                "Expertise: Low (vs High)", 
                "Rating: Low (vs High)", 
                "Cognitive Reflection", 
                "Need for Cognition", 
                "Conformity", 
                "Expertise × Rating"),
  Estimate = round(coef_table[, "Estimate"], 3),
  SE = round(coef_table[, "Std. Error"], 3),
  t = round(coef_table[, "t value"], 3)
)

results_table$p = round(2 * (1 - pt(abs(results_table$t), df = nobs(model2) - nrow(coef_table))), 3)

# Add significance indicators
results_table$Sig = ifelse(results_table$p < 0.001, "***",
                           ifelse(results_table$p < 0.01, "**",
                                  ifelse(results_table$p < 0.05, "*",
                                         ifelse(results_table$p < 0.1, ".", ""))))

# Format p-values for better display
results_table$p = ifelse(results_table$p < 0.001, "<0.001", as.character(results_table$p))

# Print the table in a way that can be easily copied to Excel or other programs
write.csv(results_table, "model2_Volga_table.csv", row.names = FALSE)

# Also display in the console
print(results_table, row.names = FALSE)

install.packages("MASS")
install.packages("MASS", repos = "https://cloud.r-project.org/")

agg_data <- long_data %>%
  group_by(participants, expertise, rating) %>%
  summarise(
    mean_opinion_change = mean(opinion_change, na.rm = TRUE),
    CRT_c = mean(CRT_c),
    NFCS_c = mean(NFCS_c),
    CS_c = mean(CS_c),
    .groups = "drop"
  )

robust_reg <- rlm(mean_opinion_change ~ expertise * rating + CRT_c + NFCS_c + CS_c, 
                  data = agg_data, method = "M")
summary(robust_reg)



library(dplyr)

results <- data.frame(
  Predictor = c("Intercept", 
                "Expertise: Low (vs High)", 
                "Rating: Low (vs High)", 
                "Cognitive Reflection", 
                "Need for Cognition", 
                "Conformity", 
                "Expertise × Rating"),
  Estimate = c(0.7763, -0.2012, -0.3547, 0.1024, -0.0714, 0.0814, 0.1674),
  SE = c(0.0648, 0.0916, 0.0916, 0.0337, 0.0349, 0.0338, 0.1296),
  t = c(11.9815, -2.1960, -3.8707, 3.0352, -2.0446, 2.4085, 1.2921)
)

# Calculate p-values (2-tailed t-test)
results$p <- 2 * pt(-abs(results$t), df = 97)

# Format p-values
results$p_formatted <- ifelse(results$p < 0.001, "<0.001", 
                              ifelse(results$p < 0.01, round(results$p, 3),
                                     round(results$p, 3)))

# Add significance stars
results$Significance <- ifelse(results$p < 0.001, "***",
                               ifelse(results$p < 0.01, "**",
                                      ifelse(results$p < 0.05, "*",
                                             ifelse(results$p < 0.1, ".", ""))))

# Round numeric values for better display
results$Estimate <- round(results$Estimate, 3)
results$SE <- round(results$SE, 3)
results$t <- round(results$t, 3)

# Create final table with selected columns for presentation
final_table <- results %>%
  select(Predictor, Estimate, SE, t, p_formatted, Significance) %>%
  rename(p = p_formatted)

# Display the table
print(final_table)

# Export to CSV for easy use in presentations
write.csv(final_table, "robust_regression_Volga.csv", row.names = FALSE)

# Model 3: With interactions between expertise and individual differences
model3 <- lmer(opinion_change ~ expertise * rating + 
                 expertise:CRT_c + expertise:NFCS_c + expertise:CS_c + 
                 CRT_c + NFCS_c + CS_c + 
                 (1|participants), data = long_data)
summary(model3)

# Model 4: With interactions between rating and individual differences
model4 <- lmer(opinion_change ~ expertise * rating + 
                 rating:CRT_c + rating:NFCS_c + rating:CS_c + 
                 CRT_c + NFCS_c + CS_c + 
                 (1|participants), data = long_data)
summary(model4)

# Compare models
anova(model1, model2, model3, model4)

cor(long_data[, c("CRT_c", "NFCS_c", "CS_c")])

final_model <- model2
summary(final_model)

confint(final_model)

residuals <- residuals(final_model)
hist(residuals, main="Histogram of Residuals", xlab="Residuals")
qqnorm(residuals); qqline(residuals)

shapiro.test(residuals)

# Basic plots
hist(residuals(model2))
qqnorm(residuals(model2)); qqline(residuals(model2))

# More detailed plots with ggplot2
library(ggplot2)

# Residuals vs fitted values
plot_data <- data.frame(
  fitted = fitted(model2),
  residuals = residuals(model2)
)

ggplot(plot_data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values")


names(long_data)
#GGplot2
ggplot(long_data, aes(x = condition, y = result, fill = condition)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Dependency of opinion change from doctor's status",
       x = "Expertise and Rating of the Doctor",
       y = "Difference between responses") +
  scale_fill_brewer(palette = "Set2")


#True-false incompability
data <- read_excel("output2.xlsx", sheet = 'LE.HR')
shapiro.test(data$Обман)
shapiro.test(data$Правда)
t.test(-data$Правда, data$Обман)



