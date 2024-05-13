
install.packages("RColorBrewer")

library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(glmnet)
library(RColorBrewer)
library(MASS)

#some of the code might seem a bit scattered as i tried multiple types of
#visualizations but opted to focus and cut it down to what was presented.

acs <- read_dta('acs.dta')

acs <- na.omit(acs) #na.omit function for potential NA values. could also use drop.na

acs.age <- acs %>% 
  filter(age >= 18 & age <= 64)

pastel_colors <- brewer.pal(3, "Pastel1")
ggplot(acs.age, aes(x = factor(statefip), fill = factor(ownershp))) +
  geom_bar() +
  labs(title = "Homeownership by State",
       x = "FIPS Code",
       y = "Ownership Count",
       fill = "Status (Owned/Loaned[1] or Rented[2])") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
        axis.title = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(color = "black", size = 12),
        panel.background = element_rect(fill = "white"),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10)) +
  scale_fill_manual(values = pastel_colors)

acs.age1 <- acs.age %>%
  mutate(across(c(metro, sex, marst, race, hispan, school, empstat, classwkr, educ),
                as.factor))

acs.stepwise <- stepAIC(lm(ownershp ~ ., data = acs.age1), direction = "both")

summary(acs.stepwise)


coefficients <- coef(acs.stepwise)[-1]
names(coefficients) <- names(coefficients)[-1]

ordered.coeff <- coefficients[order(abs(coefficients), decreasing = TRUE)]

pastel_colors <- brewer.pal(length(ordered.coeff), "Pastel1")

par(mar = c(5, 10, 4, 2))


coefficient_graph <- barplot(ordered.coeff, horiz = TRUE, col = pastel_colors, main = "Dependent Varable Effectiveness",
        xlab = "Absolute Coefficient Value", ylab = "",
        cex.names = 0.7, las = 1)


coef.1 <- coef(acs.stepwise)

predict_homeownership <- function(state_fips) {
  
  linear_predictor <- sum(coefficients * c(1, state_fips, rep(0, length(coefficients) - 2)))
  
  
  predicted_probability <- exp(linear_predictor) / (1 + exp(linear_predictor))
  
  return(predicted_probability)
}


state_fips_codes <- unique(acs.age1$statefip)


predicted_probabilities <- sapply(state_fips_codes, predict_homeownership)

#data frame for predictions
predicted_df <- data.frame(statefip = state_fips_codes, predicted_probability = predicted_probabilities)

library(ggplot2)
library(maps)


map.data <- map_data("state")

print(predicted_df)

state_names <- data.frame(statefip = unique(predicted_df$statefip),
                          state_name = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"))


predicted_df <- merge(predicted_df, state_names, by = "statefip")


ggplot(predicted_df, aes(x = reorder(state_names, predicted_probability), y = predicted_probability)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = state_name), vjust = 0, size = 3, angle = 90, hjust = 0.5) +
  labs(title = "Predicted Homeownership Probabilities by State",
       x = "State",
       y = "Predicted Probability") +
  theme_minimal()


























































