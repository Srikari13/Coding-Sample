# Honors Thesis: R Code Sample
# Author: Srikari Tadikonda
# Description: Analysis and visualization of data on children's negotiation behavior.

# Load libraries
library(tidyverse)
library(car)
library(effects)
library(sjPlot)
library(lmerTest)
library(lme4)
library(emmeans)
library(ggplot2)
library(dplyr)
library(lubridate)
library(MASS)

# Load data
askperf <- read.csv("/Users/srikari/Downloads/AskPerf - REAL DATA_January 9, 2025_07.55.csv")

# Data cleaning
askperf <- askperf[-c(1, 2), ]  # Remove metadata rows

# Remove participants who failed checks or were excluded
DNUs <- c("10E", "11E", "13C", "24E", "25E", "31E", "32E", "39C", "51C", "68E", "71C", "75C", 
          "85C", "90E", "92E", "94E", "122E", "124E", "140E", "141C", "143C", "153C", "154E", 
          "155E", "169C", "180E", "200E", "202E", "210E", "211E", "219C", "239C")
cleaned_askperf <- subset(askperf, !CB %in% DNUs)

# Remove irrelevant columns and empty rows
cleaned_askperf <- cleaned_askperf[, -c(1:17, 27:108)]
cleaned_askperf <- cleaned_askperf[rowSums(is.na(cleaned_askperf)) != ncol(cleaned_askperf), ]

# Concatenate columns and create new single columns
cleaned_askperf$smiley     <- paste(cleaned_askperf$smiley.b, cleaned_askperf$smiley.g, sep = "")
cleaned_askperf$othersask  <- paste(cleaned_askperf$othersask.b, cleaned_askperf$othersask.g, sep = "")
cleaned_askperf$ns1        <- as.numeric(paste(cleaned_askperf$ns1.b, cleaned_askperf$ns1.g, sep = ""))
cleaned_askperf$ns2        <- as.numeric(paste(cleaned_askperf$ns2.b, cleaned_askperf$ns2.g, sep = ""))
cleaned_askperf$ns3        <- as.numeric(paste(cleaned_askperf$ns3.b, cleaned_askperf$ns3.g, sep = ""))
cleaned_askperf$ns.avg     <- rowMeans(cleaned_askperf[, c("ns1", "ns2", "ns3")], na.rm = TRUE)
cleaned_askperf            <- cleaned_askperf[!is.nan(cleaned_askperf$ns.avg), ]

# Calculate age
cleaned_askperf <- cleaned_askperf[cleaned_askperf$gender != 2, ]
cleaned_askperf$age <- as.numeric(cleaned_askperf$years)
cleaned_askperf$DOT <- mdy(cleaned_askperf$DOT)
cleaned_askperf$DOB <- mdy(cleaned_askperf$DOB)
cleaned_askperf$true_age <- as.numeric(interval(cleaned_askperf$DOB, cleaned_askperf$DOT) / years(1))

# Recode and create new variables
cleaned_askperf$experimental <- ifelse(cleaned_askperf$condition == 'experimental', 1, 0)
cleaned_askperf$gender <- as.numeric(cleaned_askperf$gender)
cleaned_askperf$ask <- as.numeric(cleaned_askperf$ask)

# Mean-centering
cleaned_askperf$gender_c       <- cleaned_askperf$gender - mean(cleaned_askperf$gender)
cleaned_askperf$experimental_c <- cleaned_askperf$experimental - mean(cleaned_askperf$experimental)

# Main regression analysis
fit.4 <- lm(ask ~ gender_c * experimental_c + like_pictures, data = cleaned_askperf)
summary(fit.4)
car::Anova(fit.4)
emmeans(fit.4, ~gender_c | experimental_c)

# Ordinal regression
cleaned_askperf$ask_olr <- as.ordered(cleaned_askperf$ask)
fit.ord <- polr(ask_olr ~ gender_c * experimental_c + like_pictures + true_age, data = cleaned_askperf, Hess = TRUE)
summary(fit.ord)
car::Anova(fit.ord, type = 2)

# Visualization of means
summary_data <- cleaned_askperf %>%
  group_by(gender, experimental) %>%
  summarise(mean_ask = mean(as.numeric(ask), na.rm = TRUE),
            se_ask   = sd(as.numeric(ask), na.rm = TRUE) / sqrt(n()))

# Generate plot
ggplot() +
  geom_jitter(data = cleaned_askperf, aes(x = factor(experimental, labels = c("No Feedback", "Positive Feedback")),
                                          y = ask, color = factor(gender)), width = 0.2, alpha = 0.3, size = 2) +
  geom_point(data = summary_data, aes(x = factor(experimental, labels = c("No Feedback", "Positive Feedback")),
                                      y = mean_ask, color = factor(gender)), size = 4) +
  geom_errorbar(data = summary_data, aes(x = factor(experimental, labels = c("No Feedback", "Positive Feedback")),
                                         ymin = mean_ask - se_ask, ymax = mean_ask + se_ask, color = factor(gender)),
                width = 0.2) +
  labs(x = "Condition", y = "Number of Pictures Requested", color = "Gender") +
  scale_color_manual(values = c("0" = "#3D8EB9", "1" = "lightcoral"), labels = c("boy", "girl")) +
  theme_minimal()

# Additional regressions
fit.6 <- lm(othersask ~ gender_c * experimental_c, data = cleaned_askperf)
fit.7 <- lm(ns.avg     ~ gender_c * experimental_c, data = cleaned_askperf)
fit.8 <- lm(smiley     ~ gender_c * experimental_c, data = cleaned_askperf)
summary(fit.6); summary(fit.7); summary(fit.8)

##Individual data points for graph background
askperf_stereotypes_graph <- cleaned_askperf[c("CB", "gender", "smiley", "othersask", "ns.avg")] %>%
  gather(stereotype, fit, smiley:ns.avg) %>%
  mutate(
    stereotype = recode(
      stereotype,
      "smiley" = "Task Performance",
      "othersask" = "Task Negotiation",
      "ns.avg" = "General Negotiation"
    ),
    fit = as.numeric(fit)
  )

## Plot 2: Stereotype perception by gender
plot_data <- data.frame(
  stereotype = rep(c("Task Performance", "Task Negotiation", "General Negotiation"), each = 2),
  gender = c("boy", "girl", "boy", "girl", "boy", "girl"),
  fit = c(-0.273, 0.338, -0.097, -0.023, -0.529, -0.220),
  lower.CL = c(-0.393, 0.218, -0.277, -0.204, -0.644, -0.335),
  upper.CL = c(-0.153, 0.458, 0.084, 0.157, -0.414, -0.105)
)

c <- ggplot(plot_data, aes(x = stereotype, y = fit, color = gender, group = gender)) +
  geom_jitter(data = askperf_stereotypes_graph, aes(x = stereotype, y = fit, fill = gender), width = 0.3, alpha = 0.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, color = "black") +
  labs(x = "Stereotype Dimension", y = "Estimated Mean") +
  scale_color_manual(values = c("boy" = "#3D8EB9", "girl" = "lightcoral")) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  theme_minimal()
print(c)
ggsave(c, filename = "stereotypes.png", width = 8, height = 6, dpi = 300)