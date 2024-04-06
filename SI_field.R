#Steps:
#Read the data from the CSV file into R.
#Calculate the average δ13C for each category in the "comment" column.
#Plot the average δ13C for sediment, tube, and larvae.
#Run ANOVA to see if there are significant differences between the groups.

#Install packages. Just do this once
install.packages("tidyverse")

# Load necessary library
library(tidyverse)

# Read the data from the CSV file 
#copy your file path and paste it between the quotes with forward slashes
setwd("")
data <- read.csv("")

# View the first few rows of the data to understand its structure
names(data)


# Plot boxplots of δ13C by comment

SIfield_plot <- ggplot(data, aes(x = type, y = d13C_avg, fill = type)) +
  geom_boxplot() +
  theme_bw(base_size = 20) +
  ylim(-40,-25) +
  labs(title = "field δ13C by type", x = "type", y = "δ13C") +
  scale_fill_manual(values=c("#1ABC9C", "#FFC300", "#FF5733"))  # Optional: Use a nicer color palette

SIfield_plot

ggsave("SIfield_plot.png")

# Ensure d13C and comment are appropriate types
data$d13C_avg <- as.numeric(data$d13C_avg)  # Ensure d13C is numeric
data$type <- as.factor(data$type)  # Ensure comment is a factor

# Run ANOVA
anova_result <- aov(d13C_avg ~ type, data = data)
summary(anova_result)
lm_result <- lm(d13C_avg ~ type, data = data)
summary(lm_result)
TukeyHSD(anova_result)

result <- oneway.test(d13C_avg ~ type, data = data)
summary(result)
