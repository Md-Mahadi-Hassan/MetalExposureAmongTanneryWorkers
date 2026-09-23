# Loading packages
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(patchwork)

# Loading the data set
data <- readxl::read_excel("data/data_tannery.xlsx", sheet = 2)

# Transforming the variables
data <- data %>%
  mutate(
    across(1:13, as.factor),  # Columns 1-13 → factor
    across(14:53, ~ as.numeric(as.character(.x)))  # Force convert with NA warnings
  )

# Checking the conversion
glimpse(data)

# Long Data
data_long <- data %>%
  pivot_longer(cols = 14:53, names_to = "Sample_Type", values_to = "Concentration") %>%
  separate(Sample_Type, into = c("Metal", "Sample"), sep = "_") %>%
  filter(Metal %in% c("Ni", "Cr", "Cd", "Pb"))

# Set metal order
data_long$Metal <- factor(data_long$Metal, levels = c("Ni", "Cr", "Cd", "Pb"))

# Filter urine sample
urine_data <- data_long %>% filter(Sample == "urine")

# Boxplot
urine_plot <- ggplot(urine_data, aes(x = Metal, y = Concentration, fill = Metal)) +
  geom_boxplot(outlier.shape = 1) +
  coord_cartesian(ylim = c(0, 500)) +
  labs(title = "Sample: Urine",
       x = "Metal",
       y = "Concentration (ppm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Filter hair sample
hair_data <- data_long %>% filter(Sample == "hair")

# Boxplot
hair_plot <- ggplot(hair_data, aes(x = Metal, y = Concentration, fill = Metal)) +
  geom_boxplot(outlier.shape = 1) +
  coord_cartesian(ylim = c(0, 45)) +
  labs(title = "Sample: Hair",
       x = "Metal",
       y = "Concentration (ppm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Filter nail sample
nail_data <- data_long %>% filter(Sample == "nail")

# Boxplot
nail_plot <- ggplot(nail_data, aes(x = Metal, y = Concentration, fill = Metal)) +
  geom_boxplot(outlier.shape = 1) +
  coord_cartesian(ylim = c(0, 400)) +
  labs(title = "Sample: Nail",
       x = "Metal",
       y = "Concentration (ppm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Filter dust sample
dust_data <- data_long %>% filter(Sample == "dust")

# Boxplot
dust_plot <- ggplot(dust_data, aes(x = Metal, y = Concentration, fill = Metal)) +
  geom_boxplot(outlier.shape = 1) +
  coord_cartesian(ylim = c(0, 1600)) +
  labs(title = "Sample: Dust",
       x = "Metal",
       y = "Concentration (ppm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Arrange 4 plots in 2x2
combined_plot <- (urine_plot | hair_plot) /
  (nail_plot | dust_plot)

# Display
combined_plot

ggsave("figure/MetalConcinSamples.png", plot = combined_plot, width = 11, height = 6, dpi = 900)


















