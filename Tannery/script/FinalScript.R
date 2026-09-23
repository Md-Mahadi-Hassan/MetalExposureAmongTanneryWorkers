### Importing Packages

library(tidyverse)
library(viridis)
library(gtsummary)
library(gt)
library(ggcorrplot)
library(corrplot)
library(reshape2)
library(patchwork)
library(stringr)
library(officer)
library(flextable)
## Importing Data

data <- readxl::read_xlsx("data/tannery.xlsx", na = c("NA", "", "N/A", "na", "#N/A"))

# Summary of the Data Set

summary(data)

# Converting First 13 Columns to Factors

data[1:13] <- lapply(data[1:13], as.factor)
data[14:29] <- lapply(data[14:29], as.numeric)

# Check structure to confirm ->
str(data)

# Impute using Median for numeric columns

numeric_cols <- names(data)[14:29]

data <- data %>%
  mutate(across(all_of(numeric_cols), ~ if_else(is.na(.), median(., na.rm = TRUE), .)))

## Metal Summary

data |> 
  select(14:29) |> 
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("Mean = {mean}", 
                                     "Median = {median}", 
                                     "SD = {sd}"),
    digits = all_continuous() ~ 2
  ) |> 
  as_gt() |> 
  gtsave("table/MetalSummary.docx")

# ---- Mean Metal Concentration Plot by Samples ----

# Prepare long-format data
metal_data <- data |>
  select(ends_with("_urine"), ends_with("_hair"), ends_with("_nail"), ends_with("_dust")) |>
  pivot_longer(
    cols = everything(),
    names_to = c("Metal", "Sample"),
    names_pattern = "(.*)_(.*)",
    values_to = "Concentration"
  ) |>
  mutate(Sample = recode(Sample,
                         "urine" = "Urine",
                         "hair"  = "Hair",
                         "nail"  = "Nail",
                         "dust"  = "Dust"))

# Summarize mean concentration
metal_summary <- metal_data |>
  group_by(Metal, Sample) |>
  summarise(mean_concentration = mean(Concentration, na.rm = TRUE), .groups = "drop")

# Define custom fill colors
sample_colors <- c("Urine" = "#8dd3c7", "Hair" = "#ffffb3", "Nail" = "#bebada", "Dust" = "#fb8072")

# Plot faceted bar plot
ggplot(metal_summary, aes(x = Sample, y = mean_concentration, fill = Sample)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", linewidth = 0.5) +
  facet_wrap(~Metal, scales = "free_y") +
  scale_fill_manual(values = sample_colors) +
  labs(
    x = "",
    y = "Mean Concentration (ppb)",
    fill = "Sample Type"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12),
    legend.box.background = element_rect(color = "black"),
    legend.key = element_rect(color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)  # ✅ plot border
  )

# Save the Plot

ggsave("figure/MetalConc.jpg", width = 15, height = 8, dpi = 1000)


## ---- Box Plot and Correlation Heat Map ----

metal_long <- data |> 
  select(contains("_urine"), contains("_hair"), contains("_nail"), contains("_dust")) |> 
  pivot_longer(
    cols = everything(),
    names_to = "Metal",
    values_to = "Value"
  ) |> 
  mutate(
    Sample_Type = case_when(
      str_detect(Metal, "_urine") ~ "Urine",
      str_detect(Metal, "_hair")  ~ "Hair",
      str_detect(Metal, "_nail")  ~ "Nail",
      str_detect(Metal, "_dust")  ~ "Dust"
    ),
    Metal = str_remove(Metal, "_.*")
  ) |> 
  filter(Value > 0)  # Remove zeros/negatives for log scale

ggplot(metal_long, aes(x = Sample_Type, y = Value, fill = Sample_Type)) +
  geom_boxplot(outlier.size = 0.8) +
  facet_wrap(~ Metal, scales = "free_y") +
  scale_y_log10() +
  theme_minimal() +
  labs(
    y = "Concentration (log scale)",
    x = "Sample Type"
  ) +
  theme(legend.position = "none")

ggplot(metal_long, aes(x = Sample_Type, y = Value, fill = Sample_Type)) +
  geom_boxplot(outlier.size = 0.8) +
  facet_wrap(~ Metal, scales = "free_y") +
  scale_y_log10() +
  theme_minimal() +
  labs(title = NULL, y = "Concentration (Log scale)", x = "Sample Type") +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 0.7)
  )

# Saving the Plot

ggsave("figure/MetalConcBySamples.jpg", width = 15, height = 8, dpi = 1000)

## ---- Correlation Heat Map ----

# Your plot function with subtitle instead of title
plot_correlation_heatmap <- function(df, sample_type) {
  cor_matrix <- cor(df, use = "complete.obs")
  cor_melt <- melt(cor_matrix)
  
  ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "black", linewidth = 0.3) +  # border around tiles
    geom_text(aes(label = round(value, 2)), size = 3.5, color = "black") +
    scale_fill_viridis_c(option = "D", limits = c(-1, 1), name = "Correlation") +
    theme_minimal(base_family = "Arial") +
    labs(subtitle = paste(sample_type), x = NULL, y = NULL) +
    theme(
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
      axis.text.x = element_text(hjust = 1, colour = "black"),
      axis.text = element_text(size = 10, colour = "black"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      legend.box.background = element_rect(color = "black", size = 0.6),
      legend.key = element_rect(color = "black", size = 0.3)
    )
}

# Prepare your datasets by sample type
urine_data <- data |> select(ends_with("_urine")) |> rename_with(~ str_remove(., "_urine"))
hair_data  <- data |> select(ends_with("_hair"))  |> rename_with(~ str_remove(., "_hair"))
nail_data  <- data |> select(ends_with("_nail"))  |> rename_with(~ str_remove(., "_nail"))
dust_data  <- data |> select(ends_with("_dust"))  |> rename_with(~ str_remove(., "_dust"))

# Generate individual plots
p1 <- plot_correlation_heatmap(urine_data, "Urine")
p2 <- plot_correlation_heatmap(hair_data, "Hair")
p3 <- plot_correlation_heatmap(nail_data, "Nail")
p4 <- plot_correlation_heatmap(dust_data, "Dust")

# Combine with one shared legend, keep individual subtitles visible
combined_plot <- (p1 + p2 + p3 + p4 + plot_layout(guides = "collect")) &
  theme(legend.position = "right")

# View the Plot

combined_plot

## Save the Plot
ggsave("figure/MetalCorrelation.jpg", plot = combined_plot, width = 15, height = 8, dpi = 900)

## ---- ANOVA ----
# Select metal columns (14:29) and Zone
metal_cols <- names(data)[14:29]

# Create empty list to store ANOVA summaries
anova_results <- list()

for (metal in metal_cols) {
  # Create formula dynamically
  formula <- as.formula(paste(metal, "~ Zone"))
  
  # Fit ANOVA model
  model <- aov(formula, data = data)
  
  # Get tidy summary using broom
  tidy_res <- broom::tidy(model)
  
  # Extract the row for 'Zone' (the factor of interest)
  zone_row <- tidy_res %>% filter(term == "Zone")
  
  # Add metal name column
  zone_row <- zone_row %>% mutate(Metal = metal) %>%
    select(Metal, everything())
  
  # Append to list
  anova_results[[metal]] <- zone_row
}

# Combine all into one table
anova_table <- bind_rows(anova_results)

# Arrange columns and show p-value, F-statistic, df, etc.
anova_table <- anova_table %>% 
  select(Metal, df, statistic, p.value)

# Optional: Format p-value nicely
anova_table <- anova_table %>% 
  mutate(p.value = signif(p.value, 3))

# View results
print(anova_table)

## ---- Tukey's Post Hoc ----
tukey_results_sig <- list()

for (metal in significant_metals) {
  formula <- as.formula(paste(metal, "~ Zone"))
  model <- aov(formula, data = data)
  
  tukey <- TukeyHSD(model, "Zone")
  
  tukey_df <- as.data.frame(tukey$Zone) %>%
    rownames_to_column(var = "Comparison") %>%
    mutate(Metal = metal) %>%
    select(Metal, Comparison, everything())
  
  # Use backticks for column with space in name
  tukey_df <- tukey_df %>% filter(`p adj` < 0.05)
  
  if (nrow(tukey_df) > 0) {
    tukey_results_sig[[metal]] <- tukey_df
  }
}

tukey_table_sig <- bind_rows(tukey_results_sig)

print(tukey_table_sig)

## Save the Table
# Example: Assuming your data is in tukey_table_sig
tukey_table_sig <- data.frame(
  Metal = c("Pb_urine", "Pb_urine", "Ni_urine", "Ni_urine", "Ni_hair", "Cd_dust", "Cd_dust", "Cr_dust", "Cr_dust", "Cr_dust"),
  Comparison = c("Wet Blue-Finishing", "Wet Blue-Others", "Wet Blue-Crust", "Wet Blue-Others", "Others-Crust", "Others-Finishing", "Wet Blue-Finishing", "Others-Crust", "Others-Finishing", "Wet Blue-Finishing"),
  diff = c(41.9846930, 42.7627456, 19.0951603, 19.1263114, 2.2561882, -0.5234211, -0.6443816, 743.2768812, 1062.1366316, 598.3922632),
  lwr = c(3.78916620, 4.56721883, 2.05220598, 0.08941525, 0.31445115, -0.97246014, -1.15472131, 330.51047394, 583.46504404, 54.37475616),
  upr = c(80.18021976, 80.95827240, 36.13811453, 38.16320756, 4.19792524, -0.07438196, -0.13404185, 1156.04328854, 1540.80821912, 1142.40977016),
  p_adj = c(2.539379e-02, 2.194105e-02, 2.180828e-02, 4.848541e-02, 1.607125e-02, 1.563250e-02, 7.380156e-03, 5.395979e-05, 6.168981e-07, 2.525929e-02)
)

# Create a flextable from the data
ft <- flextable(tukey_table_sig)

# Optional: Autofit and theme
ft <- autofit(ft)

# Create a Word document and add the table
doc <- read_docx() %>%
  body_add_par("Tukey's Post Hoc Test Significant Results", style = "heading 1") %>%
  body_add_flextable(ft)

# Save the Word document
print(doc, target = "table/Tukey_PostHoc_Significant_Results.docx")

## ---- Summary Table by Zones ----
data |>
  select(Zone, 14:29) |>         # Select grouping variable and metal concentration columns
  tbl_summary(
    by = Zone,                   # Group by 'Zone'
    type = all_continuous() ~ "continuous2",  # Use mean ± SD format
    statistic = all_continuous() ~ "{mean} ± {sd}",  # Format
    digits = all_continuous() ~ 2,               # Round to 2 decimals
    missing = "no"               # Hide missing data count
  ) |>
  add_p(test = all_continuous() ~ "oneway.test") |>      # Add ANOVA p-values
  add_overall() |> 
  as_gt() |> 
  gtsave("table/MetalByZoneANOVA.docx")

### ---- Regression ----

## Relevel the Data
data$Respiratory_problem <- relevel(data$Respiratory_problem, ref = "No")
data$Skin_problem <- relevel(data$Skin_problem, ref = "No")
data$Digestive_problem <- relevel(data$Digestive_problem, ref = "No")
data$HighBp <- relevel(data$HighBp, ref = "No")

# Building the Models

mv1 <- glm(Respiratory_problem ~ Pb_urine + Cd_urine + Cr_urine + Ni_urine + Pb_hair + Cd_hair + Cr_hair + Ni_hair + Pb_nail + Cd_nail + Cr_nail + Ni_nail + Pb_dust + Cd_dust + Cr_dust + Ni_dust,
           data = data, family = binomial(link = "logit"))
mv2 <- glm(Skin_problem ~ Pb_urine + Cd_urine + Cr_urine + Ni_urine + Pb_hair + Cd_hair + Cr_hair + Ni_hair + Pb_nail + Cd_nail + Cr_nail + Ni_nail + Pb_dust + Cd_dust + Cr_dust + Ni_dust,
           data = data, family = binomial(link = "logit"))
mv3 <- glm(Digestive_problem ~ Pb_urine + Cd_urine + Cr_urine + Ni_urine + Pb_hair + Cd_hair + Cr_hair + Ni_hair + Pb_nail + Cd_nail + Cr_nail + Ni_nail + Pb_dust + Cd_dust + Cr_dust + Ni_dust,
           data = data, family = binomial(link = "logit"))
mv4 <- glm(HighBp ~ Pb_urine + Cd_urine + Cr_urine + Ni_urine + Pb_hair + Cd_hair + Cr_hair + Ni_hair + Pb_nail + Cd_nail + Cr_nail + Ni_nail + Pb_dust + Cd_dust + Cr_dust + Ni_dust,
           data = data, family = binomial(link = "logit"))

# Tabular Format
t1 <- mv1 |> 
  tbl_regression(exponentiate = T) |>
  bold_p(t = 0.05)
t2 <- mv2 |> 
  tbl_regression(exponentiate = T) |>
  bold_p(t = 0.05)
t3 <- mv3 |> 
  tbl_regression(exponentiate = T) |>
  bold_p(t = 0.05)
t4 <- mv4 |> 
  tbl_regression(exponentiate = T) |>
  bold_p(t = 0.05)

# Merging Tables

tbl_merge(
  tbls = list(t1, t2, t3, t4),
  tab_spanner = c("Respiratory", "Skin","Digestive","High Blood Pressure")) |>  
  as_gt() |> 
  gtsave("table/LogRegSymptoms.docx")












