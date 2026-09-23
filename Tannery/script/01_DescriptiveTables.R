# Loading Packages
library(tidyverse)
library(gtsummary)
library(gt)

# Loading the data set
data <- readxl::read_excel("data/data_tannery.xlsx", sheet = 2)

# Transforming the variables
data <- data %>%
  mutate(
    across(1:14, as.factor),  # Columns 1-13 → factor
    across(15:54, ~ as.numeric(as.character(.x)))  # Force convert with NA warnings
  )

# Checking the conversion
glimpse(data)

# Table 1: Demographics
data |> 
  select(2:13) |> 
  tbl_summary(
    by = Zone,
    type = all_categorical() ~ "categorical",  # Explicitly define categorical variables
    statistic = list(
      all_categorical() ~ "{n} ({p}%)")) |> 
  add_overall() |> 
  as_gt() |> 
  gtsave("table/Table01Demographics.docx")

# Table 2: Zone wise Difference in Urine Metal
data |> 
  select(Zone, 14:23) |> 
  tbl_summary(by = Zone,
              type = all_continuous() ~ "continuous",  # Explicitly define categorical variables
              statistic = list(
                all_continuous() ~ "{mean} ± {sd}")) |> 
  add_p(
    test = all_continuous() ~ "oneway.test",  # Updated to oneway.test
    test.args = all_continuous() ~ list(var.equal = TRUE),  # Classical ANOVA
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) |> 
  bold_p() |> 
  as_gt() |> 
  gtsave("table/Table02ZoneDiffUrine.docx")

# Table 3: Zone wise difference in Hair metals
data |> 
  select(Zone, 24:33) |> 
  tbl_summary(by = Zone,
              type = all_continuous() ~ "continuous",  # Explicitly define categorical variables
              statistic = list(
                all_continuous() ~ "{mean} ± {sd}")) |> 
  add_p(
    test = all_continuous() ~ "oneway.test",  # Updated to oneway.test
    test.args = all_continuous() ~ list(var.equal = TRUE),  # Classical ANOVA
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) |> 
  bold_p() |> 
  as_gt() |> 
  gtsave("table/Table03ZoneDiffHairs.docx")

# Table 4: Zone wise difference in Nail metals
data |> 
  select(Zone, 34:43) |> 
  tbl_summary(by = Zone,
              type = all_continuous() ~ "continuous",  # Explicitly define categorical variables
              statistic = list(
                all_continuous() ~ "{mean} ± {sd}")) |> 
  add_p(
    test = all_continuous() ~ "oneway.test",  # Updated to oneway.test
    test.args = all_continuous() ~ list(var.equal = TRUE),  # Classical ANOVA
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) |> 
  bold_p() |> 
  as_gt() |> 
  gtsave("table/Table04ZoneDiffNails.docx")

# Table 5: Zone wise difference in Dust Metals
data |> 
  select(Zone, 44:53) |> 
  tbl_summary(by = Zone,
              type = all_continuous() ~ "continuous",  # Explicitly define categorical variables
              statistic = list(
                all_continuous() ~ "{mean} ± {sd}")) |> 
  add_p(
    test = all_continuous() ~ "oneway.test",  # Updated to oneway.test
    test.args = all_continuous() ~ list(var.equal = TRUE),  # Classical ANOVA
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) |> 
  bold_p() |> 
  as_gt() |> 
  gtsave("table/Table05ZoneDiffDust.docx")

# Table 6: Difference in Metal concentrations between Respiratory problems
data$Respiratory_problem <- relevel(data$Respiratory_problem, ref = "No")
data |> 
  mutate(Respiratory_problem = factor(Respiratory_problem, levels = c("No", "Yes"))) |>
  select(Respiratory_problem, 14:53) |> 
  tbl_summary(by = Respiratory_problem) |> 
  add_difference() |> 
  bold_p(t = 0.05)

## ---- Urine Difference Control -----
data |> 
  select(Group, 15:24) |> 
  tbl_summary(by = Group,
              type = all_continuous() ~ "continuous",
              statistic = list(
                all_continuous() ~ "{mean} ± {sd}")) |> 
  add_difference() |> 
  bold_p() |> 
  as_gt() |> 
  gtsave("table/UrineDifferenceCaseControl.docx")

## ----- Boxplots -----
# Select urine metals
urine_data <- data %>%
  select(Group, As_urine, Se_urine, Pb_urine, Cd_urine,
         Co_urine, Cr_urine, Cu_urine, Mn_urine, Ni_urine, Hg_urine)

# Convert to long format
urine_long <- urine_data %>%
  pivot_longer(-Group, names_to = "Metal", values_to = "Concentration")

# Boxplot
ggplot(urine_long, aes(x = Group, y = Concentration, fill = Group)) +
  geom_boxplot() +
  facet_wrap(~Metal, scales = "free") +
  theme_bw() +
  labs(title = "Urine Metal Concentrations: Case vs Control",
       x = "Group",
       y = "Concentration")

## ---- New -----
# Select metals
metals <- data %>%
  select(Group, Ni_urine, Cr_urine, Cd_urine, Pb_urine)

# Convert to long format
metals_long <- metals %>%
  pivot_longer(
    cols = c(Ni_urine, Cr_urine, Cd_urine, Pb_urine),
    names_to = "Metal",
    values_to = "Concentration"
  )

# Set order of metals
metals_long$Metal <- factor(
  metals_long$Metal,
  levels = c("Ni_urine", "Cr_urine", "Cd_urine", "Pb_urine"),
  labels = c("Ni", "Cr", "Cd", "Pb")
)

# Boxplot
UrineCC <- ggplot(metals_long, aes(x = Metal, y = Concentration, fill = Group)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 250)) +
  theme_bw() +
  labs(
    x = "Metal",
    y = "Concentration (ppm)"
  )

# Save
ggsave("figure/UrineCaseControl.png", plot = UrineCC, height = 6, width = 10, dpi = 900)











