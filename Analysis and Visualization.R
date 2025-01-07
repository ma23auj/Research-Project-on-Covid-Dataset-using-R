# Load Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)


epidemic_path <- "C:/Users/M. Hamza Khalid/Desktop/COVID/epidemic"
vaccination_path <- "C:/Users/M. Hamza Khalid/Desktop/COVID/vaccination"

# Load Epidemic Datasets
cases_malaysia <- read_csv(file.path(epidemic_path, "cases_malaysia.csv"))
cases_state <- read_csv(file.path(epidemic_path, "cases_state.csv"))
cases_age <- read_csv(file.path(epidemic_path, "cases_age.csv"))
deaths_malaysia <- read_csv(file.path(epidemic_path, "deaths_malaysia.csv"))
deaths_state <- read_csv(file.path(epidemic_path, "deaths_state.csv"))
deaths_age <- read_csv(file.path(epidemic_path, "deaths_age.csv"))
hospital <- read_csv(file.path(epidemic_path, "hospital.csv"))
icu <- read_csv(file.path(epidemic_path, "icu.csv"))
tests_malaysia <- read_csv(file.path(epidemic_path, "tests_malaysia.csv"))
tests_state <- read_csv(file.path(epidemic_path, "tests_state.csv"))

# Load Vaccination Datasets
vax_malaysia <- read_csv(file.path(vaccination_path, "vax_malaysia.csv"))
vax_snapshot <- read_csv(file.path(vaccination_path, "vax_snapshot.csv"))
vax_demog_age <- read_csv(file.path(vaccination_path, "vax_demog_age.csv"))
vax_demog_sex <- read_csv(file.path(vaccination_path, "vax_demog_sex.csv"))
vax_booster_combos <- read_csv(file.path(vaccination_path, "vax_booster_combos.csv"))
vax_outcomes_capita <- read_csv(file.path(vaccination_path, "vax_outcomes_capita.csv"))
vax_school <- read_csv(file.path(vaccination_path, "vax_school.csv"))
vax_state <- read_csv(file.path(vaccination_path, "vax_state.csv"))


# Ensure the data is sorted by date
cases_malaysia <- cases_malaysia %>%
  arrange(as.Date(date)) %>%
  mutate(cases_total = cumsum(cases_new))

deaths_malaysia <- deaths_malaysia %>%
  arrange(as.Date(date)) %>%
  mutate(deaths_total = cumsum(deaths_new))

# Reshape the dataset for absolute cases
cases_age_long <- cases_age %>%
  select(week, starts_with("abs_")) %>%
  pivot_longer(
    cols = starts_with("abs_"),
    names_to = "age_group",
    values_to = "cases_new"
  ) %>%
  mutate(age_group = gsub("abs_", "", age_group))  # Clean up age group labels

deaths_age_long <- deaths_age %>%
  select(week, starts_with("abs_")) %>%
  pivot_longer(
    cols = starts_with("abs_"),
    names_to = "age_group",
    values_to = "deaths_new"
  ) %>%
  mutate(age_group = gsub("abs_", "", age_group))  # Clean up age group labels


vax_booster_combos <- vax_booster_combos %>%
  mutate(booster_doses_total = rowSums(select(., starts_with("a")), na.rm = TRUE))


# Reshape the dataset
vax_demog_age_long <- vax_demog_age %>%
  pivot_longer(
    cols = starts_with("partial_"):starts_with("booster2_"),
    names_to = c("status", "age_group"),
    names_sep = "_",
    values_to = "count"
  )


vax_school_long <- vax_school %>%
  select(school, dose1_staff, dose2_staff, dose3_staff, dose1_student, dose2_student) %>%
  pivot_longer(
    cols = starts_with("dose"),
    names_to = "vaccination_type",
    values_to = "percentage"
  )

head(vax_school_long)

tests_malaysia <- tests_malaysia %>%
  mutate(total_tests = `rtk-ag` + pcr)


################################################################################


# Visualization 1: Daily COVID-19 Cases and Deaths
ggplot(cases_malaysia, aes(x = as.Date(date))) +
  geom_line(aes(y = cases_new, color = "New Cases")) +
  geom_line(data = deaths_malaysia, aes(y = deaths_new, color = "New Deaths")) +
  labs(title = "Daily COVID-19 Cases and Deaths in Malaysia",
       x = "Date", y = "Count", color = "Legend") +
  theme_minimal()

# Visualization 2: Cumulative Cases and Deaths
ggplot(cases_malaysia, aes(x = as.Date(date), y = cases_total)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative COVID-19 Cases in Malaysia",
       x = "Date", y = "Total Cases") +
  theme_minimal()


ggplot(deaths_malaysia, aes(x = as.Date(date), y = deaths_total)) +
  geom_line(color = "red") +
  labs(title = "Cumulative COVID-19 Deaths in Malaysia",
       x = "Date", y = "Total Deaths") +
  theme_minimal()

# Visualization 3: State-Wise Cases and Deaths
cases_state %>%
  ggplot(aes(x = state, y = cases_new, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "State-Wise New COVID-19 Cases",
       x = "State", y = "New Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

deaths_state %>%
  ggplot(aes(x = state, y = deaths_new, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "State-Wise New COVID-19 Deaths",
       x = "State", y = "New Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization 4: Age-Wise Cases and Deaths
ggplot(cases_age_long, aes(x = age_group, y = cases_new, fill = age_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Age-Wise COVID-19 Cases",
       x = "Age Group", y = "Cases") +
  theme_minimal()

ggplot(deaths_age_long, aes(x = age_group, y = deaths_new, fill = age_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Age-Wise COVID-19 Deaths",
       x = "Age Group", y = "Deaths") +
  theme_minimal()

# Visualization 5: Hospital and ICU Usage
ggplot(hospital, aes(x = as.Date(date), y = beds)) +
  geom_line(color = "purple") +
  labs(title = "Hospital Bed Usage Over Time",
       x = "Date", y = "Beds in Use") +
  theme_minimal()

ggplot(icu, aes(x = as.Date(date), y = beds_icu_total)) +
  geom_line(color = "orange") +
  labs(title = "ICU Bed Usage Over Time",
       x = "Date", y = "Total ICU Beds in Use") +
  theme_minimal()


# Visualization 6: Vaccination Trends
ggplot(vax_malaysia, aes(x = as.Date(date), y = daily_partial)) +
  geom_line(color = "blue") +
  labs(title = "Daily Vaccinations in Malaysia",
       x = "Date", y = "Number of Vaccinations") +
  theme_minimal()

# Visualization 7: Booster Vaccination Trends
ggplot(vax_booster_combos, aes(x = as.Date(date), y = booster_doses_total)) +
  geom_line(color = "darkgreen") +
  labs(title = "Booster Doses Over Time",
       x = "Date", y = "Booster Doses") +
  theme_minimal()

# Visualization 8: Age-Wise Vaccination Distribution
ggplot(vax_demog_age_long, aes(x = age_group, y = count, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vaccination Distribution by Age Group",
       x = "Age Group", y = "Count",
       fill = "Vaccination Status") +
  theme_minimal()

# Visualization 9: School Vaccinations
ggplot(vax_school_long, aes(x = vaccination_type, y = percentage, fill = vaccination_type)) +
  geom_boxplot() +
  labs(title = "School Vaccination Coverage",
       x = "Vaccination Type",
       y = "Percentage Coverage") +
  theme_minimal()

# Visualization 10: Testing Trends in Malaysia
ggplot(tests_malaysia, aes(x = as.Date(date), y = total_tests)) +
  geom_line(color = "green") +
  labs(title = "COVID-19 Total Testing Trends in Malaysia",
       x = "Date", y = "Number of Tests") +
  theme_minimal()


# Visualization 11: Hospital Bed Usage Over Time
ggplot(hospital, aes(x = as.Date(date), y = beds_covid)) +
  geom_line(color = "red") +
  labs(title = "Hospital Beds Used by COVID-19 Patients",
       x = "Date", y = "Beds in Use") +
  theme_minimal()

# Visualization 12: State-Wise ICU Usage
icu %>%
  group_by(state) %>%
  summarise(total_icu_beds = sum(beds_icu_total, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(state, total_icu_beds), y = total_icu_beds, fill = state)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "State-Wise ICU Bed Usage",
       x = "State", y = "Total ICU Beds") +
  theme_minimal()

# Visualization 13: Age-Wise Booster Doses
booster_age <- vax_demog_age %>%
  select(starts_with("booster_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "age_group",
    values_to = "booster_doses"
  ) %>%
  mutate(age_group = gsub("booster_", "", age_group))

ggplot(booster_age, aes(x = age_group, y = booster_doses, fill = age_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Age-Wise Booster Doses",
       x = "Age Group", y = "Booster Doses",
       fill = "Age Group") +
  theme_minimal()

# Visualization 14: State-Level Vaccination Trends
ggplot(vax_state, aes(x = as.Date(date), y = cumul, color = state)) +
  geom_line() +
  labs(title = "State-Level Vaccination Trends",
       x = "Date", y = "Cumulative Vaccinations",
       color = "State") +
  theme_minimal()

# Visualization 15: Comparison of Hospital and ICU Bed Usage
icu_hospital_combined <- hospital %>%
  select(date, beds_hospital = beds_covid) %>%  # Replace `beds_covid` with the correct column
  left_join(icu %>% select(date, beds_icu = beds_icu_total), by = "date")

ggplot(icu_hospital_combined) +
  geom_line(aes(x = as.Date(date), y = beds_hospital, color = "Hospital Beds")) +
  geom_line(aes(x = as.Date(date), y = beds_icu, color = "ICU Beds")) +
  labs(title = "Comparison of Hospital and ICU Bed Usage",
       x = "Date", y = "Number of Beds",
       color = "Type") +
  theme_minimal()

 # Save additional plots
ggsave("daily_cases_deaths.png")
ggsave("cumulative_cases.png")
ggsave("cumulative_deaths.png")
ggsave("state_cases.png")
ggsave("state_deaths.png")
ggsave("age_cases.png")
ggsave("age_deaths.png")
ggsave("hospital_usage.png")
ggsave("icu_usage.png")
ggsave("vaccination_trends.png")
ggsave("booster_trends.png")
ggsave("age_vaccination_distribution.png")
ggsave("school_vaccinations.png")
ggsave("testing_trends.png")
ggsave("hospital_beds_covid.png")
ggsave("state_icu_usage.png")
ggsave("age_booster_doses.png")
ggsave("state_vaccination_trends.png")
ggsave("hospital_vs_icu.png")


# State-wise Analysis: ANOVA for differences in cases across states
state_analysis <- cases_state %>%
  group_by(state) %>%
  summarise(mean_cases = mean(cases_new, na.rm = TRUE))

# Perform ANOVA
anova_result_state <- aov(mean_cases ~ state, data = state_analysis)
summary(anova_result_state)

# Post hoc test using Tukey's Honest Significant Difference Test
tukey_state <- TukeyHSD(anova_result_state)
print(tukey_state)

# Age-wise Analysis: ANOVA for differences in cases across age groups
age_analysis <- cases_age_long %>%
  group_by(age_group) %>%
  summarise(mean_cases = mean(cases_new, na.rm = TRUE))

# Perform ANOVA
anova_result_age <- aov(mean_cases ~ age_group, data = age_analysis)
summary(anova_result_age)

# Post hoc test using Tukey's Honest Significant Difference Test
tukey_age <- TukeyHSD(anova_result_age)
print(tukey_age)

# Visualization 1: State-wise Average COVID-19 Cases
ggplot(state_analysis, aes(x = state, y = mean_cases, fill = state)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "State-wise Average COVID-19 Cases", x = "State", y = "Average Cases")

# Visualization 2: Age-wise Average COVID-19 Cases
ggplot(age_analysis, aes(x = age_group, y = mean_cases, fill = age_group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Age-wise Average COVID-19 Cases", x = "Age Group", y = "Average Cases")

