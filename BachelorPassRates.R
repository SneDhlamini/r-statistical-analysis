#Sinenhlanhla Dhlamini 
#Bachelor Pass Disparity:IEB or NSC

#null Hypothesis:There is no difference in the probability of achieving a Bachelor pass between candidates who write the Independent Examinations Board (IEB) examinations and those who write the National Senior Certificate (NSC) examinations.
#alternative hypothesis:Candidates who write the Independent Examinations Board (IEB) examinations are more likely to achieve a Bachelor pass than learners who write the National Senior Certificate (NSC) examinations.
install.packages(c("tidyverse", "janitor", "effectsize"))
library(tidyverse)
library(janitor)
library(effectsize)


#loading data (the dataset is simulated)
library(readxl)
data <- read_excel("C:/Users/Student/Downloads/matric_results_simulated.xlsx")
view (data)

#clean column names and fix Data Types 
data <- data %>%
  clean_names() %>%
  mutate(
    bachelor_pass = as.numeric(bachelor_pass),
    distinction_80_plus = as.numeric(distinction_80_plus),
    exam_body = factor(exam_body),
    subject_stream = factor(subject_stream)
  )

str(data)   #check after cleaning


#grouping by exam body
summary_rates <- data %>%
  group_by(exam_body) %>%
  summarise(
    total_students = n(),#counting total number of students 
    bachelor_passes = sum(bachelor_pass, na.rm = TRUE),#summing bachelor passes
    pass_rate = bachelor_passes / total_students, #% of Bachelor passes
    .groups = "drop"
  )

print(summary_rates)
#this shows that IEB has a higher bachelor pass rate than NSC

#visual comparison to show IEB bachelor passes are more than NSC
ggplot(summary_rates, aes(x = exam_body, y = pass_rate, fill = exam_body)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Bachelor Pass Rates by Examination Body",
    x = "Examination Body",
    y = "Bachelor Pass Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# now for hypothesis testing (two proportional Z-test)
prop.test(
  x = summary_rates$bachelor_passes,
  n = summary_rates$total_students,
  alternative = "greater",
  correct = FALSE
)
#there is sufficient evidence to suggest that there is a significant difference in the true proportion of bachelor passes. 
#we do not reject the null hypothesis. IEB candidates have a higher bachelor pass rate thhan NSC candidates


#Chi-Squared test 
bachelor_table <- table(data$exam_body, data$bachelor_pass)

chisq.test(bachelor_table)

cramers_v(bachelor_table)
#

#distinction rate comparisons(80%+) by Exam Body 
distinction_rates <- data %>%
  group_by(exam_body) %>%
  summarise(
    distinctions = sum(distinction_80_plus, na.rm = TRUE),
    rate = distinctions / n(),
    .groups = "drop"
  )
print(distinction_rates)
prop.test(
  x = distinction_rates$distinctions,
  n = table(data$exam_body),
  alternative = "greater",
  correct = FALSE
)
#IEB distinction rates are higher than that of NSC. this suggests that IEB candidates not onlypass more often but also achieve higher distinctions than NSC


#subject stream analysis
#for STEM
stem_data <- data %>% filter(subject_stream == "STEM")

prop.test(
  table(stem_data$exam_body, stem_data$bachelor_pass),
  alternative = "greater"
)
#for Humanities 
hum_data <- data %>% filter(subject_stream == "Humanities")

prop.test(
  table(hum_data$exam_body, hum_data$bachelor_pass),
  alternative = "greater"
)
#

#logistic regression 
# The model estimates the probability of achieving a Bachelor pass
# as a function of examination body and subject stream
data <- data %>%
  mutate(bachelor_pass = factor(bachelor_pass, levels = c(0, 1)))

log_model <- glm(
  bachelor_pass ~ exam_body + subject_stream,
  data = data,
  family = binomial
)

summary(log_model)
# NSC candidates have significantly lower odds than IEB candidates
# Subject stream is not a significant predictor

#These results suggest that systemic and structural differences between examination bodies play a more critical role in Bachelor pass outcomes than subject choice




