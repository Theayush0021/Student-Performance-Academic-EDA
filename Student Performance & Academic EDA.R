

# PROJECT: STUDENT PERFORMANCE & ACADEMIC EDA
# LANGUAGE: R
# TOOLS: dplyr, ggplot2, tidyr




# 1. INSTALL PACKAGES


packages <- c("dplyr", "ggplot2", "tidyr")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(dplyr)
library(ggplot2)
library(tidyr)



# 2. CREATE DATASET


students <- data.frame(
  
  Student_ID = 1:15,
  
  Name = c(
    "Aman", "Riya", "Rahul", "Priya", "Karan",
    "Neha", "Arjun", "Sneha", "Vikas", "Anjali",
    "Rohit", "Pooja", "Mohit", "Simran", "Aditya"
  ),
  
  Gender = c(
    "Male", "Female", "Male", "Female", "Male",
    "Female", "Male", "Female", "Male", "Female",
    "Male", "Female", "Male", "Female", "Male"
  ),
  
  Study_Hours = c(
    2, 4, 3, 5, 1,
    6, 4, 7, 2, 5,
    3, 8, 4, 6, 5
  ),
  
  Attendance = c(
    65, 85, 75, 90, 60,
    95, 80, 98, 70, 88,
    72, 96, 82, 94, 87
  ),
  
  Math = c(
    55, 85, 65, 90, 45,
    95, 80, 98, 60, 88,
    70, 96, 75, 92, 85
  ),
  
  Science = c(
    60, 80, 70, 85, 50,
    90, 75, 95, 65, 82,
    68, 98, 78, 94, 88
  ),
  
  English = c(
    65, 88, 72, 92, 55,
    85, 78, 96, 68, 90,
    75, 94, 80, 89, 86
  )
)



# 3. VIEW DATASET


print(students)

# First 5 rows
head(students, 5)

# Last 5 rows
tail(students, 5)

# Dataset structure
str(students)

# Statistical summary
summary(students)

# Dataset dimensions
dim(students)

# Total rows
nrow(students)

# Total columns
ncol(students)

# Column names
names(students)



# 4. DATA CLEANING CHECK


# Missing values in each column
colSums(is.na(students))

# Total missing values
sum(is.na(students))

# Duplicate rows
sum(duplicated(students))

# Remove duplicate rows if present
students <- students %>%
  distinct()



# 5. CREATE NEW COLUMNS


students <- students %>%
  mutate(
    
    # Average of three subjects
    Average_Marks = round(
      (Math + Science + English) / 3,
      2
    ),
    
    # Total marks
    Total_Marks = Math + Science + English,
    
    # Performance category
    Performance = case_when(
      
      Average_Marks >= 90 ~ "Excellent",
      
      Average_Marks >= 75 ~ "Good",
      
      Average_Marks >= 60 ~ "Average",
      
      TRUE ~ "Needs Improvement"
    )
  )


# View updated dataset
print(students)



# 6. BASIC STATISTICS


# Average marks
mean(students$Average_Marks)

# Highest average marks
max(students$Average_Marks)

# Lowest average marks
min(students$Average_Marks)

# Median marks
median(students$Average_Marks)

# Standard deviation
sd(students$Average_Marks)



# 7. TOP AND LOWEST STUDENTS


# Top 5 students
top_students <- students %>%
  arrange(desc(Average_Marks)) %>%
  head(5)

print(top_students)


# Lowest 5 students
lowest_students <- students %>%
  arrange(Average_Marks) %>%
  head(5)

print(lowest_students)


# Highest average marks student
students$Name[
  which.max(students$Average_Marks)
]


# Lowest average marks student
students$Name[
  which.min(students$Average_Marks)
]



# 8. GENDER ANALYSIS


# Gender frequency
gender_count <- table(students$Gender)

print(gender_count)


# Gender percentage
gender_percentage <- round(
  prop.table(gender_count) * 100,
  2
)

print(gender_percentage)


# Gender-wise average marks
gender_average <- aggregate(
  Average_Marks ~ Gender,
  data = students,
  FUN = mean
)

print(gender_average)



# 9. PERFORMANCE ANALYSIS


# Performance frequency
performance_count <- table(
  students$Performance
)

print(performance_count)


# Performance percentage
performance_percentage <- round(
  prop.table(performance_count) * 100,
  2
)

print(performance_percentage)



# 10. SUBJECT ANALYSIS


# Average Math marks
mean(students$Math)

# Average Science marks
mean(students$Science)

# Average English marks
mean(students$English)


# Subject-wise average
subject_average <- data.frame(
  
  Subject = c("Math", "Science", "English"),
  
  Average = c(
    mean(students$Math),
    mean(students$Science),
    mean(students$English)
  )
)

print(subject_average)



# 11. CORRELATION ANALYSIS


# Study hours vs average marks
study_correlation <- cor(
  students$Study_Hours,
  students$Average_Marks
)

print(study_correlation)


# Attendance vs average marks
attendance_correlation <- cor(
  students$Attendance,
  students$Average_Marks
)

print(attendance_correlation)


# Correlation matrix
correlation_matrix <- students %>%
  select(
    Study_Hours,
    Attendance,
    Math,
    Science,
    English,
    Average_Marks
  ) %>%
  cor()

print(correlation_matrix)



# 12. OUTLIER ANALYSIS


# Boxplot
boxplot(
  students$Average_Marks,
  main = "Average Marks Distribution",
  ylab = "Average Marks"
)


# IQR method
Q1 <- quantile(
  students$Average_Marks,
  0.25
)

Q3 <- quantile(
  students$Average_Marks,
  0.75
)

IQR_value <- IQR(
  students$Average_Marks
)

lower_limit <- Q1 - 1.5 * IQR_value

upper_limit <- Q3 + 1.5 * IQR_value


# Identify potential outliers
outliers <- students %>%
  filter(
    Average_Marks < lower_limit |
      Average_Marks > upper_limit
  )

print(outliers)



# 13. DATA VISUALIZATION


# Create plots folder
if (!dir.exists("plots")) {
  dir.create("plots")
}



# A. HISTOGRAM


p1 <- ggplot(
  students,
  aes(x = Average_Marks)
) +
  
  geom_histogram(
    bins = 8,
    fill = "skyblue",
    color = "black"
  ) +
  
  labs(
    title = "Distribution of Average Marks",
    x = "Average Marks",
    y = "Frequency"
  ) +
  
  theme_minimal()

print(p1)



# B. GENDER BAR CHART


p2 <- ggplot(
  students,
  aes(x = Gender)
) +
  
  geom_bar(
    fill = "steelblue"
  ) +
  
  labs(
    title = "Students by Gender",
    x = "Gender",
    y = "Number of Students"
  ) +
  
  theme_minimal()

print(p2)



# C. STUDY HOURS VS MARKS


p3 <- ggplot(
  students,
  aes(
    x = Study_Hours,
    y = Average_Marks
  )
) +
  
  geom_point(
    size = 3
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  
  labs(
    title = "Study Hours vs Average Marks",
    x = "Study Hours",
    y = "Average Marks"
  ) +
  
  theme_minimal()

print(p3)



# D. ATTENDANCE VS MARKS


p4 <- ggplot(
  students,
  aes(
    x = Attendance,
    y = Average_Marks
  )
) +
  
  geom_point(
    size = 3
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  
  labs(
    title = "Attendance vs Average Marks",
    x = "Attendance (%)",
    y = "Average Marks"
  ) +
  
  theme_minimal()

print(p4)



# E. SUBJECT-WISE BOXPLOT


students_long <- students %>%
  
  select(
    Name,
    Math,
    Science,
    English
  ) %>%
  
  pivot_longer(
    
    cols = c(
      Math,
      Science,
      English
    ),
    
    names_to = "Subject",
    
    values_to = "Marks"
  )


p5 <- ggplot(
  students_long,
  aes(
    x = Subject,
    y = Marks
  )
) +
  
  geom_boxplot(
    fill = "lightgreen"
  ) +
  
  labs(
    title = "Subject-wise Marks Distribution",
    x = "Subject",
    y = "Marks"
  ) +
  
  theme_minimal()

print(p5)



# F. SUBJECT-WISE AVERAGE


p6 <- ggplot(
  subject_average,
  aes(
    x = Subject,
    y = Average
  )
) +
  
  geom_col(
    fill = "orange"
  ) +
  
  labs(
    title = "Average Marks by Subject",
    x = "Subject",
    y = "Average Marks"
  ) +
  
  theme_minimal()

print(p6)



# G. PERFORMANCE CATEGORY


p7 <- ggplot(
  students,
  aes(
    x = Performance
  )
) +
  
  geom_bar(
    fill = "purple"
  ) +
  
  labs(
    title = "Student Performance Categories",
    x = "Performance",
    y = "Number of Students"
  ) +
  
  theme_minimal()

print(p7)


# 14. SAVE GRAPHS

ggsave(
  "plots/average_marks_histogram.png",
  plot = p1,
  width = 8,
  height = 5
)


ggsave(
  "plots/gender_bar_chart.png",
  plot = p2,
  width = 8,
  height = 5
)


ggsave(
  "plots/study_hours_vs_marks.png",
  plot = p3,
  width = 8,
  height = 5
)


ggsave(
  "plots/attendance_vs_marks.png",
  plot = p4,
  width = 8,
  height = 5
)


ggsave(
  "plots/subject_boxplot.png",
  plot = p5,
  width = 8,
  height = 5
)


ggsave(
  "plots/subject_average.png",
  plot = p6,
  width = 8,
  height = 5
)


ggsave(
  "plots/performance_categories.png",
  plot = p7,
  width = 8,
  height = 5
)


# 15. FINAL PROJECT SUMMARY

cat("\n")
cat("====================================\n")
cat(" STUDENT PERFORMANCE EDA SUMMARY\n")
cat("====================================\n")

cat(
  "Total Students:",
  nrow(students),
  "\n"
)

cat(
  "Total Subjects:",
  3,
  "\n"
)

cat(
  "Overall Average Marks:",
  round(mean(students$Average_Marks), 2),
  "\n"
)

cat(
  "Highest Average Marks:",
  max(students$Average_Marks),
  "\n"
)

cat(
  "Lowest Average Marks:",
  min(students$Average_Marks),
  "\n"
)

cat(
  "Study Hours Correlation:",
  round(study_correlation, 3),
  "\n"
)

cat(
  "Attendance Correlation:",
  round(attendance_correlation, 3),
  "\n"
)

cat(
  "Potential Outliers:",
  nrow(outliers),
  "\n"
)

cat(" PROJECT COMPLETED SUCCESSFULLY!\n")

