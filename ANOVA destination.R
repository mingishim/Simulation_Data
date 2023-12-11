
# 필요한 라이브러리 로드
library(dplyr)
library(car)
library(reshape2)
library(ez)
library(readxl)
library(stats)
library(ggplot2)
library(ggpubr)
library(emmeans)# 데이터 불러오기
library(agricolae)
library(multcomp)
library(PMCMRplus)

# Destination0
data <- read_excel("C:/Users/심민기/OneDrive - 부경대학교/바탕 화면/데이터 정리.xlsx")


# 데이터 구조 확인
str(data)

data$Level <- as.factor(data$Level)

# ANOVA 분석 수행
# FirstReachTime에 대한 ANOVA
first_reach_anova <- aov(FirstReachTime ~ Level, data = data)

print(first_reach_anova)

summary(first_reach_anova)

bonferroni_test <- glht(first_reach_anova, linfct = mcp(Level = "Tukey"))

# 결과 요약 및 출력
summary(bonferroni_test)

# LastReachTime에 대한 ANOVA
last_reach_anova <- aov(LastReachTime ~ Level, data = data)

summary(last_reach_anova)

bonferroni_test <- glht(last_reach_anova, linfct = mcp(Level = "Tukey"))

# 결과 요약 및 출력
summary(bonferroni_test)



# AverageReachTime에 대한 ANOVA
average_reach_anova <- aov(AverageReachTime ~ Level, data = data)
summary(average_reach_anova)

average_reach_anova <- glht(last_reach_anova, linfct = mcp(Level = "Tukey"))

# 결과 요약 및 출력
summary(average_reach_anova)


# Destination1

data <- read_excel("C:/Users/심민기/OneDrive - 부경대학교/바탕 화면/데이터 정리 2.xlsx")

# 데이터 구조 확인
str(data)

data$Level <- as.factor(data$Level)

# ANOVA 분석 수행
# FirstReachTime에 대한 ANOVA
first_reach_anova <- aov(FirstReachTime ~ Level, data = data)

summary(first_reach_anova)

# 본페르니 사후검정
bonferroni_test <- glht(last_reach_anova, linfct = mcp(Level = "Tukey"))

# 결과 요약 및 출력
summary(bonferroni_test)

# LastReachTime에 대한 ANOVA
last_reach_anova <- aov(LastReachTime ~ Level, data = data)
summary(last_reach_anova)

bonferroni_test <- glht(last_reach_anova, linfct = mcp(Level = "Tukey"))

# 결과 요약 및 출력
summary(bonferroni_test)



# AverageReachTime에 대한 ANOVA
average_reach_anova <- aov(AverageReachTime ~ Level, data = data)
summary(average_reach_anova)

average_reach_anova <- glht(last_reach_anova, linfct = mcp(Level = "Tukey"))

# 결과 요약 및 출력
summary(average_reach_anova)

