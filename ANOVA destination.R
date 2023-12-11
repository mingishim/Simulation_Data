
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
library(nortest)
library(rstatix) # Kruskal-Wallis 검정을 위한 패키지
library(dunn.test)


# Destination0
data <- read_excel("C:/Users/심민기/OneDrive - 부경대학교/바탕 화면/Updated_데이터 정리.xlsx")

# 데이터 구조 확인
str(data)

data$Level <- as.factor(data$Level)

# LastReachTime에 대한 비모수 검정

shapiro.test(data$FirstReachTime)

kruskal_result <- kruskal_test(FirstReachTime ~ Level, data = data)

print(kruskal_result)

dunn_result <- dunn.test(data$FirstReachTime, g = data$Level, method = "bonferroni")

# 결과 출력
print(dunn_result)

# LastReachTime에 대한 비모수 검정

shapiro.test(data$LastReachTime)


kruskal_result <- kruskal_test(LastReachTime ~ Level, data = data)

print(kruskal_result)

dunn_result <- dunn.test(data$LastReachTime, g = data$Level, method = "bonferroni")




# AverageReachTime에 대한 비모수 검정

shapiro.test(data$AverageReachTime)

kruskal_result <- kruskal_test(AverageReachTime ~ Level, data = data)

print(kruskal_result)


dunn_result <- dunn.test(data$AverageReachTime, g = data$Level, method = "bonferroni")

# Destination1

data <- read_excel("C:/Users/심민기/OneDrive - 부경대학교/바탕 화면/Updated_데이터 정리 2.xlsx")

# 데이터 구조 확인
str(data)

data$Level <- as.factor(data$Level)

# LastReachTime에 대한 비모수 검정

shapiro.test(data$FirstReachTime)

kruskal_result <- kruskal_test(FirstReachTime ~ Level, data = data)

print(kruskal_result)

dunn_result <- dunn.test(data$FirstReachTime, g = data$Level, method = "bonferroni")

# 결과 출력
print(dunn_result)

# LastReachTime에 대한 비모수 검정

shapiro.test(data$LastReachTime)


kruskal_result <- kruskal_test(LastReachTime ~ Level, data = data)

print(kruskal_result)

dunn_result <- dunn.test(data$LastReachTime, g = data$Level, method = "bonferroni")




# AverageReachTime에 대한 비모수 검정

shapiro.test(data$AverageReachTime)

kruskal_result <- kruskal_test(AverageReachTime ~ Level, data = data)

print(kruskal_result)


dunn_result <- dunn.test(data$AverageReachTime, g = data$Level, method = "bonferroni")

