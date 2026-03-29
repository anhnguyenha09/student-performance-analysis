raw_data <- read.csv("data/raw/student_prediction.csv")
View(raw_data)

str(raw_data)
summary(raw_data)

#check NA
colSums(is.na(raw_data))
#xóa cột không cần
install.packages("dplyr")
library(dplyr)

clean_data <- raw_data %>%
  select(-STUDENTID, -COURSE.ID)

#convert sang factor vì dữ liệu là categories(không phải số) 
#example: GENDER 1: female 2: male
clean_data <- clean_data %>%
  mutate(across(where(is.numeric), as.factor))

#gán một vài dữ liệu giống mô tả trong kaggle
clean_data <- clean_data %>%
  mutate(
    AGE = factor(AGE, levels = c(1,2,3),
                 labels = c("18-21","22-25","26+")),
    
    GENDER = factor(GENDER, levels = c(1,2),
                    labels = c("Female","Male")),
    
    WORK = factor(WORK, levels = c(1,2),
                  labels = c("Yes","No")),
    
    ACTIVITY = factor(ACTIVITY, levels = c(1,2),
                      labels = c("Yes","No")),
    
    PARTNER = factor(PARTNER, levels = c(1,2),
                     labels = c("Yes","No")),
    
    ATTEND = factor(ATTEND, levels = c(1,2,3),
                    labels = c("Always","Sometimes","Never")),
    
    GRADE = factor(GRADE,
                   levels = 0:7,
                   labels = c("Fail","DD","DC","CC","CB","BB","BA","AA"))
  )
str(clean_data)
summary(clean_data)

install.packages("readr")
library(readr)
write_csv(clean_data, "data/processed/clean_data.csv")

View(clean_data)
