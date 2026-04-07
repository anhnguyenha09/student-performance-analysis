# ============================================================
# PHẦN 03: MODELING - DỰ ĐOÁN KẾT QUẢ HỌC TẬP
# ============================================================

# 1. SETUP & LOAD DATA
library(dplyr)
install.packages("caret")
library(caret)
library(MASS)           # Cho Ordinal Logistic Regression (polr)
library(randomForest)
library(e1071)

# Đọc dữ liệu đã làm sạch
data <- read.csv("data/processed/clean_data.csv")

# 2. DATA PREPARATION (Dựa trên kết luận EDA)
model_data <- data %>%
  # Chọn các biến đã chốt từ EDA
  select(GRADE, CUML_GPA, ATTEND, SCHOLARSHIP, WORK, GENDER, STUDY_HRS) %>%
  mutate(across(everything(), as.factor))

# --- CHIẾN THUẬT: GỘP NHÓM GRADE (Target Engineering) ---
# Việc này giúp giải quyết vấn đề "Small cells" và tăng độ chính xác
model_data$GRADE_3LEVEL <- factor(
  ifelse(model_data$GRADE %in% c("AA", "BA", "BB"), "High",
         ifelse(model_data$GRADE %in% c("CB", "CC", "DC"), "Mid", "Low")),
  levels = c("Low", "Mid", "High"), 
  ordered = TRUE
)

# Xóa cột GRADE cũ để tránh nhầm lẫn
model_data <- model_data %>% select(-GRADE)

# 3. SPLIT DATA (Train 80% / Test 20%)
set.seed(123) # Để kết quả chia dữ liệu không thay đổi mỗi lần chạy
train_index <- createDataPartition(model_data$GRADE_3LEVEL, p = 0.8, list = FALSE)

train_set <- model_data[train_index, ]
test_set  <- model_data[-train_index, ]

# Kiểm tra tỷ lệ các nhóm sau khi chia
prop.table(table(train_set$GRADE_3LEVEL))
prop.table(table(test_set$GRADE_3LEVEL))

# ============================================================
# 4. MODEL 1: ORDINAL LOGISTIC REGRESSION (Mô hình toán học)
# ============================================================
# Phù hợp vì Low < Mid < High có tính thứ bậc
install.packages("MASS")
library(MASS)
model_ordered <- polr(GRADE_3LEVEL ~ CUML_GPA + ATTEND + SCHOLARSHIP + WORK, 
                      data = train_set, Hess = TRUE)

summary(model_ordered)

# Dự đoán trên tập test
pred_ordered <- predict(model_ordered, test_set)
library(caret)
cm_ordered <- confusionMatrix(pred_ordered, test_set$GRADE_3LEVEL)

cat("\n=== KẾT QUẢ ORDINAL LOGISTIC REGRESSION ===\n")
print(cm_ordered$overall['Accuracy'])

# ============================================================
# 5. MODEL 2: RANDOM FOREST (Mô hình máy học)
# ============================================================
# Ưu điểm: Không cần giả định về phân phối, bắt được tương tác phức tạp
set.seed(123)
install.packages("randomForest")
library(randomForest)
model_rf <- randomForest(GRADE_3LEVEL ~ ., 
                         data = train_set, 
                         importance = TRUE, 
                         ntree = 500)

# Dự đoán trên tập test
pred_rf <- predict(model_rf, test_set)
cm_rf <- confusionMatrix(pred_rf, test_set$GRADE_3LEVEL)

cat("\n=== KẾT QUẢ RANDOM FOREST ===\n")
print(cm_rf$overall['Accuracy'])
print(cm_rf$byClass) # Xem chi tiết độ chính xác từng nhóm (High/Mid/Low)

# 6. FEATURE IMPORTANCE (Kiểm chứng lại EDA)
importance_df <- as.data.frame(importance(model_rf))
importance_df$Variable <- rownames(importance_df)

p_imp <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "#4CAF50") +
  coord_flip() +
  ggtitle("Feature Importance - Random Forest") +
  theme_minimal()

print(p_imp)

# 7. LƯU MÔ HÌNH
saveRDS(model_rf, "outputs/models/student_grade_rf_model.rds")

# ============================================================
# 8. TỔNG KẾT & NHẬN ĐỊNH
# ============================================================
cat("\n=== NHẬN XÉT SAU KHI BUILD MODEL ===\n")
cat("1. Việc gộp GRADE thành 3 mức giúp mô hình đạt Accuracy ổn định hơn (thường > 65%).\n")
cat("2. Random Forest thường cho kết quả tốt hơn nhờ xử lý được các biến 'CONSIDER' như GENDER và STUDY_HRS.\n")
cat("3. Biến quan trọng nhất: ", importance_df$Variable[which.max(importance_df$MeanDecreaseGini)], "\n")

#cải thiện thử xem biến STUDY_HRS
# Gộp nhóm STUDY_HRS để giảm nhiễu (Ví dụ: 1,2 là ít; 3,4,5 là nhiều)
library(dplyr)

# Chạy lại lệnh gộp nhóm
model_data_v2 <- model_data %>%
  mutate(STUDY_HRS_BIN = factor(ifelse(as.numeric(STUDY_HRS) <= 2, "Low_Study", "High_Study"))) %>%
  select(-STUDY_HRS)
set.seed(123)
# Chia lại dữ liệu với biến mới
train_index_v2 <- createDataPartition(model_data_v2$GRADE_3LEVEL, p = 0.8, list = FALSE)
train_v2 <- model_data_v2[train_index_v2, ]
test_v2  <- model_data_v2[-train_index_v2, ]

# Huấn luyện mô hình mới
model_rf_v2 <- randomForest(GRADE_3LEVEL ~ ., 
                            data = train_v2, 
                            importance = TRUE, 
                            ntree = 500)

# Kiểm tra độ quan trọng mới
varImpPlot(model_rf_v2, main="Feature Importance sau khi gộp STUDY_HRS")