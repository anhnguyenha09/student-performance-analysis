# ============================================================
# PHẦN 04: EVALUATION - ĐÁNH GIÁ CHI TIẾT
# ============================================================

# 1. SO SÁNH ACCURACY GIỮA CÁC MÔ HÌNH
model_comparison <- data.frame(
  Model = c("Ordinal Logistic Regression", "Random Forest"),
  Accuracy = c(cm_ordered$overall['Accuracy'], cm_rf$overall['Accuracy']),
  Kappa = c(cm_ordered$overall['Kappa'], cm_rf$overall['Kappa'])
)

print("--- SO SÁNH HIỆU SUẤT TỔNG QUAN ---")
print(model_comparison)

# 2. CHI TIẾT SAI SỐ (CONFUSION MATRIX VISUALIZATION)
# Chuyển Confusion Matrix của Random Forest sang dạng Data Frame để vẽ đồ thị
cm_df <- as.data.frame(cm_rf$table)

p_cm <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vcolor = "white", size = 5) +
  scale_fill_gradient(low = "#e8f5e9", high = "#2e7d32") +
  labs(title = "Confusion Matrix - Random Forest",
       subtitle = "Đánh giá sự nhầm lẫn giữa các mức điểm",
       x = "Thực tế (Actual)",
       y = "Dự đoán (Predicted)") +
  theme_minimal()

print(p_cm)
ggsave("outputs/plots/04_confusion_matrix.png", plot = p_cm)

# 3. ĐÁNH GIÁ THEO TỪNG NHÓM (CLASS-SPECIFIC METRICS)
# Trích xuất Precision, Recall, F1-Score cho từng nhóm High, Mid, Low
metrics_by_class <- as.data.frame(cm_rf$byClass) %>%
  select(Sensitivity, Specificity, Precision, `F1`)

cat("\n--- CHỈ SỐ CHI TIẾT THEO TỪNG NHÓM (RANDOM FOREST) ---\n")
print(metrics_by_class)

# 4. KIỂM TRA ĐỘ LỆCH DỰ ĐOÁN (PREDICTION ERROR ANALYSIS)
test_results <- test_set
test_results$Predicted <- pred_rf
test_results$IsCorrect <- test_results$GRADE_3LEVEL == test_results$Predicted

# Xem những trường hợp mô hình dự đoán sai thường rơi vào nhóm nào
error_analysis <- test_results %>%
  filter(IsCorrect == FALSE) %>%
  group_control(GRADE_3LEVEL, Predicted) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

cat("\n--- PHÂN TÍCH CÁC TRƯỜNG HỢP DỰ ĐOÁN SAI ---\n")
print(error_analysis)

# 5. XUẤT BÁO CÁO KẾT QUẢ CUỐI CÙNG
final_summary <- list(
  Best_Model = "Random Forest",
  Overall_Accuracy = cm_rf$overall['Accuracy'],
  Top_Predictor = importance_df$Variable[which.max(importance_df$MeanDecreaseGini)],
  Recommendation = "Sử dụng Random Forest cho hệ thống cảnh báo sớm sinh viên có nguy cơ thấp (Low)."
)

# Lưu file RData để sau này tái sử dụng mà không cần chạy lại
save(model_comparison, metrics_by_class, file = "outputs/models/evaluation_results.RData")

cat("\n=== HOÀN TẤT ĐÁNH GIÁ ===\n")