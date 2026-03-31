install.packages("ggplot2")
library(ggplot2)
library(dplyr)

# Đọc dữ liệu
data <- read.csv("data/processed/clean_data.csv")
# Convert tất cả sang factor
data <- data %>% mutate(across(everything(), as.factor))

# 1. PHÂN BỐ GRADE (TARGET)
cat("\n=== DISTRIBUTION OF GRADE ===\n")
print(table(data$GRADE))

p1 <- ggplot(data, aes(x = GRADE)) +
  geom_bar() +
  ggtitle("Distribution of Student Grades")

print(p1)
ggsave("outputs/plots/grade_distribution.png", p1)

# 2. STUDY_HRS vs GRADE
p2 <- ggplot(data, aes(x = STUDY_HRS, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Study Hours vs Grade")

print(p2)
ggsave("outputs/plots/study_vs_grade.png", p2)

# 3. ATTEND (Đi học) vs GRADE

p3 <- ggplot(data, aes(x = ATTEND, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Attendance vs Grade")

print(p3)
ggsave("outputs/plots/attendance_vs_grade.png", p3)


# 4. NOTES vs GRADE

p4 <- ggplot(data, aes(x = NOTES, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Note Taking vs Grade")

print(p4)
ggsave("outputs/plots/note_vs_grade.png", p4)

# 5. LISTENS vs GRADE
p5 <- ggplot(data, aes(x = LISTENS, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Listening vs Grade")

print(p5)
ggsave("outputs/plots/listening_vs_grade.png", p5)

# 6. WORK vs GRADE

p6 <- ggplot(data, aes(x = WORK, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Working Status vs Grade")

print(p6)
ggsave("outputs/plots/work_vs_grade.png", p6)


# 7. SCHOLARSHIP vs GRADE

p7 <- ggplot(data, aes(x = SCHOLARSHIP, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Scholarship vs Grade")

print(p7)
ggsave("outputs/plots/scholarship_vs_grade.png", p7)

# 8. CUML_GPA vs GRADE

p8 <- ggplot(data, aes(x = CUML_GPA, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Cumulative GPA vs Grade")

print(p8)
ggsave("outputs/plots/gpa_vs_grade.png", p8)


# 9. GENDER vs GRADE

p9 <- ggplot(data, aes(x = GENDER, fill = GRADE)) +
  geom_bar(position = "fill") +
  ggtitle("Gender vs Grade")

print(p9)
ggsave("outputs/plots/gender_vs_grade.png", p9)

# 10. INSIGHTS
cat("\n=== NHỮNG INSIGHT CHÍNH TỪ EDA ===\n")

cat("\n--- PHÂN BỐ ĐIỂM SỐ (GRADE DISTRIBUTION) ---\n")
cat("1. Phân bố điểm có dạng lệch phải: DD là mức điểm xuất hiện nhiều nhất (~35 sinh viên),\n")
cat("   tiếp theo là DC (~24) và CC (~21). Các điểm cao (AA, BA) tương đối hiếm.\n")
cat("   Điều này cho thấy kết quả học tập tổng thể trong bộ dữ liệu này thấp hơn mức trung bình.\n")
cat("2. Mức điểm CB thấp bất thường (~10 sinh viên), tạo ra một khoảng trũng giữa BB và CC,\n")
cat("   điều này có thể cho thấy hiệu ứng ranh giới chấm điểm.\n")

cat("\n--- ĐIỂM DANH (ATTENDANCE) ---\n")
cat("3. Điểm danh có mối quan hệ tích cực rõ ràng với điểm số.\n")
cat("   Sinh viên 'Always' (luôn tham gia lớp học) có phân bố điểm trải đều từ AA đến DD,\n")
cat("   trong khi nhóm 'Sometimes' (thỉnh thoảng tham gia) tập trung nhiều ở DD (~40%) và hầu như không có AA.\n")
cat("   => Điểm danh là một trong những yếu tố hành vi dự báo điểm số mạnh nhất.\n")

cat("\n--- GPA TÍCH LŨY (CUMULATIVE GPA) ---\n")
cat("4. CUML_GPA là yếu tố dự báo đơn lẻ mạnh nhất đối với điểm hiện tại.\n")
cat("   Sinh viên có GPA = 1 có ~25% Fail và ~25% DD, gần như không có điểm cao.\n")
cat("   Khi GPA tăng lên (3, 4, 5), tỷ lệ BB/CB/CC tăng và Fail gần như biến mất.\n")
cat("   => Tồn tại xu hướng tăng đơn điệu rõ ràng giữa CUML_GPA và GRADE.\n")
cat("   => CUML_GPA nên được ưu tiên làm biến đầu vào trong bất kỳ mô hình dự đoán nào.\n")

cat("\n--- GIỜ HỌC (STUDY HOURS) ---\n")
cat("5. Số giờ học cao hơn nhìn chung liên quan đến điểm số tốt hơn (STUDY_HRS từ 1 đến 4),\n")
cat("   nhưng STUDY_HRS = 5 lại cho thấy mức Fail tăng bất thường (~25%).\n")
cat("   Điều này có thể cho thấy việc học quá nhiều dưới áp lực, phương pháp học kém hiệu quả,\n")
cat("   hoặc kích thước mẫu nhỏ ở mức này. Cần được điều tra thêm.\n")

cat("\n--- NGHE GIẢNG & GHI CHÉP (LISTENING & NOTE-TAKING) ---\n")
cat("6. Mức độ tham gia trong lớp (LISTENS = 3) tương quan với điểm cao hơn:\n")
cat("   AA đạt ~15% và BA ~20%, trong khi DD và Fail giảm đáng kể.\n")
cat("7. Việc ghi chép (NOTES) cho thấy xu hướng tích cực từ mức 2 đến 3 (nhiều AA, BA, BB hơn).\n")
cat("   NOTES = 1 là trường hợp bất thường: gần như toàn bộ là DC/CB, không có AA hoặc Fail,\n")
cat("   cho thấy đây có thể là một nhóm rất nhỏ hoặc không điển hình.\n")
cat("   => Cả LISTENS và NOTES đều đại diện cho hành vi học tập chủ động\n")
cat("      và nên được đưa vào mô hình sau khi kiểm tra đa cộng tuyến.\n")

cat("\n--- TÌNH TRẠNG LÀM VIỆC (WORKING STATUS) ---\n")
cat("8. Sinh viên đi làm (WORK = Yes) có phân bố điểm kém hơn rõ rệt:\n")
cat("   DD tăng lên ~35% và Fail ~10%, trong khi AA giảm còn ~5%.\n")
cat("   Sinh viên không đi làm đạt AA ~15% và có tỷ lệ Fail rất thấp.\n")
cat("   => Làm thêm bán thời gian có vẻ ảnh hưởng tiêu cực đến kết quả học tập,\n")
cat("      có thể do giảm thời gian học và tăng mệt mỏi.\n")

cat("\n--- HỌC BỔNG (SCHOLARSHIP) ---\n")
cat("9. Mức học bổng 4 gắn với kết quả tốt nhất (AA ~35%, BA ~35%).\n")
cat("   Mức học bổng 1 chỉ có CB, có thể là nhóm chỉ có một sinh viên (không đáng tin cậy).\n")
cat("   Mức 2 và 3 có độ biến thiên cao (có cả AA và DD).\n")
cat("   => Học bổng là biến dự báo có ý nghĩa nhưng có thể phản ánh một phần GPA trước đó\n")
cat("      (vì học bổng thường được trao cho sinh viên có thành tích cao).\n")

cat("\n--- GIỚI TÍNH (GENDER) ---\n")
cat("10. Sinh viên nam có tỷ lệ điểm cao lớn hơn (AA ~20%, BA ~10%),\n")
cat("    trong khi sinh viên nữ có tỷ lệ Fail cao hơn (~15%) và không có AA.\n")
cat("    Tuy nhiên, điều này có thể bị ảnh hưởng bởi mất cân bằng mẫu hoặc các biến khác\n")
cat("    (ví dụ: học bổng, tình trạng đi làm) chứ không phải tác động trực tiếp của giới tính.\n")
cat("    => Nên đưa vào mô hình một cách thận trọng và xem xét các yếu tố gây nhiễu.\n")

cat("\n--- TỔNG KẾT ---\n")
cat("11. Các yếu tố dự báo quan trọng nhất được xác định từ EDA gồm:\n")
cat("    [Mạnh]     CUML_GPA, ATTEND, SCHOLARSHIP, WORK\n")
cat("    [Trung bình] LISTENS, NOTES, STUDY_HRS, GENDER\n")
cat("    [Yếu]      AGE, HS_TYPE, MOTHER_EDU, FATHER_EDU\n")
cat("12. Vì GRADE là biến thứ bậc (DD < DC < ... < AA),\n")
cat("    nên Ordinal Logistic Regression hoặc Random Forest được khuyến nghị\n")
cat("    thay vì hồi quy tuyến tính thông thường cho mô hình dự đoán.\n")
cat("13. Cần chú ý khả năng rò rỉ dữ liệu (data leakage) từ EXP_GPA,\n")
cat("    và kiểm tra đa cộng tuyến giữa LISTENS, NOTES và STUDY_HRS\n")
cat("    trước khi chốt bộ biến đầu vào.\n")


# 11. PRE-MODELING CHECKS
install.packages(c("corrplot", "vcd", "DescTools"))
library(corrplot)
library(vcd)
library(DescTools)

# --- 11.1 CHUYỂN SANG DẠNG SỐ ĐỂ TÍNH TƯƠNG QUAN ---
# (Do dùng toàn factor nên Cần numeric cho một số test)
data_num <- data %>% 
  mutate(across(everything(), ~ as.numeric(as.factor(.))))


# 11.2 CRAMÉR'S V — ĐO TƯƠNG QUAN GIỮA CÁC BIẾN CATEGORICAL
# (Thay thế cho correlation matrix khi biến là factor)
# Cramér's V: 0 = không liên quan, 1 = liên quan hoàn toàn

cat("\n=== CRAMÉR'S V MATRIX (Categorical Correlation) ===\n")

# Chọn các biến ứng viên (Tier 1 + Tier 2)
vars_candidate <- c("CUML_GPA", "ATTEND", "SCHOLARSHIP", "WORK",
                    "LISTENS", "NOTES", "STUDY_HRS", "GENDER",
                    "GRADE")

# Tính Cramér's V cho từng cặp
cramer_matrix <- matrix(NA,
                        nrow = length(vars_candidate),
                        ncol = length(vars_candidate),
                        dimnames = list(vars_candidate, vars_candidate))

for (i in vars_candidate) {
  for (j in vars_candidate) {
    tbl <- table(data[[i]], data[[j]])
    cramer_matrix[i, j] <- round(CramerV(tbl), 3)
  }
}

print(cramer_matrix)

# Visualize dưới dạng heatmap
cramer_df <- as.data.frame(as.table(cramer_matrix))
colnames(cramer_df) <- c("Var1", "Var2", "CramerV")

p_cramer <- ggplot(cramer_df, aes(x = Var1, y = Var2, fill = CramerV)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(CramerV, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "#2196F3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Cramér's V — Correlation Between Candidate Variables")


print(p_cramer)
ggsave("outputs/plots/cramer_v_heatmap.png", p_cramer, width = 8, height = 7)

# Cảnh báo đa cộng tuyến (V > 0.7 giữa 2 biến độc lập)
cat("\n--- Cặp biến có Cramér's V > 0.7 (nguy cơ đa cộng tuyến) ---\n")

for (i in vars_candidate) {
  for (j in vars_candidate) {
    if (i != j && i != "GRADE" && j != "GRADE") {
      if (!is.na(cramer_matrix[i, j]) && cramer_matrix[i, j] > 0.7) {
        cat(sprintf("  !! %s <-> %s : V = %.3f\n", i, j, cramer_matrix[i, j]))
      }
    }
  }
}

# Kiểm tra đa cộng tuyến do lệnh phía trên không in ra gì
max(cramer_matrix, na.rm = TRUE)
cramer_matrix[upper.tri(cramer_matrix)]
# Kết luận: Ko có đa cộng tuyến mạnh
# Phân tích Cramér's V cho thấy không có cặp biến độc lập nào có mức liên hệ mạnh (V > 0.7). 
# Do đó, nguy cơ đa cộng tuyến trong tập biến dự báo là thấp và 
#CÓ THỂ tất cả các biến có thể được giữ lại cho giai đoạn xây dựng mô hình.


# 11.3 CRAMÉR'S V VỚI GRADE — RANKING MỨC ĐỘ LIÊN QUAN

cat("\n=== CRAMÉR'S V vs GRADE (Feature Importance từ EDA) ===\n")

vars_features <- setdiff(vars_candidate, "GRADE")
cramer_vs_grade <- sapply(vars_features, function(v) {
  tbl <- table(data[[v]], data$GRADE)
  round(CramerV(tbl), 3)
})

cramer_grade_df <- data.frame(
  Variable = names(cramer_vs_grade),
  CramerV  = cramer_vs_grade
) %>% arrange(desc(CramerV))

print(cramer_grade_df)

p_importance <- ggplot(cramer_grade_df,
                       aes(x = reorder(Variable, CramerV), y = CramerV)) +
  geom_col(fill = "#1976D2") +
  geom_text(aes(label = round(CramerV, 2)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  ylim(0, 1) +
  ggtitle("Cramér's V vs GRADE — Feature Importance (EDA)") +
  xlab("Variable") + ylab("Cramér's V")

print(p_importance)
ggsave("outputs/plots/cramer_vs_grade.png", p_importance, width = 7, height = 5)


# 11.4 CHI-SQUARE TEST — KIỂM ĐỊNH Ý NGHĨA THỐNG KÊ
# (Biến nào thực sự có mối quan hệ có ý nghĩa với GRADE?)

cat("\n=== CHI-SQUARE TEST vs GRADE ===\n")
cat(sprintf("%-15s %-12s %-10s %s\n", "Variable", "Chi-sq", "df", "p-value"))
cat(strrep("-", 50), "\n")

chi_results <- data.frame()

for (v in vars_features) {
  tbl    <- table(data[[v]], data$GRADE)
  test   <- chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  sig    <- ifelse(test$p.value < 0.001, "***",
            ifelse(test$p.value < 0.01,  "**",
            ifelse(test$p.value < 0.05,  "*", "")))
  cat(sprintf("%-15s %-12.2f %-10d %.4f %s\n",
              v, test$statistic, test$parameter, test$p.value, sig))
  chi_results <- rbind(chi_results, data.frame(
    Variable = v,
    ChiSq    = round(test$statistic, 2),
    Df       = test$parameter,
    PValue   = round(test$p.value, 4),
    Sig      = sig
  ))
}

#png trực quan bảng kết quả chi-square
install.packages("gridExtra")
library(gridExtra)
library(grid)

p_table <- tableGrob(chi_results)

png("outputs/plots/chi_square_table.png", width = 900, height = 500)
grid.draw(p_table)
dev.off()
# 11.5 CLASS IMBALANCE — KIỂM TRA MẤT CÂN BẰNG NHÃN

cat("\n=== CLASS IMBALANCE CHECK ===\n")
grade_counts <- as.data.frame(table(data$GRADE))
colnames(grade_counts) <- c("Grade", "Count")
grade_counts$Pct <- round(grade_counts$Count / sum(grade_counts$Count) * 100, 1)
print(grade_counts)

cat("\n")
if (max(grade_counts$Pct) > 40) {
  cat("WARNING: Mất cân bằng nghiêm trọng — grade chiếm >40%. Cân nhắc oversampling (SMOTE).\n")
} else if (max(grade_counts$Pct) > 25) {
  cat("CAUTION: Mất cân bằng vừa phải — grade chiếm >25%. Theo dõi khi đánh giá model.\n")
} else {
  cat("OK: Phân bố tương đối cân bằng.\n")
}

#Sau khi chạy lệnh này thì kết quả in ra là phân bố tương đối cân bằng.


# 11.6 SAMPLE SIZE PER CELL — KIỂM TRA NHÓM NHỎ
# (Nhóm quá nhỏ sẽ gây nhiễu cho model)

cat("\n=== SAMPLE SIZE CHECK (biến vs GRADE) ===\n")
cat("Các cell có count < 5 (có thể gây vấn đề cho model):\n\n")

for (v in vars_features) {
  tbl <- table(data[[v]], data$GRADE)
  small_cells <- sum(tbl < 5)
  if (small_cells > 0) {
    cat(sprintf("  %s: %d cell(s) với n < 5\n", v, small_cells))
  }
}

small_cell_results <- data.frame()

for (v in vars_features) {
  tbl <- table(data[[v]], data$GRADE)
  small_cells <- sum(tbl < 5)

  if (small_cells > 0) {
    cat(sprintf("  %s: %d cell(s) với n < 5\n", v, small_cells))

    small_cell_results <- rbind(
      small_cell_results,
      data.frame(
        Variable = v,
        SmallCells = small_cells
      )
    )
  }
}

#png trực quan
p_table <- tableGrob(small_cell_results)
png("outputs/plots/small_cells_table.png", width = 700, height = 400)
grid.draw(p_table)
dev.off()

# ============================================================
# 11.7 TỔNG HỢP — CHỐT BIẾN ĐƯA VÀO MÔ HÌNH (gần như bước cuối)
# ============================================================
cat("\n=== FINAL VARIABLE SELECTION SUMMARY ===\n")

final_summary <- merge(cramer_grade_df, chi_results[, c("Variable", "PValue", "Sig")],
                       by = "Variable") %>%
  arrange(desc(CramerV))

cat(sprintf("%-15s %-12s %-10s %s\n", "Variable", "Cramér's V", "p-value", "Decision"))
cat(strrep("-", 55), "\n")

for (i in 1:nrow(final_summary)) {
  row      <- final_summary[i, ]
  decision <- ifelse(row$CramerV >= 0.3 & row$PValue < 0.05, "INCLUDE",
              ifelse(row$CramerV >= 0.15 & row$PValue < 0.05, "CONSIDER",
                     "EXCLUDE"))
  cat(sprintf("%-15s %-12.3f %-10.4f %s %s\n",
              row$Variable, row$CramerV, row$PValue, row$Sig, decision))
}

cat("\n--- Ngưỡng quyết định ---\n")
cat("  INCLUDE  : Cramér's V >= 0.3 VÀ p < 0.05\n")
cat("  CONSIDER : Cramér's V >= 0.15 VÀ p < 0.05\n")
cat("  EXCLUDE  : Cramér's V < 0.15 HOẶC p >= 0.05\n")


### IN BẢNG HOÀN CHỈNH VÀO TRONG OUTPUTS/tables
final_summary <- merge(
  cramer_grade_df,
  chi_results[, c("Variable", "PValue", "Sig")],
  by = "Variable"
) %>%
  arrange(desc(CramerV))

final_summary$Decision <- ifelse(
  final_summary$CramerV >= 0.3 & final_summary$PValue < 0.05, "INCLUDE",
  ifelse(
    final_summary$CramerV >= 0.15 & final_summary$PValue < 0.05,
    "CONSIDER",
    "EXCLUDE"
  )
)

# csv hoàn chỉnh với kết quả GẦN NHƯ cuối cùng
write.csv(
  final_summary,
  "outputs/tables/final_variable_selection.csv",
  row.names = FALSE
)

#png hoàn chỉnh trực quan bảng kết quả GẦN NHƯ cuối cùng
library(gridExtra)
library(grid)

p_table <- tableGrob(final_summary)

png("outputs/tables/final_variable_selection.png",
    width = 900,
    height = 450)

grid.draw(p_table)

dev.off()

# ============================================================
# test lần cuối do TỪ CÁI BẢNG GẦN NHƯ CUỐI CÙNG, nghi ngờ sample size quá nhỏ ở một số nhóm (ví dụ: WORK = Yes) có thể làm mất ý nghĩa thống kê của chi-square test.
# DIAGNOSTIC — cần hiểu tại sao ATTEND, WORK lại có p > 0.05
# ============================================================
cat("=== SAMPLE SIZE TỔNG ===\n")
cat("Tổng số dòng:", nrow(data), "\n")

cat("\n=== PHÂN BỐ ATTEND ===\n")
print(table(data$ATTEND))

cat("\n=== PHÂN BỐ WORK ===\n")
print(table(data$WORK))

cat("\n=== PHÂN BỐ GENDER ===\n")
print(table(data$GENDER))

cat("\n=== CROSS TABLE: ATTEND x GRADE ===\n")
print(table(data$ATTEND, data$GRADE))

cat("\n=== CROSS TABLE: WORK x GRADE ===\n")
print(table(data$WORK, data$GRADE))

cat("\n=== CROSS TABLE: GENDER x GRADE ===\n")
print(table(data$GENDER, data$GRADE))

cat("\n=== CHI-SQUARE (không dùng simulate) ===\n")
cat("ATTEND:\n")
print(chisq.test(table(data$ATTEND, data$GRADE)))
cat("WORK:\n")
print(chisq.test(table(data$WORK, data$GRADE)))
cat("GENDER:\n")
print(chisq.test(table(data$GENDER, data$GRADE)))


#THỰC SỰ LÀ KẾT LUẬN CUỐI CÙNG VỀ VIỆC CHỌN BIẾN ĐƯA VÀO MÔ HÌNH DỰA TRÊN EDA VÀ PRE-MODELING CHECKS
cat("\n=== FINAL VARIABLE SELECTION (sau pre-modeling checks) ===\n")

cat("\n--- GIẢI THÍCH KẾT QUẢ DIAGNOSTIC ---\n")
cat("1. ATTEND và WORK có p > 0.05 KHÔNG có nghĩa là không quan trọng.\n")
cat("   Nguyên nhân: nhóm 'Sometimes' chỉ 35 người, 'Yes' chỉ 49 người,\n")
cat("   trải trên 8 nhóm GRADE → nhiều cell < 5 → Chi-square mất power.\n")
cat("   Warning 'approximation may be incorrect' xác nhận vấn đề này.\n")
cat("   Cramér's V ~0.26 và biểu đồ EDA vẫn cho thấy tín hiệu rõ ràng.\n")

cat("\n2. GENDER là biến mạnh nhất về mặt thống kê (V=0.425, p=0.0005)\n")
cat("   vì Female = 0 AA và Male = 0 Fail tạo ra sự phân tách cực đoan.\n")
cat("   Tuy nhiên đây có thể là confounding effect với CUML_GPA,\n")
cat("   không nhất thiết là tác động trực tiếp của giới tính.\n")

cat("\n3. Không phát hiện đa cộng tuyến nghiêm trọng (V > 0.7)\n")
cat("   giữa các biến độc lập với nhau.\n")

cat("\n--- BỘ BIẾN CUỐI CÙNG ĐƯA VÀO MÔ HÌNH ---\n")
cat("INCLUDE  : CUML_GPA, ATTEND, SCHOLARSHIP, WORK\n")
cat("CONSIDER : NOTES, STUDY_HRS, GENDER (thêm vào nếu model cần cải thiện)\n")
cat("LITTLE CONSIDER : NOTES (pattern biểu đồ có nhưng yếu), STUDY_HRS (có ngoại lệ, anomaly mức 5) (có thể thêm vào nếu model cần cải thiện)\n")
cat("EXCLUDE  : LISTENS, EXP_GPA (leakage risk)\n")

cat("\n--- LƯU Ý KHI XÂY DỰNG MÔ HÌNH ---\n")
cat("- Sample size nhỏ (n=145) với 8 nhóm GRADE → cân nhắc gộp grade\n")
cat("  (ví dụ: High = AA+BA+BB, Mid = CB+CC+DC, Low = DD+Fail)\n")
cat("  để tăng power thống kê cho model.\n")
cat("- GRADE là biến thứ bậc → dùng Ordinal Logistic Regression\n")
cat("  hoặc Random Forest, không dùng linear regression.\n")
cat("- Theo dõi GENDER trong model (nếu sau này có đưa vào) vì tín hiệu mạnh nhưng\n")
cat("  có thể bị confounded bởi CUML_GPA.\n")

#NHƯ VẬY CHỐT LÀ:
# 1. CUML_GPA, ATTEND, SCHOLARSHIP, WORK là những biến chính đưa vào model.
# 2. NOTES, STUDY_HRS, GENDER có thể được xem xét thêm nếu model cần cải thiện hiệu suất, nhưng không phải là bắt buộc.
