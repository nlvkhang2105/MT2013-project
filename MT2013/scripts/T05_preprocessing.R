# ============================================================
# T05 - Data Preprocessing
# Owner: Tue
# Goal: Prepare data for descriptive and inferential analysis
# ============================================================

source("MT2013/scripts/load_data.R")
df <- load_data()

# Selected variables:
# Width : X1    - Image width
# Class : X1558 - Ad / Non-ad
# F1    : X351  - url*ads
# F2    : X1455 - alt*click+here

# ------------------------------------------------------------
# TODO 1: Select and validate the analysis variables.
# Check data types, valid values, and Class labels.
# ------------------------------------------------------------

# 1. Trích xuất và đặt tên 4 biến mục tiêu
analysis_df <- data.frame(
  Width = df$X1,
  Class = df$X1558,
  F1    = df$X351,
  F2    = df$X1455,
  stringsAsFactors = FALSE
)

# 2. Kiểm tra tính hợp lệ (Validation)
# - Width: Kiểm tra kiểu dữ liệu số và giá trị hợp lệ (> 0)
if (!is.numeric(analysis_df$Width)) {
  stop("Lỗi xác thực: Biến 'Width' không phải kiểu số (numeric).")
}
if (any(analysis_df$Width[!is.na(analysis_df$Width)] <= 0)) {
  stop("Lỗi xác thực: Biến 'Width' chứa giá trị không hợp lệ (<= 0).")
}

# - Class: Chỉ được chứa hai nhãn hợp lệ c('ad.', 'nonad.')
valid_classes <- c("ad.", "nonad.")
invalid_classes <- setdiff(unique(analysis_df$Class[!is.na(analysis_df$Class)]), valid_classes)
if (length(invalid_classes) > 0) {
  stop("Lỗi xác thực: Phát hiện nhãn 'Class' không hợp lệ: ", paste(invalid_classes, collapse = ", "))
}

# - F1 và F2: Chỉ nhận giá trị 0, 1 hoặc NA
valid_binary <- c(0, 1)
invalid_f1 <- setdiff(unique(analysis_df$F1[!is.na(analysis_df$F1)]), valid_binary)
if (length(invalid_f1) > 0) {
  stop("Lỗi xác thực: Biến 'F1' chứa giá trị ngoài {0, 1, NA}: ", paste(invalid_f1, collapse = ", "))
}

invalid_f2 <- setdiff(unique(analysis_df$F2[!is.na(analysis_df$F2)]), valid_binary)
if (length(invalid_f2) > 0) {
  stop("Lỗi xác thực: Biến 'F2' chứa giá trị ngoài {0, 1, NA}: ", paste(invalid_f2, collapse = ", "))
}

cat("=== TODO 1: XÁC THỰC BIẾN ===\n")
cat("Tất cả các biến (Width, Class, F1, F2) đều hợp lệ theo quy định.\n\n")

# ------------------------------------------------------------
# TODO 2: Assess missing values and duplicate records.
# Create missing_by_class for Width missingness.
# Create duplicate_rows using all columns except Index.
# Do not remove duplicate records.
# ------------------------------------------------------------

# A. Đánh giá dữ liệu thiếu (Missing values)
# Đếm số giá trị NA của từng biến được chọn
missing_counts <- sapply(analysis_df, function(col) sum(is.na(col)))
cat("=== TODO 2: DỮ LIỆU THIẾU VÀ DÒNG TRÙNG ===\n")
cat("Số lượng NA theo từng biến:\n")
print(missing_counts)
cat("\n")

# Thống kê thiếu Width theo từng nhóm Class
classes <- c("ad.", "nonad.")
n_total <- as.vector(table(factor(analysis_df$Class, levels = classes)))
n_missing <- as.vector(tapply(is.na(analysis_df$Width), factor(analysis_df$Class, levels = classes), sum))
n_valid <- n_total - n_missing
missing_rate <- round(n_missing / n_total, 4)

missing_by_class <- data.frame(
  Class = classes,
  n_total = n_total,
  n_missing = n_missing,
  missing_rate = missing_rate,
  n_valid = n_valid,
  stringsAsFactors = FALSE
)

cat("Bảng thống kê thiếu Width theo Class (missing_by_class):\n")
print(missing_by_class)
cat("\n")

# B. Đánh giá dòng trùng (Duplicate records)
# Kiểm tra trùng trên toàn bộ dữ liệu gốc ngoại trừ cột Index (cột đầu tiên)
duplicate_rows <- duplicated(df[, -1])
n_duplicates <- sum(duplicate_rows)
cat("Số dòng trùng lặp phát hiện trong dữ liệu gốc (không tính Index):", n_duplicates, "\n")
cat("Lưu ý: Giữ nguyên các dòng trùng trong dữ liệu phân tích; vector 'duplicate_rows' được bàn giao cho T07.\n\n")

# ------------------------------------------------------------
# TODO 3: Prepare and validate analysis datasets.
# analysis_df: Selected variables, retaining missing values.
# rq1_data: Complete Width and Class observations.
# rq2_data: Separate complete datasets for F1 and F2.
# ------------------------------------------------------------

# 1. analysis_df: Đã tạo ở TODO 1, giữ nguyên NA
# 2. rq1_data: Chỉ giữ các quan sát đầy đủ của Width và Class (RQ1)
rq1_data <- analysis_df[!is.na(analysis_df$Width) & !is.na(analysis_df$Class), c("Width", "Class")]

# 3. rq2_data: Named list gồm 2 bộ dữ liệu được lọc NA riêng biệt (RQ2)
rq2_data <- list(
  F1 = analysis_df[!is.na(analysis_df$Class) & !is.na(analysis_df$F1), c("Class", "F1")],
  F2 = analysis_df[!is.na(analysis_df$Class) & !is.na(analysis_df$F2), c("Class", "F2")]
)

# Kiểm tra và đối chiếu cỡ mẫu để đảm bảo tính nhất quán
cat("=== TODO 3: BỘ DỮ LIỆU PHÂN TÍCH ===\n")
cat("- analysis_df :", nrow(analysis_df), "dòng,", ncol(analysis_df), "cột (giữ nguyên NA)\n")
cat("- rq1_data    :", nrow(rq1_data), "dòng (kỳ vọng:", sum(missing_by_class$n_valid), "dòng hợp lệ)\n")
cat("- rq2_data$F1 :", nrow(rq2_data$F1), "dòng (complete case Class & F1)\n")
cat("- rq2_data$F2 :", nrow(rq2_data$F2), "dòng (complete case Class & F2)\n\n")

cat("Hoàn thành T05. Các đối tượng bàn giao sẵn sàng:\n")
cat("- analysis_df, rq1_data, rq2_data, missing_by_class, duplicate_rows\n")
