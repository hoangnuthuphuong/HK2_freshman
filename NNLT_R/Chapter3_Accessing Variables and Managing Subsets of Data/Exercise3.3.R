# Xuất dữ liệu được đo ở độ sâu lớn hơn 2000 mét vào tháng 4 sang một tệp ascii mới.
ISIT2DeepApril <- ISIT2[ISIT2$SampleDepth > 2000 & ISIT2$Month == 4,]
write.table(ISIT2DeepApril, file = "ISIT2DeepApril.txt")
ISIT2DeepApril
