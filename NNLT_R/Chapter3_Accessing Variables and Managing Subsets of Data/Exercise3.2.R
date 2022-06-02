# Vị trí thư mục làm việc
setwd("C:/Ngôn ngữ lập trình R/RBook")
# Xuat du lieu sang tep ASCII
ISIT = read.table(file="ISIT.txt", header = TRUE)
names(ISIT)
str(ISIT)


# Trích xuất dữ liệu từ trạm 1
ISIT.Station1 <- ISIT[ISIT$Station == 1,]
# Có bao nhiêu quan sát được thực hiện tại nhà ga này?
nrow (ISIT.Station1) # Giả sử không có giá trị nào bị trống
# Giá trị nhỏ nhất
min(ISIT.Station1$SampleDepth)
# Giá trị median
median(ISIT.Station1$SampleDepth)
# Giá trị trung bình
mean(ISIT.Station1$SampleDepth)
# Giá trị lớn nhất
max(ISIT.Station1$SampleDepth)


# Tương tự chop trạm 2
ISIT.Station2 <- ISIT[ISIT$Station == 2,]
# Có bao nhiêu quan sát được thực hiện tại nhà ga này?
nrow (ISIT.Station2) # Giả sử không có giá trị nào bị trống
# Giá trị nhỏ nhất
min(ISIT.Station2$SampleDepth)
# Giá trị median
median(ISIT.Station2$SampleDepth)
# Giá trị trung bình
mean(ISIT.Station2$SampleDepth)
# Giá trị lớn nhất
max(ISIT.Station2$SampleDepth)


# Tương tự chop trạm 3
ISIT.Station3 <- ISIT[ISIT$Station == 3,]
# Có bao nhiêu quan sát được thực hiện tại nhà ga này?
nrow (ISIT.Station3) # Giả sử không có giá trị nào bị trống
# Giá trị nhỏ nhất
min(ISIT.Station3$SampleDepth)
# Giá trị median
median(ISIT.Station3$SampleDepth)
# Giá trị trung bình
mean(ISIT.Station3$SampleDepth)
# Giá trị lớn nhất
max(ISIT.Station3$SampleDepth)



# Xác định trạm có ít quan sát hơn đáng kể
nrow(ISIT.Station1)
nrow(ISIT.Station2)
nrow(ISIT.Station3)


# Số lần quan sát trên mỗi trạm
tapply(ISIT$Sources, FUN = length, INDEX = ISIT$Station)



# Tạo khung dữ liệu mới bỏ qua các trạm trên
ISIT2 <- ISIT[ISIT$Station != 4 & ISIT$Station != 5 ,]


# Trích xuất dữ liệu năm 2002
ISIT2[ISIT2$Year == 2002,]

# Rút gọn dữ liệu tháng 4 của tất cả các năm
ISIT2[ISIT2$Month == 4,]

# Trích xuất dữ liệu được đo ở độ sâu hơn 2000 mét (từ tất cả các năm và tháng).
ISIT2Deep <- ISIT2[ISIT2$SampleDepth > 2000,]

# Hiển thị dữ liệu theo các giá trị độ sâu tăng dần.
I1 <- order(ISIT2Deep$SampleDepth)
ISIT2Deep[I1,]

# Hiển thị dữ liệu đo được ở độ sâu hơn 2000 mét vào tháng 4.
ISIT2[ISIT2$SampleDepth > 2000 & ISIT2$Month == 4,]
