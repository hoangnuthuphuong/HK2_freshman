# Trạm từ 1 đến 5 được lấy mẫu vào tháng 4 năm 2001,
# trạm từ 6 đến 11 vào tháng 8 năm 2001,
# trạm từ 12 đến 15 vào tháng 3 năm 2002,
# trạm từ 16 đến 19 vào tháng 10 năm 2002.

# Đầu tiên tạo một biến mới bên trong tệp ISIT:
ISIT$NewMonth <- ISIT$SampleDepth

# Thay đổi các giá trị
ISIT$NewMonth[ISIT$Station <= 5] <- "Tháng 4"
ISIT$NewMonth[ISIT$Station > 5 & ISIT$Station <= 11 ]  <- "Tháng 8"
ISIT$NewMonth[ISIT$Station > 12 & ISIT$Station <= 15 ] <- "Tháng 3"
ISIT$NewMonth[ISIT$Station > 15 & ISIT$Station <= 19 ] <- "Tháng 10"
ISIT$NewMonth  
ISIT$NewMonth <- factor(ISIT$NewMonth)


# Tạo một biến mới bên trong tệp ISIT:
ISIT$NewYear <- ISIT$SampleDepth

# Thay đổi các giá trị
ISIT$NewYear[ISIT$Station > 1 & ISIT$Station <= 11 ]  <- "Năm 2001"
ISIT$NewYear[ISIT$Station > 12 & ISIT$Station <= 19 ] <- "Năm 2002"
ISIT$NewYear
ISIT$NewYear <- factor(ISIT$NewYear)

