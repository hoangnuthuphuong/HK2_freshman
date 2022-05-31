# Nhap du lieu
Farm = c('MO','MO','MO','MO','LN','SE','QM')
Month = c(11,07,07,NA,09,09,11)
Year = c(00,00,01,NA,03,03,02)
Sex = c(1,2,2,2,1,2,2)
LengthClass = c(1,1,1,1,1,1,1)
LengthCT = c(75, 85, 91.6, 95, NA, 105.5, 106)
Ecervi = c(0,0,0,NA,0,0,0)
Tb = c(0,0,1,NA,0,0,0)
# Tao dataframe 
DT1 = data.frame(Farm=Farm, Month=Month, Year=Year, Sex=Sex, LengthClass=LengthClass, LengthCT=LengthCT, Ecervi=Ecervi, Tb=Tb)
DT1
# Tao bien LengthCT2 = can bac hai cua LengthCT
LengthCT2 = sqrt(LengthCT)
LengthCT2
# Tao dataframe voi LengthCT = LengthCT2
DT2 = data.frame(Farm=Farm, Month=Month, Year=Year, Sex=Sex, LengthClass=LengthClass, LengthCT=LengthCT2, Ecervi=Ecervi, Tb=Tb)
DT2
# Tao list thay vì tao dataframe voi du lieu tren
DT3 = list(Farm=Farm, Month=Month, Year=Year, Sex=Sex, LengthClass=LengthClass, LengthCT=LengthCT2, Ecervi=Ecervi, Tb=Tb)
DT3

