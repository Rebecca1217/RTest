library(portanalytics)
library(data.table)
library(GCAMCPUB)
readFromLdc(c("201601", "201712"), "All cost based", jyDateRange = NULL)
trans <- attachHSPortInfo(dataCenter$trans, "Port_Name")

trans_div <- trans[Date > "2016-12-31" & Date <= "2017-12-31" & 
                     Class_L3 == "Stock" & Trans_Type == "分红",
                   .(Trans_Date = Date, Port_Name, Sec_Code, Sec_Name, CF_LC, IAS)]


# fix some bugs in data
trans_div <- trans_div[!(Port_Name == "Par" & Sec_Code == "KYG931751005.HK" &
                           Trans_Date == "2017-05-25" &
                           CF_LC < 30000) &
                         (IAS != "HFT")]

pos <- attachHSPortInfo(dataCenter$pos, "Port_Name")

pos <- pos[Class_L3 == "Stock", .(Pos_Date = Date, Port_Name, Sec_Code, Sec_Name,
                                  Quantity, AV_Mix_LC)]
pos <- pos[, .(Quantity = sum(Quantity), AV_Mix_LC = sum(AV_Mix_LC)), 
           by = .(Pos_Date, Port_Name, Sec_Code, Sec_Name)]


res <- pos[trans_div, on = c("Sec_Code", "Port_Name", "Sec_Name"), allow.cartesian = TRUE]

res <- res[Pos_Date <= Trans_Date & Pos_Date >= (Trans_Date - 365 - 2)]



res <- days[res, on = c("Port_Name", "Sec_Code", "Trans_Date")]

res <- res[N > 254]

res_1 <- trans_div[, Date_1_Year := Trans_Date - 365]

res_tmp <- res[, 
               .(Port_Name, Sec_Code, Sec_Name, Pos_Date, CF_LC, Quantity, N)]
setnames(res_tmp, "Pos_Date", "Trans_Date")
setorder(res_tmp, Port_Name, Sec_Code, Sec_Name, Trans_Date)
# to find the quantity at trans_date, because div trans's quantity is 0

res_1 <- res_tmp[
  res_1, on = c("Trans_Date", "Port_Name","Sec_Code", "Sec_Name", "CF_LC"), roll = TRUE]

setnames(res_1, "Quantity", "Quantity_Trans")

res_tmp <- res[, 
               .(Port_Name, Sec_Code, Sec_Name, Pos_Date, CF_LC, Quantity)]
setnames(res_tmp, "Pos_Date", "Date_1_Year")
setorder(res_tmp, Port_Name, Sec_Code, Sec_Name, Date_1_Year)

res_1 <- res_tmp[
  res_1, on = c("Date_1_Year", "Port_Name", "Sec_Code", "Sec_Name", "CF_LC"), roll = TRUE
]
setnames(res_1, "Quantity", "Quantity_1_Year_Before")
write_open_xlsx(res_1[!is.na(N)])


# how to make it roll based on one column and join based on several columns?
