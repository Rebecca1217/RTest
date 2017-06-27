library(portanalytics)
library(GCAMCPUB)
readFromLdc(c("201612", "201706"), "All cost based", jyDateRange = NULL)

date_tgt <- as.Date("2017-06-23")



tmp <- dataCenter$pl[Date >= '2017-06-01' & Date <= '2017-06-23' &
                       PL_Type == '红利收入' & 
                       Class_L3_ %in% c("Bond fund", "Active fund", "Index fund") &
                       Sec_Code %in% c("260112.OF", "000457.OF", "688888.OF")&
                       PL_LC > 0,
                     .(HS_Port_Code, Sec_Code, Sec_Name, PL_LC)]
attachHSPortInfo(tmp, "Port_Name")
tmp <- tmp[, .(Port_Name, Sec_Code, Sec_Name, PL_LC)]
wriet_open_xlsx(tmp)

tmp <- dataCenter$pos[Sec_Code == "090007.OF" & 
                        Date == "2017-06-23", 
                      .(HS_Port_Code, Sec_Code, Sec_Name, Quantity)]
attachHSPortInfo(tmp, "Port_Name")
wriet_open_xlsx(tmp)

div_code <- unique(tmp$Sec_Code)
check <- dataCenter$pl[PL_Type == "红利收入" & Sec_Code %in% div_code & PL_LC >0]
View(check)

unique(check$Sec_Name)

tmp <- dataCenter$trans[Date >= "2017-06-01" & 
                          Class_L3_ %in% c("Bond fund", "Index fund", "Active fund") &
                          Trans_Type == "卖出" & 
                          Sec_Code %in% c("000457.OF", "260112.OF", "688888.OF", "750005.OF")]


pl_sell <- dataCenter$pl[Date >= "2017-06-01" & 
                           Class_L3_ %in% c("Bond fund", "Index fund", "Active fund") &
                           Sec_Code %in% c("000457.OF", "260112.OF", "688888.OF", "750005.OF") & 
                           PL_LC != 0 &
                           PL_Type == "价差收入"]

attachHSPortInfo(pl_sell, "Port_Name")
pl_sell <- pl_sell[, .(Port_Name, Sec_Code, Sec_Name, PL_LC)]
write_open_xlsx(pl_sell)


# pipeline ----------------------------------------------------------------

tmp <- dataCenter$pl[Class_L3_ == "Pipeline" &  PL_LC > 0 & Sec_Code == "ZYGDPT.IB"]
attachHSPortInfo(tmp, "Port_Name")
write_open_xlsx(tmp[, .(Port_Name, Sec_Code, Sec_Name, PL_LC)])

