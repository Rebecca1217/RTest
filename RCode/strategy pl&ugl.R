library(RODBC)
library(GCAMCPUB)
library(openxlsx)
library(data.table)

dateBegin <- as.Date("2017-01-01")
dateBased <- as.Date("2017-04-20")
myConn <- activate_conn(myconn_)
pns <- c("Strategy No9", "Strategy No10", "Strategy No11", "Strategy No12", "Strategy No13",
         "Strategy No54", "Strategy No55", "Strategy No56", "Strategy No57", "Strategy No58")

# data loading ------------------------------------------------------------

sql <- "select Date, Port_Order, Port_Name, Sec_Code, Sec_Name, Class_L3, PL_LC, PL_Type from CORE_Data_PL where Port_Name in 
('Strategy No9', 'Strategy No10', 'Strategy No11', 'Strategy No12', 'Strategy No13', 
'Strategy No54', 'Strategy No55', 'Strategy No56', 'Strategy No57', 'Strategy No58') 
and Date = '%s'
and Class_L3 = 'Stock'
and PL_Type = '价差收入'"
sql <- sprintf(sql, dateBased)
pl <- sqlQuery(myConn, sql) %>% setDT()
pl[, Date := to_date(Date)]
pl[, .(PL_LC = sum(PL_LC)), by = .(Date, Port_Order, Port_Name, Sec_Code, Sec_Name, Class_L3)]



sql <- "select Date, Port_Order, Port_Name, Sec_Code, Sec_Name,
Quantity, AV_Book_LC, AV_Mix_LC, Class_L3, UGL_LC
from CORE_Data_Holding where Port_Name in 
('Strategy No9', 'Strategy No10', 'Strategy No11', 'Strategy No12', 'Strategy No13', 
'Strategy No54', 'Strategy No55', 'Strategy No56', 'Strategy No57', 'Strategy No58') 
and Date = '%s'
and Class_L3 = 'Stock'
"
sql <- sprintf(sql, dateBased)
pos <- sqlQuery(myConn, sql) %>% setDT()
pos[, Date := to_date(Date)]
setorder(pos, Port_Order)
pos <- pl[pos, on = c("Date", "Port_Name", "Port_Order","Sec_Code", "Sec_Name", "Class_L3")]
pos[, `:=`(
  Label = "当前持仓",
  PL_LC = GCAMCPUB::na_fill(PL_LC, 0)
)]
sold <- pl[! Sec_Code %in% pos$Sec_Code]
sold[, `:=`(
  Quantity = 0,
  AV_Book_LC = 0,
  AV_Mix_LC = 0,
  UGL_LC = 0,
  Label = "已卖出"
)]
tmp <- rbindlist(list(pos, sold), fill = TRUE)
tmp <- tmp[, .(PL_LC = sum(PL_LC), UGL_LC = sum(UGL_LC), Quantity = sum(Quantity),
               AV_Book_LC = sum(AV_Book_LC), AV_Mix_LC = sum(AV_Mix_LC)), 
           by = .(Date, Port_Order, Port_Name, Sec_Code, Sec_Name, Class_L3, Label)]

# UGL at beginning
sql <- "select Port_Name, Sec_Code, Sec_Name, UGL_LC as UGL_LYE from CORE_Data_Holding
where Date = '%s' and Class_L3 = 'Stock' and Port_Name in 
('Strategy No9', 'Strategy No10', 'Strategy No11', 'Strategy No12', 'Strategy No13', 
'Strategy No54', 'Strategy No55', 'Strategy No56', 'Strategy No57', 'Strategy No58')"
sql <- sprintf(sql, dateBegin - 1)
uglLYE <- sqlQuery(myConn, sql) %>% setDT()

tmp <- uglLYE[tmp, on = c("Port_Name", "Sec_Code", "Sec_Name")]
tmp[, UGL_LYE := GCAMCPUB::na_fill(UGL_LYE, 0)]
tmp[, UGL_Chg := UGL_LC - UGL_LYE]

setorder(tmp, Port_Order)
tmp <- tmp[, .(Date, Port_Name, Sec_Code, Sec_Name, Label, AV_Book_LC, AV_Mix_LC,
               Quantity, PL_LC, UGL_LC, UGL_Chg)]
# file setting ------------------------------------------------------------

fileName <- paste0("策略精选产品股票盈亏情况", dateBased, ".xlsx")
path <- file.path(
  "C:/Users/AMC161/Desktop",
  fileName
)

# excel writing -----------------------------------------------------------

styleHeader <-
  createStyle(valign = "center", halign = "center", wrapText = TRUE,
              border = "TopBottomLeftRight", fgFill = "grey")

wb <- createWorkbook("Strategy")
addWorksheet(wb, "股票账面收益及浮盈浮亏状况")
## Sheet 1
writeData(wb, 1, tmp, borders = "all", headerStyle = styleHeader)
addStyle(wb, 1, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(tmp)) + 1,
         cols = which(colnames(tmp) %in% c("PL_LC", "AV_Book_LC", "AV_Mix_LC", "UGL_LC", "UGL_Chg")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "DATE"), rows = seq_len(nrow(tmp)) + 1,
         cols = which(colnames(tmp) %in% c("Date")),
         stack = TRUE, gridExpand = TRUE)
setColWidths(wb, sheet = 1,
             cols = seq_len(ncol(tmp)),
             widths = c(12, 16, 12, 12, 12, 18, 18, 15, 18, 18, 18))
showGridLines(wb, 1, showGridLines = FALSE)
# setColWidths(wb, sheet = 1, cols = seq_len(ncol(pl)), widths = "auto")
## write excel
saveWorkbook(wb, path, overwrite = TRUE)
