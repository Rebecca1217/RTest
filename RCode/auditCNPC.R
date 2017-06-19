library(RODBC)
library(GCAMCPUB)
library(openxlsx)
library(data.table)

dateBegin <- as.Date("2016-09-16")
dateBased <- as.Date("2016-10-17")
myConn <- activate_conn(myconn_)
# data loading ------------------------------------------------------------

sql <- "select b.Class_L3_CN, Sec_Name, IAS, AV_Book_LC, AV_Mix_LC, 
AV_Mix_LC / (select sum(AV_Mix_LC) from CORE_Data_Holding
where Port_Name = 'CNPC'
and Date = '%s') as proportion,
YTM_Book, Maturity_Date, Rating_External, Issuer_Rating_External from CORE_Data_Holding a 
left join CORE_para_Asset_Class_relationship b
on a.Class_L3 = b.Class_L3 where Port_Name = 'CNPC'
and Date = '%s'
order by a.Class_Order
"
sql <- sprintf(sql, dateBased, dateBased)
pos <- sqlQuery(myConn, sql) %>% setDT()

sql <- "select Date, b.Class_L3_CN, Sec_Name, Trans_Type, CF_LC from core_data_trans a
left join CORE_para_Asset_Class_relationship b
on a.Class_L3 = b.Class_L3
where Port_Name = 'CNPC' 
and Date >= '%s' and Date <= '%s'
and trans_type <> '数量调整'
order by Date
"
sql <- sprintf(sql, dateBegin, dateBased)
trans <- sqlQuery(myConn, sql) %>% setDT()
trans[, Date := to_date(Date)]

setnames(pos, c("资产类别", "资产名称", "会计分类", "成本", "账面价值", "占比",
                "到期收益率", "到期日", "外部评级-债项", "外部评级-主体"))
setnames(trans, c("交易日期", "资产类别", "资产名称", "交易类型", "交易金额"))

# file setting ------------------------------------------------------------

fileName <- paste0("阳光团险组合资产报告", dateBased, ".xlsx")
path <- file.path(
  "T:/2_共享区/4_组合管理部_Portforlio_Management/",
  "4_报告_Reporting/6_Audit Report/CNPC",
  fileName
)

# excel writing -----------------------------------------------------------

styleHeader <-
  createStyle(valign = "center", halign = "center", wrapText = TRUE,
              border = "TopBottomLeftRight", fgFill = "grey")

wb <- createWorkbook("CNPC")
addWorksheet(wb, "持仓信息")
addWorksheet(wb, "过去一月交易明细")
## Sheet 1
writeData(wb, 1, pos, borders = "all", headerStyle = styleHeader)
addStyle(wb, 1, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(pos)) + 1,
         cols = which(colnames(pos) %in% c("成本", "账面价值")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "PERCENTAGE"), rows = seq_len(nrow(pos)) + 1,
         cols = which(colnames(pos) %in% c("占比", "到期收益率")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "DATE"), rows = seq_len(nrow(pos)) + 1,
         cols = which(colnames(pos) %in% c("到期日")),
         stack = TRUE, gridExpand = TRUE)
setColWidths(wb, sheet = 1,
             cols = which(colnames(pos) %in%
                            c("资产类别", "资产名称", "会计分类", "成本", "账面价值",
                              "到期收益率", "到期日", "外部评级-债项", "外部评级-主体")),
             widths = c(20, 30, 10, 15, 15, 15, 15, 15, 15))
showGridLines(wb, 1, showGridLines = FALSE)
# setColWidths(wb, sheet = 1, cols = seq_len(ncol(pos)), widths = "auto")
## Sheet 2
writeData(wb, 2, trans, borders = "all", headerStyle = styleHeader)
addStyle(wb, 2, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(pos)) + 1,
         cols = which(colnames(trans) %in% c("交易金额")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 2, createStyle(numFmt = "DATE"), rows = seq_len(nrow(pos)) + 1,
         cols = which(colnames(trans) %in% c("交易日期")),
         stack = TRUE, gridExpand = TRUE)
setColWidths(wb, sheet = 2,
             cols = which(colnames(trans) %in%
                            c("交易日期", "资产类别", "资产名称", "交易类型", "交易金额")),
             widths = c(15, 20, 30, 15, 20))
showGridLines(wb, 2, showGridLines = FALSE)
## write excel
saveWorkbook(wb, path, overwrite = TRUE)
