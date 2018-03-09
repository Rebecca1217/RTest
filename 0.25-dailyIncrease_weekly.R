
# pkgs --------------------------------------------------------------------

library(data.table)
library(portanalytics)
library(GCAMCPUB)
# library(readxl)
library(openxlsx)

# para --------------------------------------------------------------------

email_pwd <- "gcamc03!"

last_pos_date <- (function() {
  db_mo <- GCAMCPUB::activate_conn(db_mo)
  on.exit(DBI::dbDisconnect(db_mo))
  sql <- "select max(Date) as Max_Date from CORE_Data_Holding"
  max_date <- DBI::dbGetQuery(db_mo, sql)$Max_Date %>% to_date
  max_date
})()

date_tgt <- last_pos_date
pn <- "Daily Increase"
ports <- paPorts(pn)
readFromLdc(date_tgt, pn, jyDateRange = NULL)

# prepare the data --------------------------------------------------------

pos <- dataCenter$pos[Date == date_tgt] %>%
  attachHSPortInfo("Port_Name")


# Ast reClassification
# note the IAMPs are mostly buying non-standard, here we c
fi <- coreInfo$read("ClassInfo")[Class_L1 == "Fixed income" & 
                                   ! Class_L3 %in% c("Debt plan", "Trust", "CD")]$Class_L3
lq <- c(coreInfo$read("ClassInfo")[Class_L1 == "Liquidity"]$Class_L3, "CD")
fb <- c("Debt plan", "Trust")
eq <- coreInfo$read("ClassInfo")[Class_L1 == "Equity"]$Class_L3

pos[Class_L3_FundinDetail2 %in% fi, Re_Class := "固定收益类资产"]
pos[Class_L3_FundinDetail2 %in% lq, Re_Class := "现金及流动性资产"]
pos[Class_L3_FundinDetail2 %in% fb, Re_Class := "非标资产"]
pos[Class_L3_FundinDetail2 %in% eq, Re_Class := "权益类资产"]

alloc <- pos[Date == date_tgt, .(Amt = sum(AV_Mix_LC)), by = .(Re_Class)]
alloc[, Pct := Amt / sum(Amt)]
setnames(alloc, c("资产分类", "资产净值", "占比"))

# bond Rating distribution
bond_rating <- pos[Class_L3 %in% fi, .(Amt = sum(AV_Mix_LC)), by = .(Rating_External)]

# final table

lev <- pos[Class_L3 != "Repo", .(Amt = sum(AV_Mix_LC))]$Amt / pos[, .(Amt = sum(AV_Mix_LC))]$Amt - 1
mod_d <- pos[!is.na(ModD_Mix), .(ModD_Mix = sum(ModD_Mix * AV_Mix_LC) / sum(AV_Mix_LC))]$ModD_Mix
res <- data.table(Ast = sum(alloc$`资产净值`),
                  Duration = round(mod_d, 2),
                  Leverage = lev) %>%
  setnames(c("资产净值", "修正久期", "杠杆比率"))

# write excel -------------------------------------------------------------

styleHeader <-
  createStyle(valign = "center", halign = "center", wrapText = TRUE,
              border = "TopBottomLeftRight", fgFill = "grey")

wb <- createWorkbook("daily_increase")
addWorksheet(wb, "Summary")
writeData(wb, 1, res, borders = "all", headerStyle = styleHeader)
addStyle(wb, 1, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(res)) + 1,
         cols = which(colnames(res) %in% c("资产净值")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "PERCENTAGE"), rows = seq_len(nrow(res)) + 1,
         cols = which(colnames(res) %in% c("杠杆比率")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 1, createStyle(numFmt = "NUMBER"), rows = seq_len(nrow(res)) + 1,
         cols = which(colnames(res) %in% c("修正久期")),
         stack = TRUE, gridExpand = TRUE)

setColWidths(wb, sheet = 1,
             cols = which(colnames(res) %in%
                            c("资产净值", "修正久期", "杠杆比率")),
             widths = c(30, 10, 10))
showGridLines(wb, 1, showGridLines = FALSE)


addWorksheet(wb, "Allocation")
writeData(wb, 2, alloc, borders = "all", headerStyle = styleHeader)
addStyle(wb, 2, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(alloc)) + 1,
         cols = which(colnames(alloc) %in% c("资产净值")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 2, createStyle(numFmt = "PERCENTAGE"), rows = seq_len(nrow(alloc)) + 1,
         cols = which(colnames(alloc) %in% c("占比")),
         stack = TRUE, gridExpand = TRUE)

setColWidths(wb, sheet = 2,
             cols = which(colnames(alloc) %in%
                            c("资产分类", "资产净值", "占比")),
             widths = c(30, 30, 10))
showGridLines(wb, 2, showGridLines = FALSE)

excel_add <- 
  paste0("中意资产-日日增利周报", format(date_tgt, "%Y.%m.%d"), ".xlsx")
excel_add <- 
  file.path(
    "T:/2_共享区/4_组合管理部_Portforlio_Management/",
    "4_报告_Reporting/4_Product Report/Daily Increase/weekly",
    excel_add
  )

saveWorkbook(wb, excel_add, overwrite = TRUE)

# output ------------------------------------------------------------------

htmlBody <- "
<body style='color:black;font-size:10pt;font-family:微软雅黑;'>
<p>Dear:</p>
<p>您好！截至{{date}}，日日增利账户投资情况如附件。同业存单划分为流动性类资产，保险资管产品划分为非标类资产请知悉。
</p>
<p>注：产品净值等数据均为初步核算数据，仅供参考。</p>
<p>(本邮件系程序自动发送，请勿直接回复。)</p>
</br>
<p class='small'> <b>中意资产管理有限责任公司</b></p>
<p class='small' style='font-family:Segoe UI Semibold;'>
<b>Tel:</b> (010) 56801188 - 8833</p>
<p class='small' style='font-family:Segoe UI Semibold;'>
<b>Email:</b> <a href='mailto:Customer.Service@gc-amc.com'>
Customer.Service@gc-amc.com</a></p>
</body>"
htmlBody <- infuser::infuse(htmlBody, 
                            date = format(date_tgt, "%Y年%m月%d日"))
f_send_email(
  from = "Ruiling.Feng@gc-amc.com",
  to = c("Qi.Shi@gc-amc.com", "Andy.Wang@gc-amc.com"),
  bcc = c("Ruiling.Feng@gc-amc.com"),
  id = "AMC161",
  pwd = email_pwd,
  subject = paste("【自动邮件】日日增利账户-周度报送",
                  format(date_tgt, "%Y.%m.%d")),
  body = htmlBody,
  attachments = excel_add
)

