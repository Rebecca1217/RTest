
# DESC ---------------------------

# A program for calculating impairment of Capital FX Portfolio
# it can be used for other portfolio as well.
# 

# Load ---------------------------

library(GCAMCPUB)
library(openxlsx)
attach_pkgs("data")
email_pwd <- "gcamc12!"
last_pos_date <- local({
  db_mo <- activate_conn("db_mo")
  on.exit(DBI::dbDisconnect(db_mo), add = TRUE)
  sql <- "Select Max(date) as Max_Date from CORE_Data_Holding"
  max_date <- DBI::dbGetQuery(db_mo, sql)
  to_date(max_date$Max_Date)
})

# date_based <- as.Date("2017-12-18") # the date should be real date in Holding, not season end!
date_based <- last_pos_date
db_mo <- GCAMCPUB::activate_conn(db_mo)
# on.exit(DBI::dbDisconnect(db_mo), add = TRUE)

# parameter -------------------------
test_quarter <- data.table::quarter(date_based)
date_from <- switch(test_quarter,
                    "-03-31",
                    "-06-30",
                    "-09-30",
                    "-12-31")
date_from <- as.Date(paste0(data.table::year(date_based) - 1, date_from))
uglr_criteria <- 0.25

# Download Data ----------------------

sql <- "select Date, Port_Name, Class_L3, Sec_Code, Original_Currency,
max(Port_Order) as Port_Order, max(Sec_Name) as Sec_Name, 
sum(Quantity) as Quantity, sum(Book_Value_OC) as BV_OC, 
sum(AV_Mix_OC) as AV_Mix_OC, sum(UGL_OC) as UGL_OC,
sum(Book_Value_LC) as BV_LC, sum(AV_Mix_LC) as AV_Mix_LC,
sum(UGL_LC) as UGL_LC from CORE_Data_Holding
where Date > '{{date_from}}' and Date <= '{{date_based}}' and IAS = 'AFS'
and (Class_L1 = 'Equity' or if_Fund = 1) and if_GCL_Port = 1
group by Date, Port_Name, Class_L3, Sec_Code, Original_Currency"
sql <- infuser::infuse(sql, date_from = date_from, date_based = date_based)
dat <- DBI::dbGetQuery(db_mo, sql)
dat <- dat %>% dplyr::mutate(Date = GCAMCPUB::to_date(Date))

sql <- "select Date from CORE_Data_Dates where 
Date > '{{date_from}}' and Date <= '{{date_based}}'"
sql <- infuser::infuse(sql, date_from = date_from, date_based = date_based)
date_seq <- DBI::dbGetQuery(db_mo, sql)
date_seq <- date_seq %>% dplyr::mutate(Date = GCAMCPUB::to_date(Date))
date_seq <- date_seq$Date 

tmp_fun <- function(x) {
  x <- dplyr::arrange(x, Date)
  
  # ugl <0 for a year
  if (nrow(x) < length(date_seq)) {
    flag_1 <- FALSE
  } else {
    if (all(x$UGL_OC < 0)) {
      flag_1 <- TRUE
    } else {
      flag_1 <- FALSE
    }
  }
  
  # ugl ratio < -0.3
  y <- dplyr::filter(x, Date == date_based)
  if (nrow(y) == 0) {
    flag_2 <- FALSE
  } else {
    if (y$UGL_OC / y$BV_OC < -uglr_criteria) {
      flag_2 <- TRUE
    } else {
      flag_2 <- FALSE
    }
  }
  
  memo <- ""
  if (flag_1) memo <- paste0(memo, "连续1年亏损")
  tmp <- ""
  if (nchar(memo) > 0) tmp <- "; "
  if (flag_2) memo <- paste0(memo, tmp, sprintf("亏损大于%s", GCAMCPUB::f_fmt_pct(uglr_criteria)))
  tmp <- data.table::last(x)
  tmp <-
    tmp %>%
    dplyr::mutate(Memo = memo) %>%
    dplyr::select(Quantity, BV_OC, AV_Mix_OC, UGL_OC, BV_LC, AV_Mix_LC, UGL_LC, Memo) 
  tmp
}

code_name <- unique(data.table(dat)[, .(Sec_Code, Sec_Name)])

local({
  
  setDT(dat)
  dat[Sec_Code == "161835.OF", Class_L3 := "Bond fund"] # dut to YinHe fenlei change
  code_class <- unique(dat[, .(Sec_Code, Class_L3)])
  n1 <- nrow(code_class)
  n2 <- length(unique(code_class$Sec_Code))
  if(n1 != n2) stop("Same Sec_Code with different Class_L3!")
  setDF(dat)
})


r <-
  dat %>%
  dplyr::group_by(Port_Order, Port_Name, Class_L3,
                  Sec_Code, Original_Currency) %>%
  dplyr::do(tmp_fun(.)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(nchar(Memo) > 0) %>%
  dplyr::arrange(Port_Order, Class_L3, Sec_Code)

data.table::setDT(r)

r <- code_name[r, on = "Sec_Code", mult = "first"][, .(Port_Order, Port_Name, Class_L3, Sec_Code,
                                                       Sec_Name, Quantity, Original_Currency, BV_OC, AV_Mix_OC,
                                                       UGL_OC,BV_LC, AV_Mix_LC, UGL_LC, Memo)]


# r_bck <- copy(r)

r[, `:=`(
  Price_Now = AV_Mix_OC / Quantity,
  Price_Buy = BV_OC / Quantity,
  UGL_Ratio = UGL_OC / BV_OC
)]

r[, Label := ifelse((grepl("连续1年亏损", Memo) | UGL_Ratio < -0.3), "确定计提", "可能计提")]

res <- r[, .(UGL_LC = sum(UGL_LC)), by = .(Port_Name, Label)] %>%
  dcast(Port_Name~Label, fun = sum, value.var = "UGL_LC")

sql <- "select Port_Name, Class_L3, (case when UGL_LC > 0 then UGL_LC else 0 end) as UGL_Plus from CORE_Data_Holding
where Port_Type in ('cost based', 'cost based FC')
and date = '{{date}}'
and IAS = 'AFS'
"
sql <- infuser::infuse(sql, date = date_based)
ugl_plus <- DBI::dbGetQuery(db_mo, sql) %>% setDT()

ugl_plus[Class_L3 %in% c("Stock", "IAMP-Stock"), Class := "Stock"]
ugl_plus[Class_L3 %in% c("Active fund", "Index fund", "REITs"), Class := "Stock fund"]
ugl_plus[Class_L3 %in% c("Bond fund"), Class := "Bond fund"]

res_ugl_plus <- ugl_plus[, .(UGL_Plus = sum(UGL_Plus)), by = .(Port_Name, Class)] %>%
  dcast(Port_Name~Class, fun = sum, value.var = "UGL_Plus")

DBI::dbDisconnect(db_mo)


port_seq <- data.table(Port_Name = c("CNPC", "Par", "Life", "Capital RMB", "UV Individual",
                                     "UV Group", "Capital FX", "CNPC-UV"))

output1 <- res[port_seq, on = "Port_Name"]
output1 <- res_ugl_plus[output1, on = "Port_Name"]
output1 <- output1[, .(Port_Name, 确定计提, 可能计提, Stock, `Stock fund`, `Bond fund`)] %>% 
  setNames(c("Port_Name", "确定计提", "可能计提", "股票浮盈", "权益基金浮盈", "债券基金浮盈"))
output1[, `:=`(
  确定计提 = GCAMCPUB::na_fill(确定计提, 0),
  可能计提 = GCAMCPUB::na_fill(可能计提, 0)
)]
add_total <- output1[, .(Port_Name = "Total", 确定计提 = sum(确定计提), 
                         可能计提 = sum(可能计提), 股票浮盈= sum(股票浮盈), 
                         权益基金浮盈 = sum(权益基金浮盈), 债券基金浮盈 = sum(债券基金浮盈))]
output1 <- rbind(output1, add_total)

output2 <- r[, .(Port_Name, Class_L3, Sec_Code, Sec_Name, Quantity, Original_Currency,
                 BV_OC, AV_Mix_OC, UGL_OC, AV_Mix_LC, UGL_LC, Memo, Price_Now,
                 Price_Buy, UGL_Ratio, Label)] %>%
  setnames(c("Price_Now", "Price_Buy"), c("当前价格(原币)","平均买入价格(原币)"))

# write excel -------------------------------------------------------------

styleHeader <-
  createStyle(valign = "center", halign = "center", wrapText = TRUE,
              border = "TopBottomLeftRight", fgFill = "grey")

wb <- createWorkbook("impairment_daily")
addWorksheet(wb, "汇总信息")
writeData(wb, 1, output1, borders = "all", headerStyle = styleHeader)
addStyle(wb, 1, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(output1)) + 1,
         cols = which(!colnames(output1) %in% "Port_Name"),
         stack = TRUE, gridExpand = TRUE)
setColWidths(wb, sheet = 1,
             cols = which(colnames(output1) %in% 
                            c("Port_Name", "确定计提", "可能计提", "股票浮盈",
                              "权益基金浮盈", "债券基金浮盈")),
             widths = c(20, 20, 20, 20, 20, 20))
showGridLines(wb, 1, showGridLines = FALSE)
# setColWidths(wb, sheet = 1, cols = seq_len(ncol(avgPos)), widths = "auto")
addWorksheet(wb, "明细")
writeData(wb, 2, output2, borders = "all", headerStyle = styleHeader)
addStyle(wb, 2, createStyle(numFmt = "ACCOUNTING"), rows = seq_len(nrow(output2)) + 1,
         cols = which(colnames(output2) %in% c("BV_OC", "AV_Mix_OC", "UGL_OC",
                                               "AV_Mix_LC", "UGL_LC",
                                               "当前价格(原币)", "平均买入价格(原币)")),
         stack = TRUE, gridExpand = TRUE)
addStyle(wb, 2, createStyle(numFmt = "PERCENTAGE"), rows = seq_len(nrow(output2)) + 1,
         cols = which(colnames(output2) %in% c("UGL_Ratio")),
         stack = TRUE, gridExpand = TRUE)
setColWidths(wb, sheet = 2,
             cols = which(colnames(output2) %in% 
                            c("Port_Name", "Class_L3", "Sec_Code", "Sec_Name", "Quantity",
                              "Original_Currency", "BV_OC", "AV_Mix_OC", "UGL_OC",
                              "AV_Mix_LC", "UGL_LC", "Memo", "当前价格(原币)", 
                              "平均买入价格(原币)", "UGL_Ratio", "Label")),
             widths = c(15, 15, 15, 25, 15, 18, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20))
showGridLines(wb, 2, showGridLines = FALSE)

path <- file.path("E:/工作文件/[routine]Allen_季度计提减值准备")
path_excel <- format(date_based, "%Y.%m.%d") %>%
  sprintf("%s减值准备及可实现浮盈测算.xlsx", .) %>%
  file.path(path, .)
saveWorkbook(wb, path_excel, overwrite = TRUE)


# email sending -----------------------------------------------------------

htmlBody <- "
<body style='color:black;font-size:10pt;font-family:微软雅黑;'>
<p>Dear:</p>
<p>您好！附件是
<span style='color:red;font-weight:700;'>减值准备及可实现浮盈测算(#date#)，
</span>
<span style='color:black;font-weight:300'></span>
请查阅。
</br>
<p class='small'> <b>中意资产管理有限责任公司</b></p>
<p class='small' style='font-family:Segoe UI Semibold;'>
<b>Tel:</b> (010) 56801188 - 8833</p>
<p class='small' style='font-family:Segoe UI Semibold;'>
<b>Email:</b> <a href='mailto:Customer.Service@gc-amc.com'>
Customer.Service@gc-amc.com</a></p>
</body>"
htmlBody <- gsub('#date#', format(date_based, "%Y.%m.%d"), htmlBody, fixed = TRUE)
GCAMCPUB::f_send_email(
  from = "Ruiling.Feng@gc-amc.com",
  id = "AMC161",
  pwd = email_pwd,
  to = c("Joe.Zhou@gc-amc.com", "Jie.Wang@gc-amc.com", "Yu.Lu@gc-amc.com",
         "Michelle.Zhou@gc-amc.com", "DongQing.Hu@gc-amc.com"),
  cc = "Xiaojun.Nie@gc-amc.com",
  bcc = "Ruiling.Feng@gc-amc.com",
  subject = paste("减值准备及可实现浮盈测算",
                  format(date_based, "%Y.%m.%d")),
  body = htmlBody,
  attachments = c(path_excel)
)

# DONE --------------------------------------------------------------------

