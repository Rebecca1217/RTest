
# pkgs --------------------------------------------------------------------

library(data.table)
library(GCAMCPUB)

# read info ---------------------------------------------------------------

#productInfo <- yaml.load_file("param.yml")
portInfo <- read.csv("para.csv") %>%
  setDT()

# para setting ------------------------------------------------------------
portName <- "Absolute Return No3"
portNameCN <- portInfo[Port_Name == portName]$Port_Name_CN
closeLine <- portInfo[Port_Name == portName]$Close_Line
warnLine <- portInfo[Port_Name == portName]$Warn_Line
address <- portInfo[Port_Name == portName]$Address
# input value
unitPrice <- 0.88

# decide the product in which status --------------------------------------

judge <- function(unitPrice, portName, warnLine, closeLine) {
  if (isTRUE(unitPrice <= closeLine)) {
    status <- "closing"
  } else if (isTRUE(unitPrice <= warnLine)) {
    status <- "warning"
  } else {
    status <- "normal"
  }
  status
}
portStatus <- judge(unitPrice, portName, warnLine, closeLine)
stopifnot(is.element(portStatus, c("normal", "warning", "closing")))

# generate the email body ---------------------------------------------
emailBody <- "
         <body style='color:black;font-size:10pt;font-family:微软雅黑;'>
         <p>尊敬的产品份额持有人:</p>
         <p>您好！
         <span style='color:red;font-weight:700;'>中意资产{{portNameCN}}产品今日净值为{{unitPrice}}，
         </span>
         <span style='color:black;font-weight:300'></span>
         {{addBody}}
         请您知悉。
         </p>
         <p>注：产品净值数据为初步核算数据，仅供参考。</p>
         <p>(本邮件系程序自动发送，请勿直接回复。)</p>
         </br>
         <p class='small'> <b>中意资产管理有限责任公司</b></p>
         <p class='small' style='font-family:Segoe UI Semibold;'>
         <b>Tel:</b> (010) 56801188 - 8833</p>
         <p class='small' style='font-family:Segoe UI Semibold;'>
         <b>Email:</b> <a href='mailto:Customer.Service@gc-amc.com'>
         Customer.Service@gc-amc.com</a></p>
         </body>"
emailSubject <- "【自动邮件】中意资产-{{portNameCN}}估值公布{{ifRisk}}{{dateCurrent}}"

emailGenerate <-  function(portStatus) {
  if (portStatus == "normal") {
    emailBody <- infuser::infuse(emailBody, portNameCN = portNameCN, 
                                 unitPrice = unitPrice, addBody = "")
    emailSubject <- infuser::infuse(emailSubject, portNameCN = portNameCN,
                                    ifRisk = "", dateCurrent = format(Sys.Date(), "%Y.%m.%d"))
  } else {
    # input share
    share <- 148500000
    tradingDate <- GCAMCPUB::get_trading_day(Sys.Date() + 15, Sys.Date(), type = "a-shares")
    tradingDay <- tradingDate[2]
    supplement <- (warnLine - unitPrice) * share
    addBody <- "产品净值已低于{{statusCN}}线，
    请于【{{tradingDay}}{{addTime}}】之前追加资金不少于【{{supplement}}元】，
    否则产品管理人将依照产品合同，
    对本产品持有的证券资产进行以市价委托等方式自主变现等操作，"
    emailBody <- infuser::infuse(emailBody, portNameCN = portNameCN, unitPrice = unitPrice,
                                 addBody = addBody)
    statusCN <- switch(portStatus,
                     "warning" = {"预警"},
                     "closing" = {"平仓"})
    addTime <- switch(portStatus,
                      "warning" = {"14:30"},
                      "closing" = {"13:00"})
    emailBody <- infuser::infuse(emailBody, statusCN = statusCN, 
                                 tradingDay = format(tradingDay, "%Y年%m月%d日"), 
                                 addTime = addTime, 
                                 supplement = GCAMCPUB::f_fmt(supplement))
    emailSubject <- infuser::infuse(emailSubject, portNameCN = portNameCN,
                                    ifRisk = "及风险提示",
                                    dateCurrent = format(Sys.Date(), "%Y.%m.%d"))
  }
  list(emailBody = emailBody, emailSubject = emailSubject)
}
emailBody <- emailGenerate(portStatus)$emailBody
emailSubject <- emailGenerate(portStatus)$emailSubject

# send email --------------------------------------------------------------
GCAMCPUB::f_send_email(
  from = "Ruiling.Feng@gc-amc.com",
  to = address,
  id = "amc161",
  pwd = "gcamc05!",
  subject = emailSubject,
  body = emailBody
)

