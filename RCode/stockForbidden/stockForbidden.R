
library(data.table)
library(GCAMCPUB)
library(readxl)
library(rvest)

inputDate <- as.Date("2017-02-03")
inputDate2 <- "2017-2-3"
# inputCode <- "601216.SH"
# inputCode <- stringr::str_sub(inputCode, 1, 6)
# read data ---------------------------------------------------------------
# Wind
path <- paste0("T:/2_共享区/5_风控合规部_Risk_control/Jorion 合规审核留档/9.O32禁投池/保险资金禁止投资股票Wind ", 
inputDate2, ".xlsx")
poolWind <- readxl::read_excel(path, 1) %>% 
  setDT() %>%
  setnames(c("Sec_Code", "Sec_Name", "Date", "IF_Forbidden", "Reason"))
poolWind <- poolWind[1:(nrow(poolWind) - 2), ]
poolWind[, `:=`(
  Reason = enc2native(Reason),
  Date = as.character(Date),
  Sec_Code = as.numeric(stringr::str_sub(Sec_Code, 1, 6)),
  Source = "Wind"
)]
poolWind <- poolWind[Reason != "其上市公司已披露业绩大幅下滑、严重亏损或者未来将出现严重亏损的;", 
                     .(Date, Sec_Code, Sec_Name, Reason, Source)]
# Caihui:poolCaihui need convering reasons from code to words
path <- paste0("T:/2_共享区/5_风控合规部_Risk_control/Jorion 合规审核留档/9.O32禁投池/保险资金禁止投资的股票Caihui ",
inputDate2, "_.xls")
poolCaihui <- read_html(path, encoding = "GB2312") %>%
  html_node("#MainTable") %>% 
  html_table(header = TRUE) %>%
  setDT() %>%
  setnames(c("Num", "Date", "Sec_Code", "Sec_Name", "Reason"))
poolCaihui[, `:=`(
  Reason = {
    Reason %>%
      enc2native() %>%
      stringr::str_replace_all(stringr::fixed("<U+00A0>"), "")
  },
  Source = "Caihui"
)]
poolCaihui <- poolCaihui[Reason != "E", ]
poolCaihui <- local({
  def <- data.table(Reason = LETTERS[1:6], 
                    Meaning = c(
                      "特别处理股票", 
                      "警示存在终止上市风险的特别处理",
                      "已终止上市的股票",
                      "最近一年度内财务报表被会计师事务所出具拒绝表示意见或者保留意见的股票",
                      "已披露业绩大幅下滑、严重亏损或者未来将出现严重亏损的股票",
                      "正在接受监管部门调查或者最近1年内受到监管部门严重处罚的股票（特别提醒，在这个类型中我们将上市公司被交易所公开谴责、上市公司被证监会（局）、司法机关稽查、监管关注、通报批评等事项也纳入其中了"
                    ),
                    key = "Reason")
  rs <- unique(poolCaihui$Reason)
  tmpMatch <- function(rs_) {
    cRs <- stringr::str_split_fixed(rs_, "", Inf)
    cRs <- unique(as.character(cRs))
    data.table(
      Meaning = def[J(cRs), paste0(Meaning, collapse = ",")]
    )
  }
  dtRs <- lapply(setNames(rs, rs), tmpMatch) %>% rbindlist(idcol = "Reason")
  dtRs[copy(poolCaihui), on = "Reason"][, 
                                        .(Date, Sec_Code, Sec_Name, Meaning, Source)] %>%
    setnames("Meaning", "Reason")
})
# Conmbine 2 pools
poolForbidden <- rbindlist(list(poolWind, poolCaihui), use.names = TRUE, fill = TRUE)

write_open_xlsx(poolForbidden)
# find if in the forbidden pool, if TRUE, print it
checkResult <- poolForbidden[Sec_Code == inputCode, ]
if (nrow(checkResult) == 0) {
  GCAMCPUB::f_msg("The new stock is NOT in the fobidden pool.")
} else {
  checkResult <- checkResult[, .(Date, Sec_Code, Sec_Name, Reason, Source)]
  print(checkResult)
}