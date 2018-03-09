#some basic information
library(GCAMCPUB)
library(data.table)
db_mo <- activate_conn(db_mo)

# disconnect --------------------------------------------------------------

DBI::dbDisconnect(db_mo)
#how to install R packages from github?
devtools::install_github("Rdatatable/data.table")

#how to write a sql sentense without using "Paste"?
sql <- "select Port_Name, Port_Type, sum(PL_LC) as PL_LC from CORE_Data_PL
where Date = '%s' group by Port_Type, Port_Name"
sql <- sprintf(sql, tgtDate)

##what's in GCAMCPUB packages?
#activate_conn(conn_name, if_str = FALSE, if_dplyr_src = FALSE)
#na_fill()
#f_date_begin(x, unit)
#f_fmt(x, digits = 2, zero_fmt)   千位分隔符
#f_fmt_pct(x, digits = 2, zero_fmt, trim = TRUE)  百分数
#attach_pkgs(x = c("all", "data", "ggplot2", "knitr", "ts"),SuppressWarnings = !interactive())
#x只能从all,data,ggplot2,knitr,ts中选择一个，如果有多个，只取第一个

#成本户组合名称代码对照表
ULMatchTable = data.table(
  c("UL Stable", "UL Anyi", "UL Aggressive","UL Increase", "UL Strategy"),
  c("76300126", "76300158", "76300157", "76300127", "76300156"),
  c("中意稳健理财投资账户", "中意安逸稳健投资账户", "中意积极进取投资账户",
    "中意增长理财投资账户", "中意策略增长投资账户")) %>%
  setnames(c("PortName", "ULCode", "ULName"))

#add a new line





coreInfo$read("SAAInfo")
coreInfo$read("ClassInfo") # PortInfo, HSPortInfo, ClassInfo，但是没有Class_L3和SAA_CL1的关系，还是得从SQL读