library(portanalytics)
library(data.table)
library(GCAMCPUB)
readFromLdc(c("201510", "201710"), c("Stable No2", "Credit Bond No2"), jyDateRange = NULL)
pos <- attachHSPortInfo(dataCenter$pos, "Port_Name")

port <- "Stable No2"
dates <- c(ymd(20151101), ymd(20171031))

stable_pos <- pos[Port_Name == "Stable No2"]
stable_ast <- astAlloc(paPorts(as.list(port), port), dates = dates,
                       ts = TRUE, groupBy = "Class_L3_CN")
stable_ast[, Class := Group]
stable_ast[Group %in% c("货币型基金", "货币类保险资管产品(净值型)"),
           Class := "货币类资产"]
stable_ast[Group %in% c("A级基金", "普通债基"),
           Class := "债券基金"]
stable_ast <- dcast(stable_ast, Date_To~Class, fun = sum, value.var = "Weight")
write_open_xlsx(stable_ast)

port <- "Credit Bond No2"
tmp <- unitPrice(paPorts(list(port), port), dates, .est = TRUE)
write_open_xlsx(tmp)

