library(portanalytics)
library(data.table)
library(GCAMCPUB)
readFromLdc(c("201510", "201710"), c("Stable No2", "Credit Bond No2"), jyDateRange = NULL)
pos <- attachHSPortInfo(dataCenter$pos, "Port_Name")

stable_pos <- pos[Port_Name == "Stable No2"]