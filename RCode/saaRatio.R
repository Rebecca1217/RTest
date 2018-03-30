library(portanalytics)
library(GCAMCPUB)
library(data.table)

readFromLdc(c("201612", "201712"), "All cost based", jyDateRange = NULL)

# Note: you can't use the class_relationship key by Class_L3, because bond and stock conatins
# both domestic and overseas. You have to dive into the underlying pos for SAA proportion.

tgt_date <- as.Date("2016-12-31")
# exclude Capital FX
# saa_alloc <-  astAlloc(paPorts(list(c("CNPC", "Par", "Life", "Capital RMB", "UV Individual", "UV Group")
#                                    ), "Cost based"),
#                       dates = tgt_date,
#                       groupBy = "Class_L3") %>%
#   setnames("Group", "Class_L3")


saa_cl1 <- dataCenter$pos[Date == tgt_date, .(AV = sum(AV_Mix_LC)), by = .(SAA_CL1)]
saa_cl1[, Ratio := AV / sum(AV)]

saa_cl2 <- dataCenter$pos[Date == tgt_date, .(AV = sum(AV_Mix_LC)), by = .(SAA_CL2)]
saa_cl2[, Ratio := AV / sum(AV)]




