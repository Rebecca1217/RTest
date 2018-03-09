library(portanalytics)
library(data.table)
library(GCAMCPUB)
readFromLdc(c("201512", "201710"), "Stable No2", jyDateRange = NULL)

pos <- dataCenter$pos[, .(AV = sum(AV_Mix_LC)), by = .(Date, Class_L3_)]


# astAllo -----------------------------------------------------------------

astAllo <- astAlloc(paPorts(list("Stable No2"), "Stable No2"),
                    c(as.Date("2015-12-11"), as.Date("2017-10-31")),
                    ts = TRUE, groupBy = "Class_L3_CN")
astAllo <- dcast(astAllo, Date_To ~ Group, value.var = "Weight")
write_open_xlsx(astAllo) # ST financial is classified as corporate bond


# rating ------------------------------------------------------------------


rating <- astAlloc(paPorts(list("Stable No2"), "Stable No2"),
                   c(as.Date("2015-12-11"), as.Date("2017-10-31")),
                   ts = TRUE ,groupBy = "Rating_External")
rating <- dcast(rating, Date_To ~ Group, value.var = "Weight")
write_open_xlsx(rating)
