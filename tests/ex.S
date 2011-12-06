library(S4DBMSInterface)
sp = RDBMSTable("SP", dbConnect(dbDriver("MySQL"), "Date", group = "gcox"))

sp[QTY <= 300 & S %in% c("S1", "S2"), ] 

sp[, c(1, 3)]

sp[, c("S", "P")]

sp[c(1, 3), ]

sp[c(1, 3),  c("S", "P")]


