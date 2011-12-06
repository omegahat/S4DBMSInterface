# Using auto-incrementing for row indices

library(S4DBMSInterface)


con = dbConnect(dbDriver("MySQL"), "DateTest", group="gcox")
s = RDBMSTable("S", con)
sp = RDBMSTable("SP", con)

# With the names computed
sp.names = RDBMSTable("SP", con, class = "RDBMSTableWithNames")

sp.unames = RDBMSTable("SP", con, names = c("a", "b", "c"), 
                         class = "RDBMSTableWithNames")

class(s)
names(s)

sp[c(1, 3), ]
