logindata <- read.delim('/home/iulian/datashield/logindata_proxy.txt')

dssCreateFakeServers(2)
opals <- datashield.login()
opals <- datashield.login(logindata[logindata$server %in% c('abos', 'colaus'),])

datashield.aggregate(opals['local1'], as.symbol('partial.data("iris", 1, 40)'))
datashield.aggregate(opals['local2'], as.symbol('partial.data("iris", 41, 80)'))
datashield.aggregate(opals['abos'], as.symbol('partial.data("iris", 81, 120)'))
datashield.aggregate(opals['colaus'], as.symbol('partial.data("iris", 121, 150)'))
datashield.symbols(opals)
ds.summary('iris', opals)
ds.mean('iris$Sepal.Length', type = 'combine', datasources = opals)


