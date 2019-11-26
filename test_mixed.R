#logindata <- read.delim('/home/iulian/datashield/logindata_proxy.txt')

x <- dssCreateFakeServers(2)
 opals <- datashield.login(c('local1', 'local2'))

#opals <- datashield.login(logindata[logindata$server %in% c('abos', 'colaus'),])

datashield.aggregate(opals['local1'], as.symbol('partial.data("iris", 1, 40)'))
datashield.aggregate(opals['local2'], as.symbol('partial.data("iris", 41, 100)'))
login_1_2 <-'datashield.login("local2", cross_connect = TRUE)'
login_2_1 <-'datashield.login("local1", cross_connect = TRUE)'
datashield.assign(opals['local1'],'my_mates', as.symbol(login_1_2))
datashield.assign(opals['local2'],'my_mates', as.symbol(login_2_1))


datashield.symbols(opals)
.connection_object$locals$local1$envir$my_mates
.connection_object$locals$local2$envir$my_mates
ds.summary('iris', opals)
ds.mean('iris$Sepal.Length', type = 'combine', datasources = opals)


