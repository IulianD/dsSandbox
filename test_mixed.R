#logindata <- read.delim('/home/iulian/datashield/logindata_proxy.txt')

x <- dssCreateFakeServers(all_nodes,c('fake1', 'fake2'))
 opals <- datashield.login(x)

#opals <- datashield.login(logindata[logindata$server %in% c('abos', 'colaus'),])

datashield.aggregate(opals['fake1'], as.symbol('partial.data("iris", 1, 40)'))
datashield.aggregate(opals['fake2'], as.symbol('partial.data("iris", 41, 100)'))
login_1_2 <-'datashield.login("fake2")'
login_2_1 <-'datashield.login("fake1")'
datashield.assign(opals['local1'],'my_mates', as.symbol(login_1_2))
datashield.assign(opals['local2'],'my_mates', as.symbol(login_2_1))


datashield.symbols(opals)
all_nodes$locals$local1$envir$my_mates
all_nodest$locals$local2$envir$my_mates
ds.summary('iris', opals)
ds.mean('iris$Sepal.Length', type = 'combine', datasources = opals)


