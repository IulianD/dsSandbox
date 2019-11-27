#logindata <- read.delim('/home/iulian/datashield/logindata_proxy.txt')

x <- dssCreateFakeServers('local_nodes',c('fake1', 'fake2'))
opals <- datashield.login(x)
expect_equal(class(local_nodes), 'list', TRUE)
expect_s3_class(local_nodes$locals$fake1, 'local', TRUE)

#opals <- datashield.login(logindata[logindata$server %in% c('abos', 'colaus'),])

expect_true(datashield.aggregate(opals['fake1'], as.symbol('partial.data("iris", 1, 40)'))[[1]])
expect_true(datashield.aggregate(opals['fake2'], as.symbol('partial.data("iris", 41, 100)'))[[1]])
login_1_2 <-'datashield.login("fake2")'
login_2_1 <-'datashield.login("fake1")'
datashield.assign(opals['fake1'],'my_mates', as.symbol(login_1_2))
datashield.assign(opals['fake2'],'my_mates', as.symbol(login_2_1))


expect_equal(datashield.symbols(opals),sapply(local_nodes$locals, function(x) ls(x$envir), simplify = FALSE))
expect_equal(c(fake1=40, fake2=60), sapply(ds.summary('iris', opals), function(x) x$`number of rows`))

