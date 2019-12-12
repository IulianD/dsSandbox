logindata <- read.delim('/home/iulian/datashield/logindata_proxy.txt')
#devtools::load_all()
#rm(all_conns)
#x <- dssCreateFakeServers('all_conns',c('fake1', 'fake2'))
#opals <- datashield.login(x)
#expect_equal(class(local_nodes), 'list', TRUE)
#expect_s3_class(local_nodes$locals$fake1, 'local', TRUE)

opals <- datashield.login(x,logindata[logindata$server %in% c('abos', 'colaus'),, drop=FALSE])

partial_data <- function (what, start = NULL, end = NULL) 
{
  data(list = list(what), envir = environment())
  my.df <- get(what)
  if (is.null(start)) {
    start <- 1
  }
  if (is.null(end)) {
    end <- nrow(my.df)
  }
  my.df <- my.df[start:end, ]
  assign(what, my.df, pos = parent.frame())
  TRUE
}

datashield.aggregate(opals['fake1'], as.symbol('partial.data("iris", 1, 40)'))
datashield.aggregate(opals['fake2'], as.symbol('partial.data("iris", 41, 100)'))
datashield.aggregate(opals['real1'], as.symbol('partial.data("iris", 101, 150)'))
ds.summary('iris', datasources = opals)
ds.mean('iris', datasources = opals)
datashield.logout(opals)


expect_true(datashield.aggregate(opals['fake1'], as.symbol('partial.data("iris", 1, 40)'))[[1]])
expect_true(datashield.aggregate(opals['fake2'], as.symbol('partial.data("iris", 41, 100)'))[[1]])
login_1_2 <-'datashield.login("fake2")'
login_2_1 <-'datashield.login("fake1")'
datashield.assign(opals['fake1'],'my_mates', as.symbol(login_1_2))
datashield.assign(opals['fake2'],'my_mates', as.symbol(login_2_1))


expect_equal(datashield.symbols(opals),sapply(local_nodes$locals, function(x) ls(x$envir), simplify = FALSE))
expect_equal(c(fake1=40, fake2=60), sapply(ds.summary('iris', opals), function(x) x$`number of rows`))
datashield.logout(opals)

