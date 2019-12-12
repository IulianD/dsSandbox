x <- dssCreateFakeServers('on', servers = 2)
opals <- datashield.login(c('local1', 'local3', x[2]))
expect_true(all(opals==c('local1', 'local2')))

sapply(opals, function(x){
  oth <- setdiff(opals, x)
  my_call <- paste0('datashield.login("', oth, '")')
  datashield.assign(x, 'mates',as.symbol(my_call))
  return(my_call)
})

my.env <- on$locals$local1$envir
assign('matesss', eval(value, envir = my.env), envir = my.env)

value <- parse(text = 'datashield.login(bla)')

tst <- function(x){
  return(x)
}
