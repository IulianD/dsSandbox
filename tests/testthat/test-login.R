test_that("local connection objects exist", {
  opals <- datashield.login(locals = list(how_many=2, opal_name = 'test_opal'))
  expect_true(is.list(test_opal))
  good_opals <- c('local1', 'local2')
  names(good_opals) <- good_opals
  attr(good_opals, 'connection_object') <- 'test_opal'
  expect_identical(opals, good_opals)
})

datashield.assign(opals['local1'], 'iris', iris[1:40,])
datashield.assign(opals['local1'], 'iris', iris[41:80,])