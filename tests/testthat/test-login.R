test_that("local connection objects exist", {
  opals <- datashield.login(locals = list(how_many=2, opal_name = 'test_opal'))
  expect_true(is.list(test_opal))
  expect_identical(opals, c('local1', 'local2'))
})
