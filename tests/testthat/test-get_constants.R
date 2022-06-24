test_that("testing function get_constants()", {

  l_constants <- get_constants()
  expect_equal(l_constants$value_NaturaBeef,2)
  expect_equal(l_constants$value_SwissPrimBeef,3)


})
