test_that("testing function get_constants()", {

  l_constants <- get_constants()
  expect_equal(l_constants$vec_Natura_Beef,2)
  expect_equal(l_constants$vec_SwissPrimBeef,3)


})
