context("Test cases for testing that MFA functions handles arguments correctly")

test_that("test first 'data' argument of the mfa function",  {
  #give arguments that are not of type dataframe or matrix
  #expect_error()
  #give an empty data-frame or matrix and the function must give a warning
  #expect_warning()
  
})

test_that("test second 'sets' argument of the mfa function", {
  #give arguments as list of non charactor or numeric vector
  #expect_error()
})

test_that("test third 'ncomps' argument of the mfa function", {
  #make sure that the ncomps is less than the number of possible components and its numeric
  expect_error(checkNComponents("2"))
})


test_that("test fourth 'center' or fifth 'scale' argument of the mfa function", {
  #make sure that the scale argument is either TRUE, FALSE or a numeric vector
  #this is an error as the size if bigger max count
  expect_error(checkScaleOrCenter(c(1, 2, 4), 2))
  #this is correct
  expect_true(checkScaleOrCenter(c(1, 2, 4), 3))
  #this is error as it only expects numeric vectors
  expect_error(checkScaleOrCenter(c(1, "ff", "4"), 2))
  #boolean is fine
  expect_true(checkScaleOrCenter(TRUE, 2))
})



