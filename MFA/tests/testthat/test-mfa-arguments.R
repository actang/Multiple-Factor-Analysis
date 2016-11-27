context("Test cases for testing that MFA functions handles arguments correctly")

test_that("test first 'data' argument of the mfa function",  {
  expect_error(checkData(c(1, 2, 4)))
  expect_true(checkData(data.frame(x = numeric(10))))
})

test_that("test second 'sets' argument of the mfa function", {
  expect_error(checkSets(c(1, 2)))
  expect_true(checkSets(list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)))
})

test_that("test third 'ncomps' argument of the mfa function", {
  #make sure that the ncomps is less than the number of possible components and its numeric
  expect_error(checkNComponents("2"))
  expect_error(checkNComponents(2.9))
  expect_true(checkNComponents(2))
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



