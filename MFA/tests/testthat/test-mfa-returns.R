context("Test cases for testing the return value of mfa function")

test_that("mfa functions returns an object of class mfa", {
  url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
  data <- read.csv(url)
  mfa_local <- mfa(data = data, sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54),
                 ncomps = 2, center = TRUE, scale = TRUE)
  expect_is(mfa_local, "mfa")
})

test_that("mfa function returns correct eigenvalues for the fixed data-set", {
  url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
  data <- read.csv(url)
  mfa_local <- mfa(data = data, sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54),
                   ncomps = 2, center = TRUE, scale = TRUE)
  
  expectedEigenValuesFromPaper = c(0.77025513, 0.12292544, 0.09071052, 0.07601535, 0.05960069, 0.03920317, 0.03090963, 0.02495849, 0.01866122, 0.01343745, 0.01129973)
  expect_equal(expectedEigenValuesFromPaper, mfa_local$EigenValues)
})

test_that("mfa function returns correct common factor scores for the fixed data-set", {
  url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
  data <- read.csv(url)
  mfa_local <- mfa(data = data, sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54),
                   ncomps = 2, center = TRUE, scale = TRUE)
  
  expectedCommonFactorFromPaperCol1 = c(-0.98020575, -0.80886515, -0.76100584, -1.11498367,  1.37275684,  1.26401538,  0.80828274,  0.92534231, -0.66895382,  0.07316059, -0.47610885,  0.36656519)
  expectedCommonFactorFromPaperCol2 = c(0.16325474,  0.03262348, -0.45418702, -0.16586214, -0.12838880, -0.10813651,  0.20466790,  0.40775212,  0.36852275, -0.75677932, 0.51276640, -0.07623359)
  expect_equal(expectedCommonFactorFromPaperCol1, mfa_local$CompromiseFactorScores[,1])
  expect_equal(expectedCommonFactorFromPaperCol2, mfa_local$CompromiseFactorScores[,2])
})

test_that("mfa function returns correct factor loadings for the fixed data-set", {
  url <- "https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv"
  data <- read.csv(url)
  mfa_local <- mfa(data = data, sets = list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54),
                   ncomps = 2, center = TRUE, scale = TRUE)
  
  expectedLoadingFromPaperCol1 = c(-0.2943913, -0.2665541, -0.2599726,  0.2411168,  0.2863237, -0.2329070, -0.2972296, -0.2962409, -0.2672379,  0.2561528, -0.2383978, -0.2222161, -0.3047298, -0.1357863, -0.2584869,  0.2033337, -0.2772168,  0.2670554, -0.3133599, -0.2606245, -0.3032593,  0.2304584, -0.2054184, -0.2960030, -0.2126013, -0.2684987,  0.1235185, -0.2585636,  0.1765244, -0.3019274, -0.2774750, -0.2647668,  0.2305126, -0.2045797, -0.2748806, -0.2463005, -0.2765681,  0.1802094, -0.2758745, -0.2471469, -0.2353831,  0.1384297, -0.2861113,  0.2391207, -0.3034530, -0.2345574, -0.2870075,  0.2505574, -0.2961573, -0.3225824, -0.2740603, -0.2861538,  0.2822906)
  expect_true(all.equal(expectedLoadingFromPaperCol1, mfa_local$MatrixLoadings[, 1], tolerance = .00001))
})



