context("Covariance Lambda4")
test_that('Covariance Lambda4 and population covariance matrices',{
  
  test.val1<-as.numeric(round(cov.lambda4(oneFcong)[[1]][[1]],6))
  expect_that(test.val1, equals(.854614))
  
  test.val2<-as.numeric(round(cov.lambda4(oneFpar)[[1]][[1]],6))
  expect_that(test.val2, equals(.888889))
  
  test.val3<-as.numeric(round(cov.lambda4(oneFtau)[[1]][[1]],6))
  expect_that(test.val3, equals(.833575))
  
  
})