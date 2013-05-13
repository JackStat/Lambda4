context("Angoff")
test_that('angoff and population covariance matrices',{
  
  test.val1<-as.numeric(round(angoff(cong1f)[[1]],6))
  expect_that(test.val1, equals(.855697))
  
  test.val2<-as.numeric(round(angoff(par1f)[[1]],6))
  expect_that(test.val2, equals(.888889))
  
  test.val3<-as.numeric(round(angoff(tau1f)[[1]],6))
  expect_that(test.val3, equals(.833968))
  
  
})