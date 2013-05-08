context("Angoff")
test_that('angoff and population covariance matrices',{
  
  angoff.val1<-as.numeric(round(angoff(oneFcong)[[1]],6))
  expect_that(angoff.val1, equals(.855697))
  
  angoff.val2<-as.numeric(round(angoff(oneFpar)[[1]],6))
  expect_that(angoff.val2, equals(.888889))
  
  
})