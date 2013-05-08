context("Quantile Lambda4")
test_that('Quantile Lambda4 and population covariance matrices',{
    
  test.val2<-as.numeric(round(quant.lambda4(oneFpar)[[1]][[1]],6))
  expect_that(test.val2, equals(.888889))
  
  test.val3<-as.numeric(round(quant.lambda4(oneFtau)[[1]][[1]],6))
  expect_that(test.val3, equals(.833575))
  
  
})