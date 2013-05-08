test_that('angoff provides correct estimate for Rosenberg',{
  
  angoff.val<-as.numeric(round(angoff(Rosenberg)[[1]],6))
  
  expect_that(angoff.val, equals(.895033))
})