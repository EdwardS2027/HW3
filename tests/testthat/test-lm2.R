test_that("lm2 works", {
  #SLR
  output=summary(lm(melb_data$Price~melb_data$Distance,melb_data))
  output2 = lm2(melb_data$Price~melb_data$Distance,melb_data)
  expect_equal(output2$residuals,as.vector(output$residuals))
  expect_equal(output2$sigma,output$sigma)
  expect_equal(output2$coefficients,output$coefficients)

  #MLR
  output=summary(lm(melb_data$Price~melb_data$Distance+melb_data$Landsize,melb_data))
  output2 = lm2(melb_data$Price~melb_data$Distance+melb_data$Landsize,melb_data)
  expect_equal(output2$residuals,as.vector(output$residuals))
  expect_equal(output2$sigma,output$sigma)
  expect_equal(output2$coefficients,output$coefficients)

  #Interaction
  output=summary(lm(melb_data$Price~melb_data$Distance+melb_data$Landsize+melb_data$Landsize*melb_data$Distance,melb_data))
  output2 = lm2(melb_data$Price~melb_data$Distance+melb_data$Landsize+melb_data$Landsize*melb_data$Distance,melb_data)
  expect_equal(output2$residuals,as.vector(output$residuals))
  expect_equal(output2$sigma,output$sigma)
  expect_equal(output2$coefficients,output$coefficients)

  #No Intercept
  output=summary(lm(melb_data$Price~-1+melb_data$Rooms,melb_data))
  output2 = lm2(melb_data$Price~-1+melb_data$Rooms,melb_data)
  expect_equal(output2$residuals,as.vector(output$residuals))
  expect_equal(output2$sigma,output$sigma)
  expect_equal(output2$coefficients,output$coefficients)

  #Missing Value - fail
  output2 = lm2(melb_data$Price~melb_data$Distance,melb_data,na.action = 'fail')
  expect_equal(output2,melb_data)

  #Missing Value - other values
  output2 = lm2(melb_data$Price~melb_data$Distance,melb_data,na.action = 'p')
  expect_equal(output2,melb_data)


})
