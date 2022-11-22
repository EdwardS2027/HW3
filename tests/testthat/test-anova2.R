test_that("anova2 works", {
  #SLR
  output= anova(lm(melb_data$Price~melb_data$Distance,melb_data))
  output2 = anova2(melb_data$Price~melb_data$Distance,melb_data)
  expect_equal(unname(output2[,1]), unname(output$Df))
  expect_equal(unname(output2[,2]), unname(output$"Sum Sq"))
  expect_equal(unname(output2[,4]), unname(output$"F value"))


  #MLR
  output= anova(lm(melb_data$Price~melb_data$Distance+melb_data$Landsize,melb_data))
  output2 = anova2(melb_data$Price~melb_data$Distance+melb_data$Landsize,melb_data)
  expect_equal(unname(output2[,1]), unname(output$Df))
  expect_equal(unname(output2[,2]), unname(output$"Sum Sq"))
  expect_equal(unname(output2[,4]), unname(output$"F value"))

  #Interaction
  output= anova(lm(melb_data$Price~melb_data$Distance+melb_data$Landsize+melb_data$Landsize*melb_data$Distance,melb_data))
  output2 = anova2(melb_data$Price~melb_data$Distance+melb_data$Landsize+melb_data$Landsize*melb_data$Distance,melb_data)
  expect_equal(unname(output2[,1]), unname(output$Df))
  expect_equal(unname(output2[,2]), unname(output$"Sum Sq"))
  expect_equal(unname(output2[,4]), unname(output$"F value"))

  #No Intercept
  output= anova(lm(melb_data$Price~-1+melb_data$Rooms,melb_data))
  output2 = anova2(melb_data$Price~-1+melb_data$Rooms,melb_data)
  expect_equal(unname(output2[,1]), unname(output$Df))
  expect_equal(unname(output2[,2]), unname(output$"Sum Sq"))
  expect_equal(unname(output2[,4]), unname(output$"F value"))

  #Missing Value - fail
  output2 = anova2(melb_data$Price~melb_data$Distance,melb_data,na.action = 'fail')
  expect_equal(output2,melb_data)

  #Missing Value - other values
  output2 = anova2(melb_data$Price~melb_data$Distance,melb_data,na.action = 'p')
  expect_equal(output2,melb_data)
})
