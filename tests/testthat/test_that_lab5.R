
test_that("data_address and plot_address seems to work", {
  expect_error(data_address("den_har_adressen_finns_ej"))
  expect_error(data_address(10))
  
  expect_error(plot_address("den_har_adressen_finns_ej"))
  expect_error(plot_address(zoom = 10))
  expect_error(plot_address("den_har_adressen_finns_ej",10))
})




test_that("outputs are correct in the data_address function. Totebo", {
  testa <- data_address("Totebo")
  
  expect_output(print(testa),"590 92 Totebo")
  expect_output(print(testa),"57.62")
  expect_output(print(testa),"16.18")
})




test_that("class of data_address is correct", {
  
  expect_true(class(data_address("Totebo")) == "data.frame")
})

test_that("inputs are correct", {
  expect_error(data_address(1))
  suppressWarnings(expect_error(data_address(c("London", "Paris"))))
  
})




test_that("outputs are correct in the data_address function. London", {
  testa <- data_address("London")
  
  expect_output(print(testa),"London")
  expect_output(print(testa),"UK")
})



test_that("outputs are correct in the data_address function. Street", {
  testa <- data_address("central park, new york")
  
  expect_output(print(testa),"Central Park")
  expect_output(print(testa),"New York")
})


test_that("outputs are correct in the data_address function. spacing", {
  testa <- data_address("n y k o p i n g")
  
  expect_output(print(testa),"Sweden")
  expect_output(print(testa),"17.00")
})



test_that("Supposed to not find adress", {

  expect_error(data_address("/WorWlol"))
  expect_error(data_address("socksgatanilandet"))
})



test_that("outputs are correct in the data_address function. spacing", {
  testa <- data_address("riksdagen")
  
  expect_output(print(testa),"Riksgatan 1")
  expect_output(print(testa),"100 12")
})



test_that("outputs are correct in the data_address function. spacing", {
  testa <- data_address("astrid lindgrens varld")
  
  expect_output(print(testa),"598 85 Vimmerby")
  expect_output(print(testa),"598 85")
})







