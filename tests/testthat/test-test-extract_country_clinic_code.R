test_that("check_extract_country_clinic_code works", {
    codes_in_2nd_row <- data.frame(column1=c('test','test1','test2'),
                                   column2=c('test','country_DE','clinic_GR_dd'),
                                   column3=c('test','test1','test2'))

    codes_in_column_names <- data.frame(column1=c('test','test1','test3'),
                                        country_DE=c('test','test2','test2'),
                                        clinic_GR=c('test','test1','test2'))

    codes_missingCountry <- data.frame(column1=c('test','test1','test3'),
                                       column2=c('test','test2','test2'),
                                       clinic_GR=c('test','test1','test2'))

    codes_notclean <- data.frame(column1=c('test','test1','test2'),
                                 column2=c('test','blabla.country_DE.blabla.','blabla_clinic_GR_bla'),
                                 column3=c('test','test1','test2'))


  expect_equal(extract_country_clinic_code(codes_in_2nd_row), list("country_code" = "DE", "clinic_code" = "GR"))
  expect_equal(extract_country_clinic_code(codes_in_column_names), list("country_code" = "DE", "clinic_code" = "GR"))
  expect_output(extract_country_clinic_code(codes_missingCountry), "could not extract country code", ignore.case = TRUE)
  expect_equal(extract_country_clinic_code(codes_notclean), list("country_code" = "DE", "clinic_code" = "GR"))
})
