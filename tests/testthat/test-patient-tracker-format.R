test_that("check_numeric_borders works", {
    test_vec <- c(1, 2, 3.4)
    expect_equal(check_numeric_borders(test_vec, 5, 0), test_vec)
    expect_equal(check_numeric_borders(test_vec, 3.4, 1), test_vec)
    expect_equal(check_numeric_borders(test_vec, 3, 0), c(1, 2, NA))
    expect_equal(check_numeric_borders(test_vec, 5, 2), c(NA, 2, 3.4))
    expect_equal(check_numeric_borders(test_vec, 3, 3), c(NA, NA, NA))

})


test_that("date_transform function works as expected", {
    expect_equal(parse_date_string("2020-01-01"), as.Date("2020-01-01"))
    expect_equal(parse_date_string("44430"), as.Date("2021-08-22"))
    expect_equal(parse_date_string("2-Jan-20011"), as.Date("9999-01-01"))
    expect_equal(parse_date_string("May-20"), as.Date("2020-05-01"))
    expect_equal(parse_date_string("May-99"), as.Date("1999-05-01"))
    expect_equal(parse_date_string("Jan-2021"), as.Date("2021-01-01"))
    expect_equal(parse_date_string("invalid"), as.Date("9999-01-01"))
    expect_equal(parse_date_string("June2020"), as.Date("2020-06-01"))
    expect_equal(parse_date_string("June-2020"), as.Date("2020-06-01"))
    expect_equal(parse_date_string("08/05/2020"), lubridate::ymd("2020-05-08"))
    expect_equal(parse_date_string("6/10/17"), lubridate::ymd("2017-10-06"))
    expect_equal(parse_date_string("2020-05-08"), lubridate::ymd("2020-05-08"))
})



test_that("fix_date_cols works correctly", {
    expect_equal(fix_date_cols(NA), NA_Date_)
    expect_true(is.na(fix_date_cols(NA)))
    expect_equal(fix_date_cols("14(Nov-17)"), lubridate::ymd("2017-11-01"))
    expect_equal(fix_date_cols("13.0  (Jun-17)"), lubridate::ymd("2017-06-01"))
})
