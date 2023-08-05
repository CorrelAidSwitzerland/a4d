test_that("cut_numeric_value works", {
    ERROR_VAL_NUMERIC <<- 999999
    test_vec <- c(1, 2, 3.4)
    expect_equal(cut_numeric_value(test_vec, min = 0, max = 5), test_vec)
    expect_equal(cut_numeric_value(test_vec, max = 3.4, min = 1), test_vec)
    expect_equal(cut_numeric_value(test_vec, max = 3, min = 0), c(1, 2, ERROR_VAL_NUMERIC))
    expect_equal(cut_numeric_value(test_vec, max = 5, min = 2), c(ERROR_VAL_NUMERIC, 2, 3.4))
    expect_equal(cut_numeric_value(test_vec, max = 3, min = 3), as.numeric(c(ERROR_VAL_NUMERIC, ERROR_VAL_NUMERIC, ERROR_VAL_NUMERIC)))
})


test_that("fix_bmi works", {
    ERROR_VAL_NUMERIC <<- 999999
    test_df <- data.frame(weight = c(1, NA, 3, NA, 1), height = c(1, 2, NA, NA, 1), bmi = c(5, 2, 3, 4, 1), id = c("1", "2", "3", "4", "5"))
    expected_bmi <- c(5, 999999, 999999, 999999, 1)
    expect_equal(test_df %>% rowwise() %>% mutate(bmi = fix_bmi(bmi, weight, height, id)) %>% select(bmi) %>% pull(), expected_bmi)
})


test_that("split_bp_in_sys_and_dias works", {
    test_df <- data.frame(blood_pressure_mmhg = c("96/55", "101/57", NA))
    expected_df <- tibble(blood_pressure_sys_mmhg = c("96", "101", NA), blood_pressure_dias_mmhg = c("55", "57", NA))
    expect_equal(split_bp_in_sys_and_dias(test_df), expected_df)
})


test_that("convert_to works", {
    # numeric
    expect_equal(convert_to(1, as.numeric, 999999), 1)
    expect_equal(convert_to(123.123, as.numeric, 999999), 123.123)
    expect_equal(convert_to(0, as.numeric, 999999), 0)
    expect_equal(convert_to(-0.01, as.numeric, 999999), -0.01)
    expect_equal(convert_to("1234", as.numeric, 999999), 1234)
    expect_equal(convert_to(".123", as.numeric, 999999), .123)
    expect_equal(convert_to("a", as.numeric, 999999), 999999)
    expect_true(is.na(convert_to(NA, as.numeric, 999999)))

    # logical
    expect_true(convert_to(1, as.logical, FALSE))
    expect_false(convert_to(0, as.logical, FALSE))
    expect_true(convert_to(123154, as.logical, FALSE))
    expect_true(convert_to(-.123, as.logical, FALSE))
    expect_true(convert_to("true", as.logical, FALSE))
    expect_true(convert_to("TRUE", as.logical, FALSE))
    expect_true(convert_to("T", as.logical, FALSE))
    expect_false(convert_to("false", as.logical, FALSE))
    expect_false(convert_to("FALSE", as.logical, FALSE))
    expect_false(convert_to("F", as.logical, FALSE))
    expect_true(is.na(convert_to("", as.logical, FALSE)))
    expect_true(is.na(convert_to(NA, as.logical, FALSE)))
    expect_false(convert_to(c(), as.logical, FALSE))

    # integer
    expect_equal(convert_to(1, as.integer, 999999), 1)
    expect_equal(convert_to(123, as.integer, 999999), 123)
    expect_equal(convert_to(-1, as.integer, 999999), -1)
    expect_equal(convert_to(.1, as.integer, 999999), 0)
    expect_equal(convert_to(-.1, as.integer, 999999), 0)
    expect_equal(convert_to(123.123, as.integer, 999999), 123)
    expect_equal(convert_to("1", as.integer, 999999), 1)
    expect_equal(convert_to("1.5", as.integer, 999999), 1)
    expect_true(is.na(convert_to("", as.integer, 999999)))
    expect_equal(convert_to(c(), as.integer, 999999), 999999)
    expect_equal(convert_to(T, as.integer, 999999), 1)

    # date
    expect_equal(convert_to("2023-01-01", as.Date, "9999-01-01"), as.Date("2023-01-01"))
    expect_equal(convert_to("2023", as.Date, as.Date("9999-01-01")), as.Date("9999-01-01"))
    expect_equal(convert_to("45007", as.Date, as.Date("9999-01-01")), as.Date("9999-01-01"))
    expect_true(is.na(convert_to("", as.Date, "9999-01-01")))
})


test_that("fix_age works", {
    dob <- "2010-05-01"
    expect_equal(fix_age(age = 10, dob = dob, tracker_year = 2020, tracker_month = 6, id = "1"), 10)
    expect_equal(fix_age(age = 10, dob = dob, tracker_year = 2020, tracker_month = 4, id = "1"), 9)
})


test_that("fix_gender returns the correct gender codes", {
    # Test case 1: Test with a female gender
    gender1 <- "female"
    id1 <- "1"
    expected1 <- "F"
    result1 <- fix_gender(gender1, id1)
    expect_equal(result1, expected1,
        info = "For gender 'female', expected 'F'"
    )

    # Test case 2: Test with a male gender
    gender2 <- "man"
    id2 <- "2"
    expected2 <- "M"
    result2 <- fix_gender(gender2, id2)
    expect_equal(result2, expected2,
        info = "For gender 'man', expected 'M'"
    )

    # Test case 3: Test with an empty gender
    gender3 <- ""
    id3 <- "3"
    expected3 <- NA_character_
    result3 <- fix_gender(gender3, id3)
    expect_equal(result3, expected3,
        info = "For empty gender, expected NA"
    )

    # Test case 4: Test with another# gender not in the synonyms list
    gender4 <- "unknown"
    id4 <- "4"
    expected4 <- "Other"
    result4 <- fix_gender(gender4, id4)
    expect_equal(result4, expected4,
        info = "For gender 'unknown', expected 'Other'"
    )
})

test_that("fix_gender log info for 'Other' gender", {
    # Test case 1: Test with a valid gender
    gender1 <- "female"
    id1 <- "1"
    expected1 <- ""
    expect_equal(capture_output(fix_gender(gender1, id1)), expected1,
        info = "For valid gender, expect no log info"
    )

    # Test case 2: Test with an 'Other' gender
    gender2 <- "unknown"
    id2 <- "2"
    expected2 <- paste0(
        "Patient ", id2, ": gender ", gender2,
        " is not in the list of synonyms. Replacing it with Other."
    )
    expect_equal(capture_output(fix_gender(gender2, id2)), expected2,
        info = "For 'Other' gender, expect log info"
    )
})
