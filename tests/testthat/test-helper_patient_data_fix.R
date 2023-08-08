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
    expect_equal(convert_to(T, as.integer, 999999), 1)
    expect_true(is.na(convert_to(NA, as.integer, 999999)))

    # date
    expect_equal(convert_to("2023-01-01", lubridate::as_date, as.Date("9999-01-01")), as.Date("2023-01-01"))
    expect_equal(convert_to("2023", lubridate::as_date, as.Date("9999-01-01")), as.Date("9999-01-01"))
    expect_equal(convert_to("45007", lubridate::as_date, as.Date("9999-01-01")), as.Date("9999-01-01"))
    expect_true(is.na(convert_to("", lubridate::as_date, "9999-01-01")))
    expect_true(is.na(convert_to(NA, lubridate::as_date, "9999-01-01")))
})


test_that("fix_age works", {
    dob <- "2010-05-01"
    expect_equal(fix_age(age = 10, dob = dob, tracker_year = 2020, tracker_month = 6, id = "1"), 10)
    expect_equal(fix_age(age = 10, dob = dob, tracker_year = 2020, tracker_month = 4, id = "1"), 9)
})


test_that("fix_gender returns the correct gender codes", {
    # Test case 1: Test with a female gender
    expect_equal(fix_gender("female", "1"), "F",
        info = "For gender 'female', expected 'F'"
    )

    # Test case 2: Test with a male gender
    expect_equal(fix_gender("man", "2"), "M",
        info = "For gender 'man', expected 'M'"
    )

    # Test case 3: Test with an empty gender
    expect_true(is.na(fix_gender("", "3")),
        info = "For empty gender, expected NA"
    )

    # Test case 4: Test with another# gender not in the synonyms list
    expect_equal(fix_gender("unknown", "4"), "Other",
        info = "For gender 'unknown', expected 'Other'"
    )
})


test_that("replace_empty_string_with_NA works", {
    expect_equal(replace_empty_string_with_NA(c("a", "", "NA", NA, "b")), c("a", NA, "NA", NA, "b"))
})


test_that("extract_year_from_age works", {
    expect_equal(extract_year_from_age("2y9m"), "2")
    expect_equal(extract_year_from_age("10y10m"), "10")
    expect_equal(extract_year_from_age("7y6m"), "7")
})


# Test case for detecting 'birth' in t1d_diagnosis_age
test_that("Test for detecting 'birth' in t1d_diagnosis_age", {
    expect_equal(fix_t1d_diagnosis_age("birth", "2000-01-01", "1"), "0")
    expect_equal(fix_t1d_diagnosis_age("At birth", "2000-01-01", "1"), "0")
})

# Test case for detecting 'born' in t1d_diagnosis_age
test_that("Test for detecting 'born' in t1d_diagnosis_age", {
    expect_equal(fix_t1d_diagnosis_age("born", "2020-01-01", "1"), "0")
})

# Test case for detecting 'month' in t1d_diagnosis_age

test_that("Test for detecting 'month' in t1d_diagnosis_age", {
    expect_equal(fix_t1d_diagnosis_age("4 months", "2020-01-01", "1"), "0")
})

# Test case for detecting 'y' in t1d_diagnosis_age
test_that("Test for detecting 'y' in t1d_diagnosis_age", {
    expect_equal(fix_t1d_diagnosis_age("5y", "2020-01-01", "1"), "5")
    expect_equal(fix_t1d_diagnosis_age("10y10m", "2020-01-02", "2"), "10")
})

# Test case for handling NA values in t1d_diagnosis_age
test_that("Test for handling NA values in t1d_diagnosis_age", {
    expect_true(is.na(fix_t1d_diagnosis_age(NA, "2020-01-01", "1")))
})

# Test case for default case
test_that("Test for default case in fix_t1d_diagnosis_age", {
    expect_equal(fix_t1d_diagnosis_age("10", "2020-01-01", "1"), "10")
    expect_equal(fix_t1d_diagnosis_age("0", "2020-01-01", "1"), "0")
    expect_equal(fix_t1d_diagnosis_age("1", "2020-01-01", "1"), "1")
})


test_that("correct_decimal_sign works", {
    expect_equal(correct_decimal_sign("12,2"), "12.2")
})


test_that("Replaces high textual description with numeric value 200", {
    expect_equal(fix_fbg("High"), "200")
    expect_equal(fix_fbg("Bad"), "200")
    expect_equal(fix_fbg("Hi"), "200")
})

test_that("Replaces medium textual description with numeric value 170", {
    expect_equal(fix_fbg("Medium"), "170")
    expect_equal(fix_fbg("Med"), "170")
})

test_that("Replaces low textual description with numeric value 140", {
    expect_equal(fix_fbg("Low"), "140")
    expect_equal(fix_fbg("Good"), "140")
    expect_equal(fix_fbg("Okay"), "140")
})


test_that("Does not replace other descriptions", {
    fbg <- "123 (DKA)"
    expected <- "123"
    result <- fix_fbg(fbg)
    expect_equal(result, expected)
})


test_that("Test date_as_fivedigit_number_fix()", {
    expect_equal(date_as_fivedigit_number_fix(44693), "2022-05-12")
    expect_equal(date_as_fivedigit_number_fix("44693"), "2022-05-12")
    expect_equal(date_as_fivedigit_number_fix("2021-05-12"), "2021-05-12")
})
