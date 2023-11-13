ERROR_VAL_CHARACTER <<- "Other"
ERROR_VAL_NUMERIC <<- 999999
ERROR_VAL_DATE <<- as.Date("9999-09-09")

test_that("cut_numeric_value works", {
    expect_equal(cut_numeric_value(1, min = 0, max = 5), 1)
    expect_equal(cut_numeric_value(3.4, min = 0, max = 5), 3.4)
    expect_equal(cut_numeric_value(3.4, max = 3.4, min = 1), 3.4)
    expect_equal(cut_numeric_value(3.4, max = 3, min = 0), ERROR_VAL_NUMERIC)
    expect_equal(cut_numeric_value(1, max = 5, min = 2), ERROR_VAL_NUMERIC)
    expect_equal(cut_numeric_value(2, max = 3, min = 3), ERROR_VAL_NUMERIC)
    expect_true(is.na(cut_numeric_value(NA, max = 3, min = 1)))
})


test_that("fix_bmi works", {
    test_df <- data.frame(
        weight = c(1, 10, NA, 3, NA, 999999),
        height = c(1, 2, 2, NA, NA, 1),
        id = c("1", "2", "3", "4", "5", "6")
    )
    expected_bmi <- c(1, 10 / 4, NA, NA, NA, 999999)
    expect_equal(test_df %>% rowwise() %>% mutate(bmi = fix_bmi(weight, height, id)) %>% select(bmi) %>% pull(), expected_bmi)
})


test_that("split_bp_in_sys_and_dias works", {
    test_df <- data.frame(blood_pressure_mmhg = c("96/55", "101/57", NA))
    expected_df <- tibble(blood_pressure_sys_mmhg = c("96", "101", NA), blood_pressure_dias_mmhg = c("55", "57", NA))
    expect_equal(split_bp_in_sys_and_dias(test_df), expected_df)

    test_df <- data.frame(blood_pressure_mmhg = c("96", "1,6"))
    expected_df <- tibble(
        blood_pressure_sys_mmhg = c(as.character(ERROR_VAL_NUMERIC), as.character(ERROR_VAL_NUMERIC)),
        blood_pressure_dias_mmhg = c(as.character(ERROR_VAL_NUMERIC), as.character(ERROR_VAL_NUMERIC))
    )
    expect_equal(split_bp_in_sys_and_dias(test_df), expected_df)
})


test_that("convert_to works", {
    # numeric
    expect_equal(convert_to(1, as.numeric, ERROR_VAL_NUMERIC), 1)
    expect_equal(convert_to(123.123, as.numeric, ERROR_VAL_NUMERIC), 123.123)
    expect_equal(convert_to(0, as.numeric, ERROR_VAL_NUMERIC), 0)
    expect_equal(convert_to(-0.01, as.numeric, ERROR_VAL_NUMERIC), -0.01)
    expect_equal(convert_to("1234", as.numeric, ERROR_VAL_NUMERIC), 1234)
    expect_equal(convert_to(".123", as.numeric, ERROR_VAL_NUMERIC), .123)
    expect_equal(convert_to("a", as.numeric, ERROR_VAL_NUMERIC), ERROR_VAL_NUMERIC)
    expect_true(is.na(convert_to(NA, as.numeric, ERROR_VAL_NUMERIC)))

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
    expect_equal(convert_to(1, as.integer, ERROR_VAL_NUMERIC), 1)
    expect_equal(convert_to(123, as.integer, ERROR_VAL_NUMERIC), 123)
    expect_equal(convert_to(-1, as.integer, ERROR_VAL_NUMERIC), -1)
    expect_equal(convert_to(.1, as.integer, ERROR_VAL_NUMERIC), 0)
    expect_equal(convert_to(-.1, as.integer, ERROR_VAL_NUMERIC), 0)
    expect_equal(convert_to(123.123, as.integer, ERROR_VAL_NUMERIC), 123)
    expect_equal(convert_to("1", as.integer, ERROR_VAL_NUMERIC), 1)
    expect_equal(convert_to("1.5", as.integer, ERROR_VAL_NUMERIC), 1)
    expect_true(is.na(convert_to("", as.integer, ERROR_VAL_NUMERIC)))
    expect_equal(convert_to(T, as.integer, ERROR_VAL_NUMERIC), 1)
    expect_true(is.na(convert_to(NA, as.integer, ERROR_VAL_NUMERIC)))

    # date
    expect_equal(convert_to("2023-01-01", lubridate::as_date, ERROR_VAL_DATE), as.Date("2023-01-01"))
    expect_equal(convert_to("2023", lubridate::as_date, ERROR_VAL_DATE), ERROR_VAL_DATE)
    expect_equal(convert_to("45007", lubridate::as_date, ERROR_VAL_DATE), ERROR_VAL_DATE)
    expect_true(is.na(convert_to("", lubridate::as_date, ERROR_VAL_DATE)))
    expect_true(is.na(convert_to(NA, lubridate::as_date, ERROR_VAL_DATE)))
})


test_that("fix_age works", {
    dob <- "2010-05-01"
    expect_equal(fix_age(age = 10, dob = dob, tracker_year = 2020, tracker_month = 6, id = "1"), 10)
    expect_equal(fix_age(age = 10, dob = dob, tracker_year = 2020, tracker_month = 4, id = "1"), 9)
})


test_that("fix_sex works", {
    # Test case 1: Test with a female gender
    expect_equal(fix_sex("female", "1"), "F",
        info = "For sex 'female', expected 'F'"
    )

    # Test case 2: Test with a male gender
    expect_equal(fix_sex("man", "2"), "M",
        info = "For sex 'man', expected 'M'"
    )

    # Test case 3: Test with an empty gender
    expect_true(is.na(fix_sex("", "3")),
        info = "For empty sex, expected NA"
    )

    # Test case 4: Test with another# gender not in the synonyms list
    expect_equal(fix_sex("unknown", "4"), ERROR_VAL_CHARACTER,
        info = "For sex 'unknown', expected 'Other'"
    )
})


test_that("extract_year_from_age works", {
    expect_equal(extract_year_from_age("2y9m"), "2")
    expect_equal(extract_year_from_age("10y10m"), "10")
    expect_equal(extract_year_from_age("7y6m"), "7")
})


# Test case for detecting 'birth' in t1d_diagnosis_age
test_that("fix_t1d_diagnosis_age works", {
    expect_equal(fix_t1d_diagnosis_age("birth", "1"), "0")
    expect_equal(fix_t1d_diagnosis_age("At birth", "1"), "0")

    # Test case for detecting 'born' in t1d_diagnosis_age
    expect_equal(fix_t1d_diagnosis_age("born", "1"), "0")

    # Test case for detecting 'month' in t1d_diagnosis_age
    expect_equal(fix_t1d_diagnosis_age("4 months", "1"), "0")

    # Test case for detecting 'y' in t1d_diagnosis_age
    expect_equal(fix_t1d_diagnosis_age("5y", "1"), "5")
    expect_equal(fix_t1d_diagnosis_age("10y10m", "2"), "10")

    # Test case for handling NA values in t1d_diagnosis_age
    expect_true(is.na(fix_t1d_diagnosis_age(NA, "1")))

    # Test case for default case
    expect_equal(fix_t1d_diagnosis_age("10", "1"), "10")
    expect_equal(fix_t1d_diagnosis_age("0", "1"), "0")
    expect_equal(fix_t1d_diagnosis_age("1", "1"), "1")
})


test_that("correct_decimal_sign works", {
    expect_equal(correct_decimal_sign("12,2"), "12.2")
    expect_equal(correct_decimal_sign(""), "")
    expect_true(is.na(correct_decimal_sign(NA)))
})


test_that("fix_fbg works", {
    expect_equal(fix_fbg("High"), "200")
    expect_equal(fix_fbg("Bad"), "200")
    expect_equal(fix_fbg("Hi"), "200")

    expect_equal(fix_fbg("Medium"), "170")
    expect_equal(fix_fbg("Med"), "170")

    expect_equal(fix_fbg("Low"), "140")
    expect_equal(fix_fbg("Good"), "140")
    expect_equal(fix_fbg("Okay"), "140")

    expect_equal(fix_fbg("123 (DKA)"), "123")

    expect_true(is.na(fix_fbg(NA)))
    expect_true(is.na(fix_fbg("")))
})


test_that("fix_digit_date works", {
    expect_equal(fix_digit_date(44693), "2022-05-12")
    expect_equal(fix_digit_date("44693"), "2022-05-12")
    expect_equal(fix_digit_date("2021-05-12"), "2021-05-12")
})


test_that("check_allowed_values works", {
    valid_support_values <- c(
        "Standard",
        "Partial",
        "Semi-Partial",
        "SAC",
        "Monitoring"
    )
    expect_equal(check_allowed_values("partial", valid_support_values, ERROR_VAL_CHARACTER, "1"), "partial")
    expect_equal(check_allowed_values("Standard", valid_support_values, ERROR_VAL_CHARACTER, "1"), "Standard")
    expect_equal(check_allowed_values("SAc", valid_support_values, ERROR_VAL_CHARACTER, "1"), "SAc")
    expect_true(is.na(check_allowed_values("", valid_support_values, ERROR_VAL_CHARACTER, "1")))
    expect_true(is.na(check_allowed_values(NA, valid_support_values, ERROR_VAL_CHARACTER, "1")))
    expect_equal(check_allowed_values("abc", valid_support_values, ERROR_VAL_CHARACTER, "1"), ERROR_VAL_CHARACTER)
    expect_equal(check_allowed_values("abc", valid_support_values, NA, "1"), "abc")
})


test_that("fix_testing_frequency works", {
    expect_equal(fix_testing_frequency("2"), "2")
    expect_equal(fix_testing_frequency("1.5"), "1.5")
    expect_equal(fix_testing_frequency("0-2"), "1")
    expect_equal(fix_testing_frequency("2-3"), "2.5")
    expect_true(is.na(fix_testing_frequency("")))
    expect_true(is.na(fix_testing_frequency(NA)))
})


test_that("extract_date_from_measurement works", {
    test_df <- data.frame(
        measurement = c(
            "8.53 (28/8/2017)",
            "10,75 (31/12/2020)",
            "1075 (31/12/2020)",
            "10.75 (31/12/2020)",
            "10.75 (31-12-2020)",
            "15.42 (2022-01)",
            "10.75 (01-Sept-2022)",
            "10.75 (01-Sept-2022",
            "10.75(01-2017)",
            "3"
        )
    )
    expected_df <- dplyr::as_tibble(data.frame(
        measurement = c(
            "8.53 ",
            "10,75 ",
            "1075 ",
            "10.75 ",
            "10.75 ",
            "15.42 ",
            "10.75 ",
            "10.75 ",
            "10.75",
            "3"
        ),
        measurement_date = c(
            "28/8/2017",
            "31/12/2020",
            "31/12/2020",
            "31/12/2020",
            "31-12-2020",
            "2022-01",
            "01-Sept-2022",
            "01-Sept-2022",
            "01-2017",
            NA
        )
    ))

    expect_equal(extract_date_from_measurement(test_df, "measurement"), expected_df)
})


test_that("fix_id works", {
    expect_equal(fix_id("KD_EW004"), "KD_EW004")
    expect_equal(fix_id("K_EW004"), "Other")
    expect_equal(fix_id("KD_E004"), "Other")
    expect_equal(fix_id("KD_EWX04"), "Other")
    expect_equal(fix_id("11_EW004"), "Other")
    expect_equal(fix_id("KD_E1004"), "Other")
    expect_equal(fix_id("KD_EW004XY"), "KD_EW004")
})
