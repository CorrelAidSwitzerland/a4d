test_that("check_numeric_borders works", {
    test_vec <- c(1, 2, 3.4)
    expect_equal(check_numeric_borders(test_vec, 5, 0), test_vec)
    expect_equal(check_numeric_borders(test_vec, 3.4, 1), test_vec)
    expect_equal(check_numeric_borders(test_vec, 3, 0), c(1, 2, NA))
    expect_equal(check_numeric_borders(test_vec, 5, 2), c(NA, 2, 3.4))
    expect_equal(check_numeric_borders(test_vec, 3, 3), c(NA, NA, NA))
})


test_that("bmi_fix works", {
    test_df <- data.frame(weight = c(1, NA, 3, NA), height = c(1, 2, NA, NA), bmi = c(1, 2, 3, 4))
    expected_df <- data.frame(weight = c(1, NA, 3, NA), height = c(1, 2, NA, NA), bmi = c(1, NA, NA, NA))
    expect_equal(bmi_fix(test_df), expected_df)
})

test_that("split_bp_in_sys_and_dias works", {
    test_df <- data.frame(blood_pressure_mmhg = c("96/55", "101/57", NA))
    expected_df <- tibble(blood_pressure_sys_mmhg = c("96", "101", NA), blood_pressure_dias_mmhg = c("55", "57", NA))
    expect_equal(split_bp_in_sys_and_dias(test_df), expected_df)
})
