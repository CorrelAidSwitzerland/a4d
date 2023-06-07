test_that("check_numeric_borders works", {
    test_vec <- c(1, 2, 3.4)
    expect_equal(check_numeric_borders(test_vec, 5, 0), test_vec)
    expect_equal(check_numeric_borders(test_vec, 3.4, 1), test_vec)
    expect_equal(check_numeric_borders(test_vec, 3, 0), c(1, 2, NA))
    expect_equal(check_numeric_borders(test_vec, 5, 2), c(NA, 2, 3.4))
    expect_equal(check_numeric_borders(test_vec, 3, 3), c(NA, NA, NA))
})
