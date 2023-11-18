test_that("check_get_tracker_year works", {
    tracker_file <- "test_2022.xlsx"
    month_list <- c("Jan23", "Feb23", "Dec23")
    wrong_month_list <- c("Jan", "Feb", "Dec")
    expect_equal(get_tracker_year(tracker_file, month_list), 2023)
    expect_equal(get_tracker_year(tracker_file, wrong_month_list), 2022)
})
