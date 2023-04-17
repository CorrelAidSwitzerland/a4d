test_that("check_compute_balance works", {
    expect_equal(helper_compute_exit_balance(compute_balance(df_balance_product, 2020), 2020), 6)
    expect_equal(helper_compute_exit_balance(compute_balance(df_balance_product, 2021), 2021), 6)
})




