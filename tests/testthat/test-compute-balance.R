test_that("check_compute_balance works", {
    expect_equal(tail(compute_balance(df_balance_product)$product_balance,1), helper_compute_correct_exit_balance(df_balance_product))
})




