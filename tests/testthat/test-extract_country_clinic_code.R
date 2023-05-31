test_that("check_extract_country_clinic_code works", {
    expect_equal(extract_country_clinic_code(codes_in_2nd_row), list("country_code" = "DE", "clinic_code" = "GR"))
    expect_equal(extract_country_clinic_code(codes_in_column_names), list("country_code" = "DE", "clinic_code" = "GR"))
    expect_equal(extract_country_clinic_code(codes_fromPatientID), list("country_code" = "DE", "clinic_code" = "GR"))
    expect_equal(extract_country_clinic_code(codes_notclean), list("country_code" = "DE", "clinic_code" = "GR"))
})