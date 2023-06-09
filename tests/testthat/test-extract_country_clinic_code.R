test_that("check_extract_country_clinic_code works", {
    expect_equal(extract_country_clinic_code(patient_demo_data), list("country_code" = "DE", "clinic_code" = "GR"))
    expect_equal(extract_country_clinic_code(patient_demo_data2), list("country_code" = "DE", "clinic_code" = "AB"))
})
