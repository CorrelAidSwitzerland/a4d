test_that("read_column_synonyms works", {
    patient_synonyms <- read_column_synonyms(
        "synonyms_patient.yaml",
        path_prefixes = c("test_data", "synonyms")
    )

    expect_equal(colnames(patient_synonyms), c("variable_name", "tracker_name"))


    # TODO(KW) Medium: This test should be refactored such that it does not test
    # the content the test data, but only code logic. Testing the content of the
    # test data is meaningless unless it is maintained to be up to date with the
    # actual reference data, and even in that case valid configuration changes
    # should not be able to cause tests to fail.
    age_synonyms <- patient_synonyms %>% dplyr::filter(variable_name == "age")
    expect_setequal(age_synonyms$tracker_name, c("Age", "Age*", "age on reporting", "Age (Years)"))

    baseline_synonyms <- patient_synonyms %>% dplyr::filter(grepl(variable_name, pattern = "baseline_fbg"))
    expect_contains(baseline_synonyms$tracker_name, c(
        "FBG (mg/dL) - BASELINE", "Baseline SMBG or CBG (mg/dL)", "Baseline FBG (mg/dL)",
        "Baseline FBG (mg%)", "Baseline FBG or CBG (mg/dL)"
    ))
})
