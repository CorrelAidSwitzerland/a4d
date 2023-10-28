test_that("read_column_synonyms works", {
    patient_synonyms <- read_column_synonyms(
        "synonyms_patient.yaml"
    )

    expect_equal(colnames(patient_synonyms), c("variable_name", "tracker_name"))


    # TODO(KW) Medium: This test should be refactored such that it does not test
    # the content of reference_data, but only code logic. Otherwise reference
    # data changes can cause unexpected test failures.
    age_synonyms <- patient_synonyms %>% dplyr::filter(variable_name == "age")
    expect_setequal(age_synonyms$tracker_name, c("Age", "Age*", "age on reporting", "Age (Years)"))

    baseline_synonyms <- patient_synonyms %>% dplyr::filter(grepl(variable_name, pattern = "fbg_baseline_mg"))
    expect_contains(baseline_synonyms$tracker_name, c(
        "FBG (mg/dL) - BASELINE", "Baseline SMBG or CBG (mg/dL)", "Baseline FBG (mg/dL)",
        "Baseline FBG (mg%)", "Baseline FBG or CBG (mg/dL)"
    ))
})
