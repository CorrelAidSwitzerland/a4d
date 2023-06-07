test_that("read_column_synonyms works", {
    patient_synonyms <- read_column_synonyms(
        "synonyms_patient.yaml"
    )

    expect_equal(colnames(patient_synonyms), c("variable_name", "tracker_name"))

    age_synonyms <- patient_synonyms %>% dplyr::filter(variable_name == "age")
    expect_setequal(age_synonyms$tracker_name, c("Age", "Age*", "age on reporting", "Age (Years)"))

    baseline_synonyms <- patient_synonyms %>% dplyr::filter(grepl(variable_name, pattern = "baseline_fbg"))
    expect_setequal(baseline_synonyms$tracker_name, c(
        "FBG (mg/dL) - BASELINE", "Baseline SMBG or CBG (mg/dL)", "Baseline FBG (mg/dL)",
        "Baseline FBG (mmol/L)", "Baseline FBG (mg%)", "Baseline FBG or CBG (mg/dL)",
        "baseline_fbg_unit"
    ))
})
