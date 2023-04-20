test_that("read_column_synonyms works", {
    master_file <- test_path("testdata", "synonyms.xlsx")

    patient_synonyms <- read_column_synonyms(
        master_file, sheet = "synonyms_PatientData"
    )

    expect_equal(colnames(patient_synonyms), c("variable_name", "tracker_name"))

    age_synonyms = patient_synonyms %>% dplyr::filter(variable_name=="age")
    expect_setequal(age_synonyms$tracker_name, c("Age", "Age*"))

    baseline_synonyms = patient_synonyms %>% dplyr::filter(variable_name=="baseline_fbg_mgdl")
    expect_setequal(baseline_synonyms$tracker_name, c("FBG (mg/dL) - BASELINE", "Baseline SMBG or CBG (mg/dL)", "Baseline FBG (mg/dL)"))

})
