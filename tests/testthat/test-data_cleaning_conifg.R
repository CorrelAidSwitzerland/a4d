test_that("data cleaning config for support_from_a4d yields valid R", {
    env <- list(support_from_a4d = "Standard", ERROR_VAL_CHARACTER = "Undefined")

    expect_equal(
        eval(
            parse_character_cleaning_pipeline(
                "support_from_a4d",
                a4d:::config$cleaning$support_from_a4d
            ),
            env
        ),
        "Standard"
    )
})

test_that("data cleaning config for status yields valid R", {
    env <- list(status = "Active", ERROR_VAL_CHARACTER = "Undefined")

    expect_equal(
        eval(
            parse_character_cleaning_pipeline(
                "status",
                a4d:::config$cleaning$status
            ),
            env
        ),
        "Active"
    )
})

test_that("data cleaning config for insulin_regimen yields valid R", {
    env <- list(insulin_regimen = "Basal-bolus", ERROR_VAL_CHARACTER = NA_character_)

    expect_equal(
        eval(
            parse_character_cleaning_pipeline(
                "insulin_regimen",
                a4d:::config$cleaning$insulin_regimen
            ),
            env
        ),
        "Basal-bolus"
    )
})

test_that("data cleaning config for t1d_diagnosis_with_dka yields valid R", {
    env <- list(t1d_diagnosis_with_dka = "N", ERROR_VAL_CHARACTER = NA_character_, id = 1)

    expect_equal(
        eval(
            parse_character_cleaning_pipeline(
                "t1d_diagnosis_with_dka",
                a4d:::config$cleaning$t1d_diagnosis_with_dka
            ),
            env
        ),
        "N"
    )
})

test_that("data cleaning config for hospitalization_cause yields valid R", {
    env <- list(hospitalization_cause = "DKA", ERROR_VAL_CHARACTER = NA_character_)

    expect_equal(
        eval(
            parse_character_cleaning_pipeline(
                "hospitalization_cause",
                a4d:::config$cleaning$hospitalization_cause
            ),
            env
        ),
        "DKA"
    )
})

test_that("data cleaning config for province yields valid R", {
    env <- list(province = "Not a valid province", id = 1)

    expect_equal(
        eval(
            parse_character_cleaning_pipeline(
                "province",
                a4d:::config$cleaning$province
            ),
            env
        ),
        "Undefined"
    )
})

test_that("data cleaning config for provinces had placeholder expanded", {
    expect_gt(length(a4d:::config$cleaning$province$steps[[1]]$allowed_values), 1)
})
