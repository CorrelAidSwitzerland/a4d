test_that("sanitized column name is correct", {
    expect_equal(sanitize_str("keepso"), "keepso")
    expect_equal(sanitize_str("remove whitespace"), "removewhitespace")
    expect_equal(sanitize_str("John Doe's Column"), "johndoescolumn")
    expect_equal(sanitize_str("Date 2022"), "date2022")
    expect_equal(sanitize_str("My Awesome 1st Column!!"), "myawesome1stcolumn")
    expect_equal(sanitize_str("Education/\nOcupation"), "educationocupation")
})
