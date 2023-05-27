test_that("sanitized column name is correct", {
    expect_equal(sanitize_column_name("keepso"), "keepso")
    expect_equal(sanitize_column_name("remove whitespace"), "removewhitespace")
    expect_equal(sanitize_column_name("John Doe's Column"), "johndoescolumn")
    expect_equal(sanitize_column_name("Date 2022"), "date2022")
    expect_equal(sanitize_column_name("My Awesome 1st Column!!"), "myawesome1stcolumn")
    expect_equal(sanitize_column_name("Education/\nOcupation"), "educationocupation")
})
