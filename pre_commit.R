styler::style_pkg(filetype = c(".R", ".Rmd", ".Rmarkdown", ".Rnw"), indent_by = 4L, start_comments_with_one_space = TRUE)
devtools::document()
devtools::check()

