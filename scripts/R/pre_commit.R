styler_confs <- rlang::exprs(
    filetype = c(".R", ".Rmd", ".Rmarkdown", ".Rnw"),
    indent_by = 4L,
    start_comments_with_one_space = TRUE
)

do.call(styler::style_pkg, styler_confs)
do.call(styler::style_dir, c(list(path = here::here("scripts")), styler_confs))
do.call(styler::style_dir, c(list(path = here::here("reference_data")), styler_confs))
do.call(styler::style_dir, c(list(path = here::here("tests")), styler_confs))
do.call(styler::style_dir, c(list(path = here::here("tools")), styler_confs))
source(here::here("reference_data", "build_package_data.R"))
devtools::document()
devtools::test()
