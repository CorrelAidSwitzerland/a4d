## Welcome!

Below is a description of the folders and the files we find in these

- **data**: Files containing dummy data to start setting up the scripts
- **R**: Contains the scripts to extract, and preprocess the patient and the product data.
    - *patient_tracker_extract_helper* : Helper functions to extract the data for the patients (needed to run the *patient_tracker_extract* script)
    - *helper_product_data*: Helper functions to extract the data for the products
    - *patient_tracker_extract*: Script that reads in the different raw (Excel) trackers for each clinic and year and extracts the data into an machine readable table.
    - *patient_tracker_format*: Script that reads in the output of *patient_tracker_extract* and reformats columns according to the codebook indications, performs checks and removes duplicates (i.e., patients whose information is copied across months but remains unchanged). Returns a dataframe that  is ready to input in the database and another dataframe indicating the locations of errors or non-readable data.
- **4ADMonthlyTrackerCodebook**: Codebook containing the information on the variables that we are extracting from the trackers (patient and product data). Also contains tabs listing the different labels that one variable may have together with its standardized formulation.

## Setup

We use the [renv](https://rstudio.github.io/renv/index.html) package to manage dependencies.

This project was setup with `renv` and uses a local `.Rprofile` file that activates `renv`. The first time you open this project in RStudio, this will check if `renv` is already installed, and, if that is not the case, install it.

You are then informed about the difference between your local R environment and the packages used for this project. Check your console in RStudio after opening. You should see something like:

```R
* One or more packages recorded in the lockfile are not installed.
* Use `renv::status()` for more details.
```

You can use `renv::status()` to see which packages will be installed. Once you are ready, run

```R
renv::restore()
```

and `renv` will install all packages with the version as stated in the `renv.lock` file.

### Updating the Lockfile

See [collaborating](https://rstudio.github.io/renv/articles/collaborating.html) for the full details.

While working on a project, you or your collaborators may need to update or install new packages in your project. When this occurs, you’ll also want to ensure your collaborators are then using the same newly-installed packages. In general, the process looks like this:

- A user installs, or updates, one or more packages in their local project library;
- That user calls `renv::snapshot()` to update the `renv.lock` lockfile;
- That user then shares the updated version of `renv.lock` with their collaborators (meaning that this file is commited and pushed via `git`);
- Other collaborators then call `renv::restore()` to install the packages specified in the newly-updated lockfile.

If you want to add another package to this project, install the package with `renv::install()` instead of `package.install()`.

**Note**: Not all packages are locked by `renv`. For example, if you want to preview this Readme with RStudio, RStudio will likely ask to install or update additional packages like `markdown`. That is ok and intended because these additional packages are not used by the project code so it is up to you to install them or not.

### Loading the code

Once you have installed the dependencies with `renv::restore()` you can go on and load our "package".

However, you need one more dependency, and that is [devtools](https://devtools.r-lib.org/). This is because `renv` does only lock required packages that are needed to run the code, not packages to develop the code (like `devtools`).

So make sure to run

```r
install.packages("devtools")
```

It is also possible that this package was already downloaded because we have this code in the `.Rprofile` file that is executed automatically when RStudio is opened:

```r
if (interactive()) {
    require("devtools", quietly = TRUE)
    # automatically attaches usethis
    devtools::load_all()
}
```

Now you have access to `devtools`, which is in fact a set of packages that are installed, like `usethis` and `roxygen2`.

To load all functions within the `./R` folder, run 

```r
devtools::load_all()
```

This will make available the `a4d` package in the global environment, giving you access to all functions within it, as well as the core packages of `tidyverse` because it is listed at "Depends" in the DESCRIPTION file.

You can now go ahead and run one of the two main scripts:
- `run_a4d_patient_data.R`
- `run_a4d_product_data.R`

### Loading the data

We will all have the encrypted data stored on different folders within our computers.
To account for the different file paths for every user and to speed the selection of the tracker files (where the data is stored), there is the following solution:

- Run `usethis::edit_r_environ()`
    - This should open the following file: `.Renviron`
- Add the following line:
    - `A4D_DATA_ROOT = "your_path"`
    - Replace `"your_path"` with the path to your A4D tracker files
        - E.g. `A4D_DATA_ROOT = "D:/A4D"`
- Save the `.Renviron` file
- You are good to go and will not need to re-select the folder containing the tracker files when running `select_A4D_directory()`.  This function will now get the correct path from the `.Renviron` file.

## Development workflow

For a short overview, see [cheatsheets](https://devtools.r-lib.org/#cheatsheet).

If you want to change any (code) file, add new files or delete existing files, please follow these steps:

1. only once: `git clone` this repository
2. switch to the develop branch: `git checkout develop` or `git switch develop`
3. update develop: `git pull`
4. create a new branch: `git checkout -b <issue-no>-<tilte>` (or create the branch in GitHub and just switch to it, no `-b` needed then)
5. do your code changes, create new R files with `usethis::use_r()` and new test files with `usethis::use_test()`
6. load and test/execute your code changes: `devtools::load_all()`
7. run the tests: `devtools::test()`
   a. Fix any problems until all tests are green and make sure your changes do not break other code
8. check the package: `devtools::check()`
   a. Fix any problems until all checks are green
9. document your functions: use roxygen documentation by comments starting with `#'`
10. update documentation: `devtools:document()`
11. optional: add additional documentation to the README or create a RMarkdown file with examples
12. get latest changes from develop: `git merge develop`
   a. if there are any conflicts, solve them
13. create a (final) commit with all your changes: `git add <files>` and `git commit -m"<message>"`
14. push your changes: `git push` (for a newly created local branch, you will need to set an up-stream first, just follow the instructions on the command line)
15. Check the GitHub workflows (cicd pipelines) for your branch and fix any problems
16. Create a PR with develop as target
17. Again, check the GitHub workflows for your PR and fix any problems

In addition to this general workflow, there are some additional steps required if you made use of external packages not yet stored in `renv.lock`:

1. install the package: Use `renv::install()` if you want to use this package only for development, or `usethis::use_package()` to add the package to the DESCRIPTION file
   a. if you want to update a package, you can also use `renv::install()`, without arguments it will update all listed packages in the lock file
2. use the package with the `package::fun()` syntax in your code
3. use `renv::snapshot()` to update the `renv.lock`
4. make sure to add the `renv.lock` file with your PR

