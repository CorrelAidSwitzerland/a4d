## Welcome!

Below is a description of the folders and the files we find in these

-  **1_Orga** : Files related to organization of the project (e.g., timelines)

-  **2_Data**: Files containing dummy data to start setting up the scripts

-  **3_Code** : Contains the scripts to extract, and preprocess the patient and the product data.
      - *00_a4d_patient_tracker_extract_helper* : Helper functions to extract the data for the patients (needed to run the *01_a4d_patient_tracker_extract* script)
      - *00_helper_product_data* : Helper functions to extract the data for the products
      - *01_a4d_patient_tracker_extract* : Script that reads in the different raw (Excel) trackers for each clinic and year and extracts the data into an machine readble table.
      - *02_a4d_patient_tracker_format*: Script that reads in the output of *01_a4d_patient_tracker_extract* and reformats columns according to the codebook indications, performs checks and removes duplicates (i.e., patients whose information is copied across months but remains unchanged). Returns a dataframe that  is ready to input in the database and another dataframe indicating the locations of errors or non-readable data.
      - *OTHER PRODUCT SCRIPTS*: ....NEED TO ADD INFORMATION
      
-  **4_db**: Subfolders and files related to the organization and structure of the database

- **4ADMonthlyTrackerCodebook** : Codebook containing the information on the variables that we are extracting from the trackers (patient and product data). Also contains tabs listing the different labels that one variable may have together with its standardized formulation.

## Setup

We use the [renv](https://rstudio.github.io/renv/index.html) package to manage dependencies.

This project was setup with `renv` and uses a local `.Rprofile` file that activates `renv`. The first time you open this project in RStudio, this will check if `renv` is already installed, and, if that is not the case, install it.

You are then informed about the difference between your local R environment and the packages used for this project. Check your console in RStudio after opening. You should see something like:

```R
* One or more packages recorded in the lockfile are not installed.
* Use `renv::status()` for more details.
```

You can use `renv::status()` to see which packages will be installed. Once you ready, run

```R
renv::restore()
```

and `renv` will install all packages with the version as stated in the `renv.lock` file.

### Updating the Lockfile

See (collaborating)[https://rstudio.github.io/renv/articles/collaborating.html] for the full details.

While working on a project, you or your collaborators may need to update or install new packages in your project. When this occurs, you’ll also want to ensure your collaborators are then using the same newly-installed packages. In general, the process looks like this:

- A user installs, or updates, one or more packages in their local project library;
- That user calls `renv::snapshot()` to update the `renv.lock` lockfile;
- That user then shares the updated version of `renv.lock` with their collaborators (meaning that this file is commited and pushed via `git`);
- Other collaborators then call `renv::restore()` to install the packages specified in the newly-updated lockfile.

If you want to add another package to this project, install the package with `renv::install()` instead of `package.install()`.

**Note**: Not all packages are locked by `renv`. For example, if you want to preview this Readme with RStudio, RStudio will likely ask to install or update additional packages like `markdown`. That is ok and intended because these additional packages are not used by the project code so it is up to you to install them or not.
