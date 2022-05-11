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
