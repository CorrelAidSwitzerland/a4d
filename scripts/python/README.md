## Preprocessing Tracker Files

This script can be used to replace patient names with patient ids.

The script can be called like this:

```bash
python main.py <src-directory>
```

The script will do the following:

1. Find all `.xlsx` files in the `<src-directory>` and its subdirectories.
2. For each `.xlsx` file, it will:
    1. Open the workbook.
    2. Look for the sheet `Patient List`.
    3. Get a list of all patient names and their ids.
    4. Go through all sheets in the workbook and replace the patient names with the patient ids.
    5. Save the file in `output/patient_data_without_names` directory next to the `src` directory.
    6. Create a log file under `output/logs/main_replace_patient_names.log`.
    
The script works in a way that it does (hopefully) not change the structure or format of the workbook. It only replaces the patient names with the patient ids by manipulating the value of single cells without changing their attributes.

## How to build

Run

```bash
pyinstaller main.py -F -n "a4d_replacer_tool"
```

to generate the executable. 
