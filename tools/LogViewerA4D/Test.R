logError <- function(string,string2="",string3="",string4="",string5="",string6="",string7="")
{
  print(paste0(string,string2,string3,string4,string5,string6,string7))
}
logWarn <- function(string,string2="",string3="",string4="",string5="",string6="",string7="")
{
  print(paste0(string,string2,string3,string4,string5,string6,string7))
}

e <- "[e]"
x <- "[x]"
id <- "[id]"
field <- "[w]"
fixed_sex <- "[fixed_sex]"
sex <- "[sex]"
w <- "[w]"
unknown_column_names<- "[unknown_column_names]"
tracker_name <- "[tracker_name]"
col_name <- "[col_name]"
col <- "[col]"
curr_sheet <- "[curr_sheet]"
curr_sheet <- "[tracker_name]"
cols_extra <- "[cols_extra]"
cols_missing <- "[cols_missing]"
non_processed_dates <-"[non_processed_dates]"
print(c(
logError("Error in calculating the most frequent 'product_hospital': ", e),
logError("Error in calculating the most frequent 'product_country': ", e),
logError(paste("Warning in converting", field, ": ", w))  ,
logWarn(paste("Error in converting", field, ": ", e)),
logError("Could not convert value ", x, " in column ", col_name, " for patient: ", id),
logWarn("Could not convert value ", x, " in column ", col_name, " for patient: ", id),
logWarn("Patient ", id, ": Value ", x, " for column ", col, " is not in the list of allowed values. ") ,
logWarn("Patient ", id, ": calculated age is negative. Something went wrong.") ,
logWarn("Patient ", id, ": the weight is out of bounds.") ,
logWarn("Patient ", id, ": the height is out of bounds.") ,
logWarn("Patient ", id, ": sex ", sex, " is not in the list of synonyms. Replacing it with ", fixed_sex, ".") ,
logWarn("Patient ", id, ": id cannot be matched to a 7 letter alpha numeric code like XX_YY001. ") ,
logWarn("Patient ", id, ": id was truncated because it is longer than 8 characters.") ,
logError("Patient ", id, ": id is not valid.") ,
logWarn("Unknown column names in sheet: ", paste(unknown_column_names, collapse = ", "))
))

e$messagee<- "[e]"
w$messagee<- "[w]"

print(c(
logError("Could not link csv files for product and patient data. Error: ", e$message) ,
logWarn("Could not link csv files for product and patient data. Warning: ", w$message) ,
logError(curr_sheet, " trying with num_na_rows for products. Error: ", e$message) ,
logError(curr_sheet, " trying with non_processed_dates in product_entry_date. Error: ", e$message) ,
logError("Error in loading stock product list: ", e) ,
logError("Could not process ", tracker_name, ". Error = ", e$message, ".") ,
logWarn("Could not process ", tracker_name, ". Warning = ", w$message, ".") ,
logError("Could not process patient data. Error = ", e$message, ".") ,
logWarn("Could not process patient data. Warning = ", w$message, ".") ,
logError("Could not process product data. Error = ", e$message, ".") ,
logWarn("Could not process product data. Warning = ", w$message, ".") ,
logWarn("No product data in the file") ,
logError("Could not process raw patient data. Error = ", e$message, ".") ,
logWarn("Could not process raw patient data. Warning = ", w$message, ".") ,
logError("Could not process raw product data. Error = ", e$message, ".") ,
logWarn("Could not process raw product data. Warning = ", w$message, ".") ,
logWarn("Extra columns in patient data: ", paste(cols_extra, collapse = ", ")) ,
logWarn("Missing columns in patient data: ", paste(cols_missing, collapse = ", ")) ,
logError("Could not create table csv for static patient data. Error: ", e$message) ,
logWarn("Could not create table csv for static patient data. Error: ", w$message) ,
logError("Could not create table csv for monthly patient data. Error: ", e$message) ,
logWarn("Could not create table csv for monthly patient data. Error: ", w$message) ,
logError("Could not create table csv for longitudinal patient data. Error: ", e$message) ,
logWarn("Could not create table csv for longitudinal patient data. Error: ", w$message) ,
logError("Could not create table for product data. Error: ", e$message) ,
logWarn("Could not create table for product data. Warning: ", w$message) ,
logError("Could not create clinic data static table. Error: ", e$message) ,
logWarn("Could not create clinic data static table. Warning: ", w$message) ,
logError("Could not link files for product and patient data. Error: ", e$message) ,
logWarn("Could not link files for product and patient data. Warning: ", w$message)
))


logWarn("[curr_sheet] the number of rows with non-processed dates in product_entry_date is nrow(non_processed_dates)]: [non_processed_dates$product_entry_date")
logWarn("The number of [row_category] with empty [col_category] is [nrow(df_row_sums)]: [df_row_sums$row_name]")

