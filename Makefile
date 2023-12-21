ROOT ?= "/Volumes/USB SanDisk 3.2Gen1 Media/a4d/output"
PROJECT ?= "a4d-315220"
DATASET ?= "tracker"

ingest:
	bq load --replace=true --source_format=PARQUET --clustering_fields=clinic_code,id,tracker_year,tracker_month --project_id=$(PROJECT) --max_bad_records=0 $(DATASET).patient_data_monthly $(ROOT)"/tables/patient_data_monthly.parquet"
	bq load --replace=true --source_format=PARQUET --clustering_fields=id,tracker_year,tracker_month --project_id=$(PROJECT) --max_bad_records=0 $(DATASET).patient_data_static $(ROOT)"/tables/patient_data_static.parquet"
	bq load --replace=true --source_format=PARQUET --clustering_fields=clinic_code,id,tracker_year,tracker_month --project_id=$(PROJECT) --max_bad_records=0 $(DATASET).patient_data_hba1c $(ROOT)"/tables/longitudinal_data_hba1c.parquet"
	bq load --replace=true --source_format=PARQUET --clustering_fields=product_hospital,product_released_to,product_table_year,product_table_month --project_id=$(PROJECT) --max_bad_records=0 $(DATASET).product_data_v2 $(ROOT)"/tables/product_data.parquet"

