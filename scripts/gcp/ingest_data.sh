bq load --source_format CSV --clustering_fields id,tracker_year,tracker_month --encoding UTF-16LE --project_id=a4d-315220 --skip_leading_rows 1 --max_bad_records 100 a4d_ds_tracker.patient_data "gs://a4d-315220-clean-data/patient_data/*.csv" scripts/gcp/schema.json