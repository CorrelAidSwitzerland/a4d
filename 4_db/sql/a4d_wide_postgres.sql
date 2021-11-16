CREATE TABLE "trackers" (
  "iid" SERIAL UNIQUE PRIMARY KEY NOT NULL,
  "ihash" varchar,
  "iname" varchar,
  "itracker_year" int
);

CREATE TABLE "clinics" (
  "cid" SERIAL UNIQUE PRIMARY KEY NOT NULL,
  "cname" varchar,
  "ccountry" varchar,
  "caddress" varchar,
  "ccomment" varchar
);

CREATE TABLE "patients" (
  "pid" SERIAL UNIQUE PRIMARY KEY NOT NULL,
  "pid_a4d" varchar,
  "pname" varchar,
  "pdob" varchar,
  "pprovince" varchar,
  "pgender" varchar,
  "page_diagnosis" float,
  "precruitment_date" date,
  "pbaseline_hba1c" float,
  "pbaseline_fbg" float
);

CREATE TABLE "patients_calculated" (
  "pid" int NOT NULL,
  "page" float,
  "pbmi" float,
  "ptesting_frq_pday" float,
  "pest_strips_pmonth" float
);

CREATE TABLE "patients_latest" (
  "pid" int NOT NULL,
  "pcid" int,
  "pedu_occ" varchar,
  "pstatus" varchar,
  "plast_clinic_visit" date,
  "pinsulin_regimen" varchar,
  "pval_hba1c" float,
  "pdate_hba1c" date,
  "pval_fbg" float,
  "pdate_fbg" date,
  "phospitalization_last" date
);

CREATE TABLE "patient_changes" (
  "pid" int,
  "pvar" varchar,
  "pval_old" varchar,
  "pval_new" varchar,
  "pdate" date
);

CREATE TABLE "measurement_visits_w" (
  "pid" int,
  "cid" int,
  "iid" int,
  "mval_hba1c" float,
  "mdate_hba1c" date,
  "mval_fbg" float,
  "mdate_fbg" date,
  "mval_fbg_sample" varchar,
  "msupport_from_a4d" varchar,
  "mstatus" varchar,
  "minsulin_regimen" varchar,
  "mbloodp_sys" float,
  "mbloodp_dias" float,
  "mweight" float,
  "mheight" float,
  "mbmi_date" date,
  "medu_occ" varchar,
  "mhospitalization" date,
  "mlast_clinic_visit" date,
  "msupport_additional" varchar,
  "mremarks" varchar,
  "mcompl_scr_date" date,
  "mcompl_scr_comment" varchar,
  "mage" float,
  "mbmi" float,
  "mtesting_fqr_pday" float,
  "mest_strips_pmonth" float
);

CREATE TABLE "products" (
  "rid" SERIAL UNIQUE PRIMARY KEY NOT NULL,
  "rname" varchar,
  "rcontaining" int
);

CREATE TABLE "released_products" (
  "rid" int,
  "pid" int,
  "runits" int,
  "rdate" date,
  "cid" int,
  "iid" int
);

CREATE TABLE "current_product_status" (
  "pid" int,
  "rid" int,
  "rquantity_current" int
);

ALTER TABLE "patients_calculated" ADD FOREIGN KEY ("pid") REFERENCES "patients" ("pid");

ALTER TABLE "patients_latest" ADD FOREIGN KEY ("pid") REFERENCES "patients" ("pid");

ALTER TABLE "patient_changes" ADD FOREIGN KEY ("pid") REFERENCES "patients" ("pid");

ALTER TABLE "measurement_visits_w" ADD FOREIGN KEY ("pid") REFERENCES "patients" ("pid");

ALTER TABLE "measurement_visits_w" ADD FOREIGN KEY ("cid") REFERENCES "clinics" ("cid");

ALTER TABLE "measurement_visits_w" ADD FOREIGN KEY ("iid") REFERENCES "trackers" ("iid");

ALTER TABLE "released_products" ADD FOREIGN KEY ("rid") REFERENCES "products" ("rid");

ALTER TABLE "released_products" ADD FOREIGN KEY ("iid") REFERENCES "trackers" ("iid");

ALTER TABLE "released_products" ADD FOREIGN KEY ("pid") REFERENCES "patients" ("pid");

ALTER TABLE "released_products" ADD FOREIGN KEY ("cid") REFERENCES "clinics" ("cid");

ALTER TABLE "current_product_status" ADD FOREIGN KEY ("pid") REFERENCES "patients" ("pid");

ALTER TABLE "current_product_status" ADD FOREIGN KEY ("rid") REFERENCES "products" ("rid");
