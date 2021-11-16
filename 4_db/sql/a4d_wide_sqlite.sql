-- import to SQLite by running: sqlite3.exe db.sqlite3 -init sqlite.sql

PRAGMA journal_mode = MEMORY;
PRAGMA synchronous = OFF;
PRAGMA foreign_keys = OFF;
PRAGMA ignore_check_constraints = OFF;
PRAGMA auto_vacuum = NONE;
PRAGMA secure_delete = OFF;
BEGIN TRANSACTION;


CREATE TABLE `trackers` (
`iid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
`ihash` TEXT,
`iname` TEXT,
`itracker_year` int
);

CREATE TABLE `clinics` (
`cid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
`cname` TEXT,
`ccountry` TEXT,
`caddress` TEXT,
`ccomment` TEXT
);

CREATE TABLE `patients` (
`pid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
`pid_a4d` TEXT,
`pname` TEXT,
`pdob` TEXT,
`pprovince` TEXT,
`pgender` TEXT,
`page_diagnosis` float,
`precruitment_date` date,
`pbaseline_hba1c` float,
`pbaseline_fbg` float
);

CREATE TABLE `patients_calculated` (
`pid` int NOT NULL,
`page` float,
`pbmi` float,
`ptesting_frq_pday` float,
`pest_strips_pmonth` float
);

CREATE TABLE `patients_latest` (
`pid` int NOT NULL,
`pcid` int,
`pedu_occ` TEXT,
`pstatus` TEXT,
`plast_clinic_visit` date,
`pinsulin_regimen` TEXT,
`pval_hba1c` float,
`pdate_hba1c` date,
`pval_fbg` float,
`pdate_fbg` date,
`phospitalization_last` date
);

CREATE TABLE `patient_changes` (
`pid` int,
`pvar` TEXT,
`pval_old` TEXT,
`pval_new` TEXT,
`pdate` date
);

CREATE TABLE `measurement_visits_w` (
`pid` int,
`cid` int,
`iid` int,
`mval_hba1c` float,
`mdate_hba1c` date,
`mval_fbg` float,
`mdate_fbg` date,
`mval_fbg_sample` TEXT,
`msupport_from_a4d` TEXT,
`mstatus` TEXT,
`minsulin_regimen` TEXT,
`mbloodp_sys` float,
`mbloodp_dias` float,
`mweight` float,
`mheight` float,
`mbmi_date` date,
`medu_occ` TEXT,
`mhospitalization` date,
`mlast_clinic_visit` date,
`msupport_additional` TEXT,
`mremarks` TEXT,
`mcompl_scr_date` date,
`mcompl_scr_comment` TEXT,
`mage` float,
`mbmi` float,
`mtesting_fqr_pday` float,
`mest_strips_pmonth` float
);

CREATE TABLE `products` (
`rid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
`rname` TEXT,
`rcontaining` int
);

CREATE TABLE `released_products` (
`rid` int,
`pid` int,
`runits` int,
`rdate` date,
`cid` int,
`iid` int
);

CREATE TABLE `current_product_status` (
`pid` int,
`rid` int,
`rquantity_current` int
);
ALTER TABLE `patients_calculated` ADD FOREIGN KEY (`pid`) REFERENCES `patients` (`pid`);
ALTER TABLE `patients_latest` ADD FOREIGN KEY (`pid`) REFERENCES `patients` (`pid`);
ALTER TABLE `patient_changes` ADD FOREIGN KEY (`pid`) REFERENCES `patients` (`pid`);
ALTER TABLE `measurement_visits_w` ADD FOREIGN KEY (`pid`) REFERENCES `patients` (`pid`);
ALTER TABLE `measurement_visits_w` ADD FOREIGN KEY (`cid`) REFERENCES `clinics` (`cid`);
ALTER TABLE `measurement_visits_w` ADD FOREIGN KEY (`iid`) REFERENCES `trackers` (`iid`);
ALTER TABLE `released_products` ADD FOREIGN KEY (`rid`) REFERENCES `products` (`rid`);
ALTER TABLE `released_products` ADD FOREIGN KEY (`iid`) REFERENCES `trackers` (`iid`);
ALTER TABLE `released_products` ADD FOREIGN KEY (`pid`) REFERENCES `patients` (`pid`);
ALTER TABLE `released_products` ADD FOREIGN KEY (`cid`) REFERENCES `clinics` (`cid`);
ALTER TABLE `current_product_status` ADD FOREIGN KEY (`pid`) REFERENCES `patients` (`pid`);
ALTER TABLE `current_product_status` ADD FOREIGN KEY (`rid`) REFERENCES `products` (`rid`);





COMMIT;
PRAGMA ignore_check_constraints = ON;
PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
