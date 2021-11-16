CREATE TABLE `trackers` (
  `iid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
  `ihash` varchar(255),
  `iname` varchar(255),
  `itracker_year` int
);

CREATE TABLE `clinics` (
  `cid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
  `cname` varchar(255),
  `ccountry` varchar(255),
  `caddress` varchar(255),
  `ccomment` varchar(255)
);

CREATE TABLE `patients` (
  `pid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
  `pid_a4d` varchar(255),
  `pname` varchar(255),
  `pdob` varchar(255),
  `pprovince` varchar(255),
  `pgender` varchar(255),
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
  `pedu_occ` varchar(255),
  `pstatus` varchar(255),
  `plast_clinic_visit` date,
  `pinsulin_regimen` varchar(255),
  `pval_hba1c` float,
  `pdate_hba1c` date,
  `pval_fbg` float,
  `pdate_fbg` date,
  `phospitalization_last` date
);

CREATE TABLE `patient_changes` (
  `pid` int,
  `pvar` varchar(255),
  `pval_old` varchar(255),
  `pval_new` varchar(255),
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
  `mval_fbg_sample` varchar(255),
  `msupport_from_a4d` varchar(255),
  `mstatus` varchar(255),
  `minsulin_regimen` varchar(255),
  `mbloodp_sys` float,
  `mbloodp_dias` float,
  `mweight` float,
  `mheight` float,
  `mbmi_date` date,
  `medu_occ` varchar(255),
  `mhospitalization` date,
  `mlast_clinic_visit` date,
  `msupport_additional` varchar(255),
  `mremarks` varchar(255),
  `mcompl_scr_date` date,
  `mcompl_scr_comment` varchar(255),
  `mage` float,
  `mbmi` float,
  `mtesting_fqr_pday` float,
  `mest_strips_pmonth` float
);

CREATE TABLE `products` (
  `rid` int UNIQUE PRIMARY KEY NOT NULL AUTO_INCREMENT,
  `rname` varchar(255),
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
