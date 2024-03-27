import logging
import shutil
from pathlib import Path

import click
import openpyxl
import pandas as pd

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("replace patient names")

PATIENT_DATA_RANGE = ("B1", "C200")


@click.command("replace")
@click.argument("src", type=click.Path(exists=True))
@click.option("--output", "-o", default="output", help="Output directory")
@click.version_option("0.1.0", prog_name="replace patient names")
def replace_name_with_id(src: str, output: str):
    excel_files = list(Path(src).rglob("*.xlsx"))

    if not excel_files:
        raise ValueError(f"No excel files found in directory {src}.")

    output_dir = Path(src).parent / output / "patient_data_without_names"
    log_dir = Path(src).parent / output / "logs"

    if not output_dir.exists():
        output_dir.mkdir(parents=True)
        logger.info(f"Created output directory {output_dir} under {src}.")

    if not log_dir.exists():
        log_dir.mkdir(parents=True)
        logger.info(f"Created log directory {log_dir} under {src}.")

    logger.addHandler(logging.FileHandler(log_dir / "main_replace_patient_names.log"))

    logger.info(f"Start processing {len(excel_files)} excel files.")
    for i, excel_file in enumerate(excel_files):
        logger.info(f"Start processing {excel_file.name} ({i+1}/{len(excel_files)}).")

        try:
            wb = openpyxl.load_workbook(str(excel_file))
        except openpyxl.BadZipFile:
            logger.warning(f"Failed to open {excel_file.name}. Skipping.")
            continue

        sheets = wb.sheetnames

        if "Patient List" not in sheets:
            logger.warning(f"Sheet 'Patient List' not found. Skipping.")
            # shutil.copy(excel_file, output_dir / excel_file.name)
            continue

        patient_data = pd.DataFrame(
            [
                (b.value, c.value)
                for b, c in wb["Patient List"][
                    PATIENT_DATA_RANGE[0] : PATIENT_DATA_RANGE[1]
                ]
                if b.value and c.value
            ],
            columns=["id", "name"],
        )
        patient_data = patient_data.iloc[1:]

        if all(patient_data.name == patient_data.id):
            logger.info(
                "Patient names are already replaced with ids. Copying original file to output dir."
            )
            shutil.copy(excel_file, output_dir / excel_file.name)
            continue

        for row in patient_data.itertuples(index=False):
            for sheet in sheets:
                ws = wb[sheet]

                for col in ws.iter_cols():
                    for cell in col:
                        if cell.value == row.name:
                            cell.value = row.id

        wb.save(output_dir / excel_file.name)
        logger.info(f"Saved changed file to {output_dir}.")
        logger.info(f"Finished processing {excel_file.name}.")


if __name__ == "__main__":
    replace_name_with_id()
