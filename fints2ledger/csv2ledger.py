from fints2ledger.ledger_converter import LedgerConverter
import csv
import os

class Csv2Ledger:
    def __init__(self, config):
        self.config = config

    def convertToLedger(self):
        print("""
            Controls:
                - enter 's' to skip an entry
                - Ctrl + C to abort
        """)

        writer = LedgerConverter(self.config)
        if os.path.exists(self.config["files"]["ledger_file"]):
            with open(self.config["files"]["ledger_file"], 'r') as existing_journal:
                writer.with_existing_journal(existing_journal.readlines())

        if "autocomplete" in self.config["ledger"]:
            for autocomplete_file in self.config["ledger"]["autocomplete"]:
                for autocomplete_key in self.config["ledger"]["autocomplete"][autocomplete_file]:
                    # add extension to have a grouping for autocomplete files
                    file_with_extension = os.path.join(self.config["files"]["files_path"], autocomplete_file + ".auto")
                    # create file if non-existent
                    if not os.path.exists(file_with_extension):
                        with open(file_with_extension, 'w'):
                            pass
                    writer.with_autocomplete_file(
                        autocomplete_key, file_with_extension)

        with open(self.config["files"]["csv_file"]) as csvfile, open(self.config["files"]["ledger_file"], 'a') as ledger_journal:
            reader = csv.DictReader(csvfile, delimiter=self.config["fints"]["csv_separator"])
            for row in reader:
                if "defaults" in self.config["ledger"]:
                    row.update(self.config["ledger"]["defaults"])
                entry = writer.journal_entry(row)
                if entry:
                    ledger_journal.write(entry)
                    ledger_journal.write("\n")
