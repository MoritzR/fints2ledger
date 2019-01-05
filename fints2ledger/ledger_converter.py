import hashlib
import re
from fints2ledger.autocomplete import Completer
import readline
import json
import os
from fints2ledger.config import Config

DEFAULT_PROMPTS = [
    "credit_account",
    "debit_account"
]

MD5_REGEX = r"((md5sum:) (.*))"


class LedgerConverter:
    def __init__(self, config):
        self.prompts = config["ledger"]["prompts"] if "prompts" in config["ledger"] else []
        self.config = config
        self.existing_md5_entries = []
        self.autocomplete_files = {}
        self.autocomplete_entries = {}

    def with_autocomplete_entries(self, entries_by_input):
        self.autocomplete_entries = entries_by_input

    def with_autocomplete_file(self, key, path_to_file):
        self.autocomplete_files[key] = path_to_file
        with open(path_to_file, "r") as file:
            self.autocomplete_entries[key] = list(
                map(lambda line: line.strip(), file.readlines()))

    def with_existing_journal(self, journal_lines):
        for line in journal_lines:
            match = re.search(MD5_REGEX, line)
            if match:
                self.existing_md5_entries.append(match.group(3))

    def prompt_for_input(self, input_key):
        if input_key not in self.autocomplete_entries:
            self.autocomplete_entries[input_key] = []
            
        existing_entries = self.autocomplete_entries[input_key]

        completer = Completer(existing_entries)
        readline.set_completer_delims(' \t\n;')
        readline.set_completer(completer.complete)
        readline.parse_and_bind('tab: complete')
        readline.set_completion_display_matches_hook(completer.display_matches)

        input_value = input("\n"+input_key+": ").strip()

        if input_value == "s":
            return None

        if input_value and input_value not in existing_entries:
            self.autocomplete_entries[input_key] += [input_value]
            if input_key in self.autocomplete_files:
                with open(self.autocomplete_files[input_key], "a") as file:
                    file.write(input_value)
                    file.write("\n")

        return input_value

    def journal_entry(self, data):
        template = Config.DEFAULT_TEMPLATE
        if os.path.exists("template.txt"):
            with open("template.txt", "r") as template_file:
                template = template_file.read()

        md5digest = None
        if "md5" in self.config["ledger"]:
            md5 = hashlib.md5()
            for entry in self.config["ledger"]["md5"]:
                md5.update(data[entry].encode("UTF-8"))
            md5digest = md5.hexdigest()
            if md5digest in self.existing_md5_entries:
                return None

        print(json.dumps(data, indent=1))
        input_dict = {}
        for input_key in self.prompts:
            input_value = self.prompt_for_input(input_key)
            if input_value == None:
                return None
            if input_value:
                input_dict[input_key] = input_value

        data.update(input_dict)

        data.update({
            'debit': float(data["amount"]),
            'credit': -float(data["amount"]),

            'md5sum': md5digest
        })

        # generate and clean output
        output_lines = template.format(**data).split('\n')
        output = '\n'.join([x.rstrip()
                            for x in output_lines if x.strip()]) + '\n'

        return output
