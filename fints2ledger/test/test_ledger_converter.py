import unittest
from unittest.mock import patch
import re
from fints2ledger.ledger_converter import LedgerConverter
from fints2ledger.ledger_converter import fill 


class LedgerConverterTest(unittest.TestCase):
    def setUp(self):
        config = {"ledger": {"md5": ["date", "payee", "purpose", "amount"]}}
        self.writer = LedgerConverter(config)

    def test_write_to_ledger(self):
        expected_entry = """\
2018/03/19 someone some kind of credit some description
    ; md5sum: e7224d45e6102ad5cb5fc7587ffee349
    test:debit                                                      EUR 535.0
    test:credit                                                     EUR -535.0
"""
        data = {
            "date": "2018/03/19",
            "amount": "535",
            "currency": "EUR",
            "payee": "someone",
            "posting": "some kind of credit",
            "purpose": "some description",
            "debit_account": "test:debit",
            "credit_account": "test:credit"
        }

        actual_entry = self.writer.journal_entry(data)

        self.assertEquals(expected_entry, actual_entry)

    def test_missing_autocomplete_file(self):
        with patch("fints2ledger.ledger_converter.input", return_value="some entry", create=True):
            try:
                self.writer.prompt_for_input("inputPromptWithoutFile")
            except KeyError:
                pass
            else:
                return
            self.fail(
                "prompt_for_input shouldn't raise error when prompting for input without a matching autocomplete file.")

    def test_should_fill_when_regex_matches(self):
        prefill_config = [
            {
                "match": {"purpose": ".*SUPERMARKET.*"},
                "fill": {"credit_account": "expenses:daily:groceries"}
            }
        ]
        transaction = {
            "date": "2018/03/19",
            "amount": "535",
            "currency": "EUR",
            "payee": "someone",
            "posting": "some kind of credit",
            "purpose": "Thank you for your purchase at SUPERMARKET",
        }

        result = fill(transaction, prefill_config)

        self.assertEquals(result, {"credit_account": "expenses:daily:groceries"})

    def test_should_only_fill_when_all_matches_match(self):
        credit_account_key = "credit_account"
        prefill_config = [
            {
                "match": {"purpose": ".*VACATION.*", "payee": "vacation_company"},
                "fill": {credit_account_key: "expenses:vacation"}
            }
        ]
        matching_transaction = {
            "payee": "vacation_company",
            "purpose": "VACATION on an island",
        }
        other_transaction = {
            "payee": "spouse",
            "purpose": "VACATION on an island",
        }

        matching_result = fill(matching_transaction, prefill_config)
        other_result = fill(other_transaction, prefill_config)

        self.assertEquals(matching_result, {"credit_account": "expenses:vacation"})
        self.assertEquals(other_result, {})


if __name__ == '__main__':
    unittest.main()
