import unittest
import unittest.mock as mock
import re
from fints2ledger.ledger_converter import LedgerConverter
from fints2ledger.ledger_converter import fill
from fints2ledger.ledger_converter import print_transaction
from fints2ledger.ledger_converter import get_remaining_prompts_from_prefill


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
        with mock.patch("fints2ledger.ledger_converter.input", return_value="some entry", create=True):
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

        self.assertEquals(
            result, {"credit_account": "expenses:daily:groceries"})

    def test_should_not_fill_None_values(self):
        prefill_config = [
            {
                "match": {"purpose": ".*SUPERMARKET.*"},
                "fill": {"credit_account": "expenses:daily:groceries", "purpose": None}
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

        self.assertEquals(
            result, {"credit_account": "expenses:daily:groceries"})

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

        self.assertEquals(matching_result, {
                          "credit_account": "expenses:vacation"})
        self.assertEquals(other_result, {})
    
    def test_should_list_None_prefills_as_remaining_prompts(self):
        credit_account_key = "credit_account"
        prefill_config = [
            {
                "match": {"purpose": ".*VACATION.*", "payee": "vacation_company"},
                "fill": {credit_account_key: "expenses:vacation", "purpose": None}
            }
        ]
        transaction = {
            "payee": "vacation_company",
            "purpose": "VACATION on an island",
        }

        self.assertEquals(get_remaining_prompts_from_prefill(transaction, prefill_config), ["purpose"])

        
    def test_should_return_None_if_transaction_is_not_matching(self):
        credit_account_key = "credit_account"
        prefill_config = [
            {
                "match": {"purpose": ".*SUPERMARKET.*", "payee": "vacation_company"},
                "fill": {credit_account_key: "expenses:vacation", "purpose": None}
            }
        ]
        transaction = {
            "payee": "vacation_company",
            "purpose": "VACATION on an island",
        }

        self.assertEquals(get_remaining_prompts_from_prefill(transaction, prefill_config), None)


    @mock.patch("fints2ledger.ledger_converter.print")
    def test_prints_transaction_in_uncidoe(self, mock_print):
        print_transaction({
            "purpose": "😀"
        })
        self.assertIn("😀", mock_print.call_args_list[0][0][0])


if __name__ == '__main__':
    unittest.main()
