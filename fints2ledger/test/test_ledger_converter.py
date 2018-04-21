import unittest
from unittest.mock import Mock
from fints2ledger.csv_converter import CsvConverter
from mt940.models import Amount, Date
from fints2ledger.ledger_converter import LedgerConverter


class LedgerConverterTest(unittest.TestCase):
    def setUp(self):
        config = {"ledger": {"md5": ["date", "payee", "purpose", "amount"]}}
        self.writer = LedgerConverter(config)
        self.maxDiff = None

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


if __name__ == '__main__':
    unittest.main()
