import unittest
from unittest.mock import Mock
from csv_converter import CsvConverter
from mt940.models import Amount, Date
from ledger_writer import LedgerWriter


class LedgerWriterTest(unittest.TestCase):
    def setUp(self):
        self.writer = LedgerWriter(";")
        self.maxDiff=None

    def test_write_to_ledger(self):
        expected_entry="""\
2018/03/19 someone someone some kind of credit
    debit_account                                                   EUR 535.0
    credit_account                                                  EUR -535.0
"""
        csv_line="2018/03/19;535;EUR;someone;some kind of credit;some description"

        actual_entry = self.writer.journal_entry(csv_line, "credit_account", "debit_account")

        self.assertEquals(expected_entry, actual_entry)


if __name__ == '__main__':
    unittest.main()
