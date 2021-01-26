import unittest
from unittest.mock import Mock
from fints2ledger.csv_converter import CsvConverter
from mt940.models import Amount, Date


class ConverterTest(unittest.TestCase):
    def setUp(self):
        self.csvConverter = CsvConverter(",")

    def test_convert_to_csv(self):
        expected_purpose = "purchased book"
        expected_applicant_name = "book store"
        expected_posting_text = "posting text"
        expected_date = Date(2017, 11, 1)
        expected_amount = Amount('44', 'D', 'EUR')
        fintsTransaction = {
            "date": expected_date,
            "amount": expected_amount,
            "applicant_name": expected_applicant_name,
            "posting_text": expected_posting_text,
            "purpose": expected_purpose
        }
        hbciData = Mock()
        hbciData.data = fintsTransaction

        csv_result = self.csvConverter.convert(hbciData)
        entries = csv_result.split(",")

        self.assertIn(expected_purpose, entries)
        self.assertIn(expected_applicant_name, entries)
        self.assertIn(expected_posting_text, entries)
        self.assertIn("2017/11/01", entries)
        self.assertIn("-44", entries)

    def test_convert_with_missing_values(self):
        fintsTransaction = {
            "date": Date(2017, 11, 1),
            "amount": Amount('44', 'D', 'EUR'),
            "applicant_name": None,
            "posting_text": None,
            "purpose": None
        }
        hbciData = Mock()
        hbciData.data = fintsTransaction

        csv_result = self.csvConverter.convert(hbciData)
        entries = csv_result.split(",")

        self.assertEquals(['2017/11/01', '-44', 'EUR', '', '', ''], entries)


    def test_uses_date_format(self):
        fintsTransaction = {
            "date": Date(2017, 11, 1),
            "amount": Amount('44', 'D', 'EUR'),
            "applicant_name": None,
            "posting_text": None,
            "purpose": None
        }
        hbciData = Mock()

        hbciData.data = { **fintsTransaction, "date": Date(2017, 11, 1) }
        result1 = CsvConverter(",", "%Y/%m/%d").convert(hbciData)

        hbciData.data = { **fintsTransaction, "date": Date(2017, 11, 1) }
        result2 = CsvConverter(",", "%Y-%m-%d").convert(hbciData)

        self.assertIn("2017/11/01", result1)
        self.assertIn("2017-11-01", result2)


if __name__ == '__main__':
    unittest.main()
