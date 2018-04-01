import unittest
from unittest.mock import Mock
from csv_converter import CsvConverter
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


if __name__ == '__main__':
    unittest.main()
