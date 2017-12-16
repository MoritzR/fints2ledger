from hconverter import Converter, Transaction
import unittest
from mt940.models import Amount
from mt940.models import Date


class ConverterTest(unittest.TestCase):
    def setUp(self):
        self.converter = Converter

    def test_convertToLedger_example1(self):
        transaction=Transaction(Date(2017,11,1), Amount('44', 'D', 'EUR'), "details", "orignalDetails")
        
        result=self.converter.transactionToLedger(transaction,"assets:cash","expenses:electricity", "spent money")

        self.assertIn("2017/11/01", result)
        self.assertIn("expenses:electricity", result)
        self.assertIn("assets:cash", result)
        self.assertIn("€-44", result)

    def test_convertToTransaction_example1(self):
        '''should correctly convert electric bill to transaction'''
        expectedDate = Date(2017, 11, 1)
        expectedAmount = Amount('44', 'D', 'EUR')
        expectedDetails = "Abschlagsanforderung Strom44,00/ 200100703297/ Blubblubstr. 6FVGRJNKC753DE87543551800005456782GASAG AG"
        hbciData = {
            "status": "D",
            "funds_code": None,
            "amount": expectedAmount ,
            "id": "NMSC",
            "customer_reference": "NONREF",
            "bank_reference": None,
            "extra_details": "",
            "currency": "EUR",
            "date": expectedDate,
            "entry_date": Date(2017,
                               1,
                               1),
            "transaction_details": "005?00Lastschrifteinzug?10006220?20EREF+004568581456?21MREF+01200\n700145817001?22CRED+DE5600500000727403?23SVWZ+Abschlagsanforderun\ng S?24trom44,00/ 200100703297/ Bl?25ubblubstr. 6?30FVGRJNKC753?31\nDE87543551800005456782?32GASAG AG"
        }

        result = self.converter.hbciDataToTransaction(hbciData)

        self.assertEqual(expectedDate, result.date)
        self.assertEqual(expectedAmount, result.amount)
        self.assertEqual(expectedDetails, result.details)

if __name__ == '__main__':
    unittest.main()
