import unittest
from unittest.mock import Mock
from transaction_retriever import TRetriever
from mt940.models import Date, Amount


class TRetrieverTest(unittest.TestCase):
    def test_calls_client_with_select_account(self):
        '''retriever should get the statements of the client with the selected account,
        when the client has two accounts'''
        startDate = Date(2017, 1, 1)
        endDate = Date(2017, 2, 1)
        selectedAccount = "selected account"

        clientMock = Mock()
        account1Mock = Mock()
        account1Mock.accountnumber = selectedAccount
        account2Mock = Mock()
        account2Mock.accountnumber = "other account"
        clientMock.get_sepa_accounts.return_value = [
            account1Mock, account2Mock]
        clientMock.get_statement.return_value = []
        retriever = TRetriever(clientMock, selectedAccount)

        retriever.get_hbci_transactions(startDate, endDate)

        clientMock.get_statement.assert_called_with(
            account1Mock, startDate, endDate)

    def test_calls_returns_correct_transaction(self):
        '''retrieved should return a transaction with data, amount and details from the hcbi data'''
        expectedDate = Date(2017, 1, 1)
        expectedDetails = "some details"
        expectedAmount = Amount("44", "D", "EUR")
        selectedAccount = "selected account"
        hbciData = Mock()
        data = {
            "amount": expectedAmount,
            "date": expectedDate,
            "transaction_details": expectedDetails
        }
        hbciData.data = data

        clientMock = Mock()
        account1Mock = Mock()
        account1Mock.accountnumber = selectedAccount
        clientMock.get_sepa_accounts.return_value = [account1Mock]
        clientMock.get_statement.return_value = [hbciData]
        retriever = TRetriever(clientMock, selectedAccount)

        result = retriever.get_hbci_transactions(Date(2017, 2, 2), Date(2017, 3, 3))

        self.assertEqual(result[0].date, expectedDate)
        self.assertEqual(result[0].amount, expectedAmount)
        self.assertEqual(result[0].details, expectedDetails)

if __name__ == '__main__':
    unittest.main()
