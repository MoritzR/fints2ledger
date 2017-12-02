import unittest
from unittest.mock import Mock
from transaction_retriever import TRetriever
from hconverter import Transaction
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
        retriever = TRetriever(clientMock, selectedAccount)

        retriever.get_statements(startDate, endDate)

        clientMock.get_statement.assert_called_with(
            account1Mock, startDate, endDate)

    def test_saves_transactions_to_database_as_unclassified(self):
        expectedInsert = {
            "year": 2017,
            "month": 1,
            "day": 1,
            "amount": -44.5,
            "currency": "€",
            "details": "some details",
            "classified": False
        }
        dbMock = Mock()
        retriever = TRetriever(Mock(), Mock(), db=dbMock)
        transaction = Transaction(Date(2017, 1, 1), Amount(
            "44.5", "D", "EUR"), "some details")

        retriever.save_transaction(transaction)

        dbMock.insert.assert_called_with(expectedInsert)


if __name__ == '__main__':
    unittest.main()
