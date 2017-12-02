import unittest
from unittest.mock import Mock
from transaction_retriever import TRetriever
from mt940.models import Date


class TRetrieverTest(unittest.TestCase):
    def test_calls_client_with_select_account(self):
        '''retriever should get the statements of the client with the selected account,
        when the client has two accounts'''
        startDate = Date(2017, 1, 1)
        endDate = Date(2017, 2, 1)
        selectedAccount="selected account"

        clientMock = Mock()
        account1Mock = Mock()
        account1Mock.accountnumber = selectedAccount
        account2Mock = Mock()
        account2Mock.accountnumber = "other account"
        clientMock.get_sepa_accounts.return_value = [account1Mock, account2Mock]
        retriever = TRetriever(clientMock, selectedAccount)

        retriever.get_statements(startDate, endDate)

        clientMock.get_statement.assert_called_with(account1Mock, startDate, endDate)


if __name__ == '__main__':
    unittest.main()
