import unittest
import unittest.mock as mock
from fints2ledger.config import Config

class ConfigTest(unittest.TestCase):
    def setUp(self):
        self.config = Config()
        self.config.files_path = "~/.config/fints2ledger/"

    @mock.patch("fints2ledger.config.Path")
    @mock.patch("fints2ledger.config.open")
    @mock.patch("fints2ledger.config.os")
    def test_creates_files_in_user_config_path(self, mock_os, mock_open, *_):
        mock_os.path.exists.return_value=False
        mock_os.path.join = lambda a,b: a+b
        expectedCalls = [ mock.call("~/.config/fints2ledger/config.yml", "w")
                        , mock.call("~/.config/fints2ledger/template.txt", "w")
                        ]

        self.config.setup_files()
        
        self.assertEquals(mock_open.call_args_list, expectedCalls)

    @mock.patch("fints2ledger.config.open")
    @mock.patch("fints2ledger.config.glob")
    @mock.patch("fints2ledger.config.print")
    @mock.patch("fints2ledger.config.Path")
    @mock.patch("fints2ledger.config.os")
    # move local files create with fints2ledger version <= 0.4.4
    def test_moves_local_config_files_to_config_directory(self, mock_os, mock_path, mock_print, mock_glob, *_):
        mock_os.path.exists = lambda f: True if f in ["config.yml", "template.txt"] else False
        mock_os.path.join = lambda a,b: a+b
        mock_glob.glob.return_value = ["accounts.auto", "purpose.auto"]
        expectedCalls = [ mock.call("~/.config/fints2ledger/config.yml")
                        , mock.call("~/.config/fints2ledger/accounts.auto")
                        , mock.call("~/.config/fints2ledger/purpose.auto")
                        , mock.call("~/.config/fints2ledger/template.txt")
                        ]

        self.config.setup_files()
        
        self.assertEquals(mock_path.return_value.mkdir.call_args_list, [mock.call(exist_ok=True, parents=True)])
        self.assertEquals(mock_path.return_value.replace.call_args_list, expectedCalls)
        self.assertIn(mock.call("I moved your 'config.yml' to '~/.config/fints2ledger/'"), mock_print.call_args_list)
        self.assertIn(mock.call("I moved your 'template.txt' to '~/.config/fints2ledger/'"), mock_print.call_args_list)

    @mock.patch("fints2ledger.config.open")
    @mock.patch("fints2ledger.config.Path")
    @mock.patch("fints2ledger.config.os")
    def test_doesnt_try_to_move_files_when_they_dont_exist(self, mock_os, mock_path, *_):
        mock_os.path.exists.return_value = False

        self.config.setup_files()
        
        mock_path.return_value.replace.assert_not_called()

if __name__ == '__main__':
    unittest.main()
