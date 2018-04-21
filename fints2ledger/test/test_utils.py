import unittest
import fints2ledger.utils as utils
from mt940.models import Date

class UtilsTest(unittest.TestCase):
    def test_update_dict_empty(self):
        '''update empty dict with another dict should yield the other dict'''
        empty_dict = {}
        other_dict = {"a": "b", 2: True}

        result_dict = utils.update_dict(empty_dict, other_dict)

        self.assertEqual(result_dict, other_dict)

    def test_update_dict(self):
        '''update dict with another dict should yield a dict with entries from both dicts'''
        dict_one = {"a": "b"}
        dict_two = {2: True}

        result_dict = utils.update_dict(dict_one, dict_two)

        self.assertEqual(result_dict, {"a": "b", 2: True})

    def test_update_dict_nest(self):
        '''update dict with another dict should yield a dict with entries from both dicts even when nested dicts are used'''
        dict_one = {"key": {"a": "b"}}
        dict_two = {"key": {2: True}}

        result_dict = utils.update_dict(dict_one, dict_two)

        self.assertEqual(result_dict, {"key": {"a": "b", 2: True}})
    
    def test_convert_mt940_date_2017(self):
        '''should correctly convert the string 2017/12/31 to a mt940 date'''

        result = utils.date_string_to_mt940_date("2017/12/31")

        self.assertEqual(result, Date(year="2017", month="12", day="31"))

    def test_convert_mt940_date_17(self):
        '''should correctly convert the string 17/12/31 to a mt940 date by adding 2000 to the year'''

        result = utils.date_string_to_mt940_date("17/12/31")

        self.assertEqual(result, Date(year="2017", month="12", day="31"))


if __name__ == '__main__':
    unittest.main()
