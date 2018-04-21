import unittest
import fints2ledger.utils as utils


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


if __name__ == '__main__':
    unittest.main()
