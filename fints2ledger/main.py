from fints2ledger.config import Config
from fints2ledger.fints2csv import Fints2Csv
from fints2ledger.csv2ledger import Csv2Ledger

'''
This requires a "config.yml" file in the same folder, according to the following format:
fints:
  blz: <your bank's BLZ>
  account: <your account number>
  password: <your banking password>
  endpoint: <your bank fints endpoint>
'''


def main():
    config = Config().get_config()

    if config["fints"]["convert_to_csv"]:
        Fints2Csv(config).retrieveAndSave()
    if config["ledger"]["convert_to_ledger"]:
        Csv2Ledger(config).convertToLedger()


if __name__ == '__main__':
    main()
