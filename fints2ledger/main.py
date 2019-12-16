from fints2ledger.config import Config
from fints2ledger.fints2csv import Fints2Csv
from fints2ledger.csv2ledger import Csv2Ledger

def main():
    config = Config().get_config()

    if config["fints"]["convert_to_csv"]:
        Fints2Csv(config).retrieveAndSave()
    if config["ledger"]["convert_to_ledger"]:
        Csv2Ledger(config).convertToLedger()


if __name__ == '__main__':
    main()
