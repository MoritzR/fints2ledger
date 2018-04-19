import os
import yaml

class Config:
    DEFAULT_CONFIG = """\
fints:
  blz: <your bank's BLZ>
  account: <your account number>
  password: <your banking password>
  endpoint: <your bank fints endpoint>

ledger:
  prompts: 
    - credit_account
    - debit_account
  autocomplete:
    accounts:
      - credit_account
      - debit_account
  defaults:
    debit_account: assets:bank:checking
"""

    def setup_files(self):
        if not os.path.exists('config.yml'):
            with open('config.yml', 'w') as config_file:
                print(Config.DEFAULT_CONFIG)
                config_file.write(Config.DEFAULT_CONFIG)
    
    def get_config(self):
        config = {}
        with open("config.yml", "r") as config_file:
            config = yaml.load(config_file.read())
        return config