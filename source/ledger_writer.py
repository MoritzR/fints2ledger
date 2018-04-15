import hashlib
import re

DEFAULT_TEMPLATE = """\
{date} {payee} {posting} {purpose}
    ; md5sum: {md5sum}
    {debit_account:<60}    {currency} {debit}
    {credit_account:<60}    {currency} {credit}
"""

MD5_REGEX = r"((md5sum:) (.*))"

class LedgerWriter:
    def __init__(self):
        self.existing_md5_entries=[]
    def with_existing_journal(self, journal_lines):
        for line in journal_lines:
            match=re.search(MD5_REGEX, line)
            if match:
                self.existing_md5_entries.append(match.group(3))
        
    def journal_entry(self, data):
        template = DEFAULT_TEMPLATE

        md5=hashlib.md5()
        md5.update(data["date"].encode("UTF-8"))
        md5.update(data["payee"].encode("UTF-8"))
        md5.update(data["purpose"].encode("UTF-8"))
        md5.update(data["amount"].encode("UTF-8"))
        md5digest=md5.hexdigest()
        if md5digest in self.existing_md5_entries:
            return None

        data.update({
            'debit': float(data["amount"]),
            'credit': -float(data["amount"]),

            'md5sum': md5digest
        })

        # generate and clean output
        output_lines = template.format(**data).split('\n')
        output = '\n'.join([x.rstrip()
                            for x in output_lines if x.strip()]) + '\n'

        return output
