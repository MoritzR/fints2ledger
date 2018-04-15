import hashlib

DEFAULT_TEMPLATE = """\
{date} {payee} {posting} {purpose}
    ; md5sum: {md5sum}
    {debit_account:<60}    {currency} {debit}
    {credit_account:<60}    {currency} {credit}
"""

class LedgerWriter:
    def journal_entry(self, data):
        template = DEFAULT_TEMPLATE

        md5=hashlib.md5()
        md5.update(data["date"].encode("UTF-8"))
        md5.update(data["payee"].encode("UTF-8"))
        md5.update(data["purpose"].encode("UTF-8"))
        md5.update(data["amount"].encode("UTF-8"))

        data.update({
            'debit': float(data["amount"]),
            'credit': -float(data["amount"]),

            'md5sum': md5.hexdigest()
        })

        # generate and clean output
        output_lines = template.format(**data).split('\n')
        output = '\n'.join([x.rstrip()
                            for x in output_lines if x.strip()]) + '\n'

        return output
