from ledgerautosync.converter import CsvConverter, Posting, Transaction, Amount
import datetime
import re
import jdatetime
import persian
import json
import os


class Eghtesad(CsvConverter):
    FIELDSET = set(['enbank'])

    def __init__(self, *args, **kwargs):
        super(Eghtesad, self).__init__(*args, **kwargs)
        self.name = 'Assets:Bank:Eghtesad'
        dirname = os.path.dirname(os.path.abspath(__file__))
        with open(os.path.join(dirname, 'accounts.json')) as f:
            self.accounts = json.load(f)

    def convert(self, row):
        desc = persian.convert_fa_numbers(row['enbank'])
        metadata = {
            'Desc.': desc
        }
        inputAmount = int(row['3'])
        outputAmount = int(row['4'])
        amount = 0
        reverse = True

        accountNumber = ""
        matchNumber = re.findall('(\d{7,9})', desc)
        if matchNumber:
            accountNumber = int(matchNumber[0])

        fromAccount = self.unknownaccount
        foundAccount = self.accounts.get(str(accountNumber), None)
        payee = 'Unknown'
        if accountNumber is not '' and foundAccount:
            fromAccount = foundAccount['account']
            payee = foundAccount['name']

        if accountNumber is not '':
            payee = "(" + str(accountNumber) + ") " + payee

        if outputAmount > 0:
            amount = outputAmount / 10000
            reverse = True
        else:
            amount = inputAmount / 10000
            reverse = False
            fromAccount = 'Income:Salary'

        jdatestring = row['1']
        jdate = jdatetime.date(
            int(jdatestring[0:4]), int(jdatestring[5:7]),
            int(jdatestring[8:]))
        date = jdate.togregorian()
        metadata['Date'] = jdate.isoformat() + ', ' + jdate.j_weekdays_en[
            jdate.weekday()]
        metadata['Time'] = row['2']

        unit = 'kIRT'

        return Transaction(
            cleared=True,
            date=date,
            payee=payee,
            metadata=metadata,
            postings=[
                Posting(
                    account=fromAccount,
                    amount=Amount(amount, unit, reverse=not reverse)),
                Posting(
                    account=self.name,
                    amount=Amount(amount, unit, reverse=reverse))
            ])
