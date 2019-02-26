from ledgerautosync.converter import CsvConverter, Posting, Transaction, Amount
import datetime
import re
import jdatetime
import persian
import json
import os

t = {
    'mande': 'مانده',
    'bestankar': 'مبلغ بستانكار',
    'bedehkar': 'مبلغ بدهكار',
    'sharh': 'شرح',
    'nam': 'نام واريز كننده',
    'tarikh': 'تاريخ تراكنش',
    'time': 'زمان تراكنش',
    'radif': 'رديف'
}


class Mellat(CsvConverter):
    FIELDSET = set([t['radif'], t['mande']])

    def __init__(self, *args, **kwargs):
        super(Mellat, self).__init__(*args, **kwargs)
        self.name = 'Assets:Bank:Mellat'
        dirname = os.path.dirname(os.path.abspath(__file__))
        with open(os.path.join(dirname, 'accounts.json')) as f:
            self.accounts = json.load(f)

    def convert(self, row):
        metadata = {
            'Desc.': persian.convert_fa_numbers(row[t['sharh']]),
        }
        inputAmount = int(row[t['bestankar']])
        outputAmount = int(row[t['bedehkar']])
        amount = 0
        reverse = True

        name = persian.convert_fa_numbers(row[t['nam']])
        matchNumber = re.findall('(\d+)', name)
        accountNumber = ''
        if matchNumber:
            accountNumber = matchNumber[0]
            name = name.replace(accountNumber, ' ' + accountNumber + ' ')
        metadata['Name'] = name

        fromAccount = self.unknownaccount
        foundAccount = self.accounts.get(str(accountNumber), None)
        payee = name
        if accountNumber is not '' and foundAccount:
            fromAccount = foundAccount['account']
            payee = foundAccount['name']

        if accountNumber is not '':
            payee = "(" + accountNumber + ") " + payee

        if outputAmount > 0:
            amount = outputAmount / 10000
            reverse = True
        else:
            amount = inputAmount / 10000
            reverse = False
            fromAccount = 'Income:Salary'

        jdatestring = row[t['tarikh']]
        jdate = jdatetime.date(
            int(jdatestring[0:4]), int(jdatestring[4:6]),
            int(jdatestring[6:8]))
        date = jdate.togregorian()
        metadata['Date'] = jdate.isoformat() + ', ' + jdate.j_weekdays_en[
            jdate.weekday()]
        metadata['Time'] = row[t['time']]

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
