from ledgerautosync.converter import CsvConverter, Posting, Transaction, Amount
import datetime
import re
import jdatetime
import persian
import json
import os

t = {
    'mande': 'مانده',
    'mablagh': 'مبلغ گردش بدهکار',
    'variz': 'مبلغ گردش بستانکار',
    'sharh': 'شرح',
    'zinaf': 'واریز کننده/ ذیتفع',
    'time': 'زمان',
    'date': 'تاریخ',
    'radif': 'رديف'
}


class Mellat(CsvConverter):
    FIELDSET = set([t['mande'], t['mablagh']])

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
        amount = int(row[t['mablagh']])
        variz = int(row[t['variz']])
        amount = amount / 10000
        variz = variz / 10000

        name = persian.convert_fa_numbers(row[t['zinaf']])
        matchNumber = re.findall('(\d+)', name)
        accountNumber = ''
        if matchNumber:
            accountNumber = matchNumber[0]
            name = name.replace(accountNumber, ' ' + accountNumber + ' ')
        metadata['Name'] = name

        fromAccount = self.unknownaccount
        foundAccount = self.accounts.get(str(accountNumber), None)
        payee = name
        if accountNumber != '' and foundAccount:
            fromAccount = foundAccount['account']
            payee = foundAccount['name']

        if accountNumber != '':
            payee = "(" + accountNumber + ") " + payee

        if amount > 0:
            reverse = True
            amount = abs(amount)
        else:
            reverse = False
            amount = abs(variz)
            fromAccount = 'Income:Salary'

        jdatestring = row[t['date']]
        jdate = jdatetime.date(
            int(jdatestring[:4]), int(jdatestring[5:7]),
            int(jdatestring[8:]))
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
