#!/usr/bin/env python

import subprocess
import time
import jdatetime
from datetime import date


TMP_FILE = '/tmp/hiweb'
COOKIE_FILE = '/tmp/hiwebcoockie'


def run(command):
    output = subprocess.check_output(command, shell=True)
    return output.decode('UTF-8').strip()


run("wget -qO {} http://p.hiweb.ir --save-cookies={} --keep-session-cookies".format(TMP_FILE, COOKIE_FILE))
remain = run("cat {} | grep -Poh '<tr.*?</tr>' | grep  'مگابایت' | grep -o '[0-9]*'".format(TMP_FILE))
service_days = run("cat {} | grep -Poh '<tr.*?</tr>' | grep 'روزهای باقیمانده'| grep -o '[0-9]*'".format(TMP_FILE))
service_days = int(service_days)
run("wget -qO {} http://panel.hiweb.ir/xhr/usl.php\?user\=551402\&action\=load_usl --load-cookies={}".format(TMP_FILE, COOKIE_FILE))
credit = run("cat {} | grep -Poh '<td.*?</td>' | sed '7q;d' | grep -oh '[0-9]*\/[0-9]*\/[0-9]*'".format(TMP_FILE))
d = time.strptime('13' + credit, '%Y/%m/%d')
credit_end_date = jdatetime.date(d.tm_year, d.tm_mon, d.tm_mday).togregorian()
delta = credit_end_date - date.today()
credit_days = delta.days
days = min(credit_days, service_days)

if days >= 0 and days < 3:
    print("HiWeb: {} GB <font color='red'>in {} days</font>".format(float(remain) / 1000, days))
else:
    print("HiWeb: {} GB".format(float(remain) / 1000))

print("---")
print("In {} days (service)".format(service_days))

if credit_days >= 0:
    print("In {} days (credit)".format(credit_days))

