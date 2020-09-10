#!/usr/bin/env python
import subprocess
from datetime import date, timedelta
import re

TMP_FILE = '/tmp/fund'
COOKIE_FILE = '/tmp/fundcoockie'


def run(command):
    output = subprocess.check_output(command, shell=True)
    return output.decode('UTF-8').strip()


def scrape(regno, name):
    URL = 'http://www.fipiran.com/Fund/MFwithRegNo?regno={}'.format(regno)

    run("wget -qO {} {} --save-cookies={} --keep-session-cookies".format(
        TMP_FILE, URL, COOKIE_FILE))
    table_line_no = int(
        run("cat {} | grep -n '<table' |  gawk '{{print $1}}' FS=\":\" | tail -n 1".
            format(TMP_FILE)))


    def getForCellNumber(cell_number, days):
        res=''
        try:
            res = run("tail -n+61 /tmp/fund | grep td | sed -n -e {}p | grep -oh '([0-9]*\.*[0-9]*)'".format(cell_number))
            res = float(re.sub(r'^\((.*?)\)$', r'-\1', res).replace(',',''))
            res = round(res * 365, 2)
        except (ValueError, subprocess.CalledProcessError):
            try:
                res = float(
                    run("tail -n+61 /tmp/fund | grep td | sed -n -e {}p | grep -oh '[0-9]*\.*[0-9]*'".format(cell_number)
                        ))
                res = round(res * 365 / days, 2)
            except ValueError:
                pass

        return res;


    day1 = getForCellNumber(4, 1)
    day7 = getForCellNumber(5, 7)
    day30 = getForCellNumber(6, 30)
    day90 = getForCellNumber(7, 90)
    day180 = getForCellNumber(8, 180)
    day365 = getForCellNumber(9, 365)

    print(
        "{:<25}, {:<7}, {:<5}, {:<5}, {:<5}, {:<5}, {:<5}, http://www.fipiran.com/FundDetails?regno={}".
        format(name, day1, day7, day30, day90, day180, day365,regno))



print("{:<25}, {:<7}, {:<5}, {:<5}, {:<5}, {:<5}, {:<5},".format("Name", "1D", "1W", "1M",
                                                  "3M", "6M", "1Y"))

scrape(11315, "اعتماد")
scrape(11172, "آسام")
scrape(11215, "اطلس")
scrape(11518, "فیروزا")
scrape(11459, "کیان")
scrape(11308, "فیروزه")
scrape(11341, "آگاس")
scrape(11183, "کاریس")
scrape(11195, "آساس")
# scrape(11460, "AminYekom")
# scrape(11416, "Paarand")
scrape(11327, "ثروتم")
scrape(11509, "طلا")
scrape(11409, "آکورد")
scrape(11513, "کمند")
scrape(11260, "الماس")
scrape(11378, "آتیمس")
scrape(11588, "اوصتا")
scrape(11196, "صنوین")
scrape(11569, "تصمیم")
scrape(11197, "بذر")
scrape(11649, "سرو")
scrape(11323, "صایند")
