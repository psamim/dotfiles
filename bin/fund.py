#!/usr/bin/env python

import subprocess
from exchanges.coindesk import CoinDesk
from datetime import date, timedelta

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

    day1 = float(
        run("tail -n+61 /tmp/fund | grep td | sed -n -e 4p | grep -oh '[0-9]*\.*[0-9]*'"
            ))
    day1 = round(day1 * 365, 2)

    day7 = float(
        run("tail -n+61 /tmp/fund | grep td | sed -n -e 5p | grep -oh '[0-9]*\.*[0-9]*'"
            ))
    day7 = round(day7 * 365 / 7, 2)

    day30 = float(
        run("tail -n+{} /tmp/fund | grep td | sed -n -e 6p | grep -oh '[0-9]*\.*[0-9]*'".
            format(table_line_no)))
    day30 = round(day30 * 365 / 30, 2)

    day90 = float(
        run("tail -n+{} /tmp/fund | grep td | sed -n -e 7p | grep -oh '[0-9]*\.*[0-9]*'".
            format(table_line_no)))
    day90 = round(day90 * 365 / 90, 2)

    print(
        "{:<25}, {:<7}, {:<5}, {:<5}, {:<5}, http://www.fipiran.com/FundDetails?regno={}".
        format(name, day1, day7, day30, day90, regno))


def BTC():
    today = date.today()
    lastMonth = today - timedelta(days=30)
    lastWeek = today - timedelta(days=7)
    lastDay = today - timedelta(days=1)
    last90 = today - timedelta(days=90)
    currentPrice = CoinDesk().get_current_price()
    lastMonthPrice = CoinDesk.get_past_price(lastMonth.strftime('%Y-%m-%d'))
    lastWeekPrice = CoinDesk.get_past_price(lastWeek.strftime('%Y-%m-%d'))
    lastDayPrice = CoinDesk.get_past_price(lastDay.strftime('%Y-%m-%d'))
    last90Price = CoinDesk.get_past_price(last90.strftime('%Y-%m-%d'))
    btcDay = round((currentPrice - lastDayPrice) * 100 / lastDayPrice, 2)
    btcWeek = round((currentPrice - lastWeekPrice) * 100 / lastWeekPrice, 2)
    btcMonth = round((currentPrice - lastMonthPrice) * 100 / lastMonthPrice, 2)
    btc90 = round((currentPrice - last90Price) * 100 / last90Price, 2)
    print("{:<25}, {:<7}, {:<5}, {:<5}, {:<5}, Current BTC/USD: {}".format(
        "BTC", btcDay, btcWeek, btcMonth, btc90, currentPrice))


print("{:<25}, {:<7}, {:<5}, {:<5}, {:<5}".format("Name", "1D", "1W",
                                                  "1M", "3M"))
scrape(10911, "ArzeshAfarinan (Dey)")
scrape(10639, "Yekom (EghtesadNovin)")
scrape(11405, "Andokhteh (ToseeSaderat)")
scrape(10919, "Ganjineh (Shahr)")
scrape(10765, "Atieh (EghtesadNovin)")
print("------")
BTC()
print("------")
# scrape(10929, "Amin (Saman)")
# scrape(10883, "Gardeshgari * ")
# scrape(11500, "Negin (Saman)")
# scrape(11098, "Lotus (Parsian)")
# scrape(11075, "Miz (Mellat)")
# scrape(11419, "Zarrin (Pasargad)")
# scrape(11168, "Andisheh (Pasargad)")
# scrape(11385, "Saba (ToseeTavon)")
# scrape(10895, "Atieh (Mellat)")
# scrape(11014, "Andokhteh (Mellat)")
# scrape(11310, "Kardan (Tejarat)")
# scrape(10915, "Gostaresh (Ayandeh, +100)")
# scrape(10581, "Karafarin (No More)")
# scrape(10845, "AK (Karafarin) (No More)")
print("------")
scrape(11308, "Firouzeh")
scrape(11341, "Agas")
scrape(11183, "Karis")
scrape(11195, "Asas")
scrape(11315, "Etemad")
scrape(11460, "AminYekom")
scrape(11459, "Kian")
scrape(11409, "Akord")
scrape(11513, "Kamand")
scrape(11416, "Parand")
scrape(11215, "Atlas")
