#!/usr/bin/env python

import subprocess


def run(command):
    output = subprocess.check_output(command, shell=True)
    return output.decode('UTF-8').strip()


content = run("wget -qO /tmp/hiweb http://p.hiweb.ir")
remain = run("cat /tmp/hiweb | grep -Poh '<tr.*?</tr>' | grep  'مگابایت' | grep -o '[0-9]*'")
days = run("cat /tmp/hiweb | grep -Poh '<tr.*?</tr>' | grep 'روزهای باقیمانده'| grep -o '[0-9]*'")
print("HiWeb: {} GB".format(float(remain) / 1000))
print("---")
print("In {} days (service)".format(days))
