#!/usr/bin/env python

from subprocess import Popen, PIPE
import sys
import os

tomato = 'https://emojipedia-us.s3.amazonaws.com/cache/bc/62/bc6271e8d5db5a36ff98d95b390a50b2.png'
clock_off = 'https://emojipedia-us.s3.amazonaws.com/cache/de/50/de50e49fa32a5ddd0f1e104f8311a1d7.png'
clock_on = 'https://emojipedia-us.s3.amazonaws.com/cache/67/59/6759cc24da158731982ec16fc0790885.png'
stopwatch = 'https://emojipedia-us.s3.amazonaws.com/cache/50/5f/505f468020b1b24068617dba4d2cb267.png'


def run_elisp(elisp):
    p = Popen(args=["/usr/bin/emacsclient", "--eval", elisp], stdout=PIPE)
    (output, err) = p.communicate()
    p.wait()
    return output.decode('UTF-8').strip().replace('nil', '').replace('"', '')


def is_active():
    return run_elisp("(if (org-clock-is-active) 1 0)") == '1'


def effort():
    s = run_elisp("""
    (if (and
     (org-clock-is-active) org-clock-effort)
    (org-minutes-to-clocksum-string
     (org-duration-string-to-minutes
      org-clock-effort)))
    """)
    if s is not '':
        return '/' + s

    return ''


def clock_time():
    return run_elisp("""
    (if
      (org-clock-is-active)
        (org-minutes-to-clocksum-string
          (org-clock-get-clocked-time)))
    """)


def go_to_clock():
    """
    Go to the currently clocked-in entry, or to the most recently clocked one.
    With prefix arg =select=, offer recently clocked tasks for selection.
    """
    return run_elisp("(org-clock-goto)")


def clock_out():
    """
    Stop the currently running clock.
    If there is no running clock, throw an error, unless =fail-quietly= is set.
    """
    return run_elisp("(org-clock-out)")


def kill_pomodoro():
    return run_elisp("(org-pomodoro-kill)")


def heading():
    return run_elisp("""
    (if (org-clock-is-active) org-clock-heading)
    """)


def is_pomodoro_active():
    return run_elisp("(if (org-pomodoro-active-p) 1 0)") == '1'


def current_pomodoro():
    s = run_elisp("""
    (if (org-pomodoro-active-p) (org-pomodoro-format-seconds))
    """)
    if s is not '':
        return "(<img src='{}' width='18' height='18'/> {})".format(tomato, s)

    return ''


def icon(color):
    icon = "<img height='18'  width='18' src='{}' />".format(stopwatch)
    return icon


def process_arg(arg):
    if arg == 'cancel':
        clock_out()
    elif arg == 'goto':
        go_to_clock()


if __name__ == '__main__':
    if len(sys.argv) > 1:
        process_arg(sys.argv[1])
    else:
        this_file = os.path.realpath(__file__)
        if is_active():
            print("{icon} {clock}{effort} {pomodoro}<font color='grey'> @ {heading}</font>\
            | bash='{this_file} goto' onclick=bash size=10"
                  .format(icon=icon('#ffc107'),
                          pomodoro=current_pomodoro(),
                          clock=clock_time(),
                          effort=effort(),
                          heading=heading(),
                          this_file=this_file))
            print("---")
            print("  Clock out | bash='{} cancel' onclick=bash".format(this_file))
        else:
            print("{}| bash='{} goto' onclick=bash"
                  .format(icon('#f3f3f3'), this_file))
