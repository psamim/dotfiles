set -e
# mbsync --all
cd ~/.mail/account.gmail && gmi sync
notmuch new

notmuch tag --batch <<EOF

    # Tag urgent mail
    +global +work tag:new and to:samim@globalworkandtravel.com

    # We've finished processing incoming mail
    # -new tag:new
EOF
notifymuch
