function isrunning -d "check if process name is running and print processs id"
    ps -ef | grep $argv | grep -v grep | tr -s ' ' | cut -d ' ' -f 2,8
end
