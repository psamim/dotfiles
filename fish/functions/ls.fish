function ls -d "personal ls"
    # color
    set -l param '--color=auto'
    if isatty 1
        set param $param '--indicator-style=classify'
    end

    # ignore these files
    set ignore_files \
        '*.pyc' \
        '__pycache__'
    for p in $ignore_files
        set param $param "--hide=$p"
    end

    # single column
    set param $param '-1'

    command ls $param $argv
end
