function back -d "create backup"
    for file in $argv
        cp -R $file $file.back
    end
end
