function e
    if [ -z $argv ]
        set arg .
    else
        set arg $argv
    end
    emacsclient -t -a '' -- $arg
end
