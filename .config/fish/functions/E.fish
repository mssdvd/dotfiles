function E
    if [ -z $argv ]
        set arg .
    else
        set arg $argv
    end
    emacsclient -c -a '' -- $arg &
    disown
end
