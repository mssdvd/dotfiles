function et
    if count $argv >/dev/null
        set arg $argv
    else
        set arg .
    end
    emacsclient -t -a '' -- $arg
end
