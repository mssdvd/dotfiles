function rg
    if isatty 1
        command rg --pretty $argv \
            &| less --RAW-CONTROL-CHARS --quit-if-one-screen --no-init
    else
        command rg $argv
    end
end
