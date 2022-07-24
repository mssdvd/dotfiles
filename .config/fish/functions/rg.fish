function rg
    if isatty 1
        command rg -S --pretty $argv \
            &| less --RAW-CONTROL-CHARS --quit-if-one-screen --no-init
    else
        command rg -S $argv
    end
end
