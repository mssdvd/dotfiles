function rg
    set rg_command rg -S -g '!.git/'
    if isatty 1
        command $rg_command --pretty $argv \
            &| less --RAW-CONTROL-CHARS --quit-if-one-screen --no-init
    else
        command $rg_command $argv
    end
end
