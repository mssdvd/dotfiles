function cdr --description "cd to git root directory"
    set dir $(git rev-parse --show-toplevel); and cd $dir
end
