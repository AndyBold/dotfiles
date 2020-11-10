function dl -d "Download a file from the specified URL"
    # Parse the input. We want the filename from the end.
    set filename (basename $argv[1])
    curl -L --progress-bar -o ~/Downloads/$filename $argv[1]
end
