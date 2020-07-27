# I use Dotdrop to manage my dotfiles, and I use templates.
# In Bash or Zsh I use an alias like this to temporarily set environment
# variables that are going to be used in the templates:
#
# alias dotdrop='eval $(grep -v "^#" ~/dotfiles/.secrets) /usr/bin/dotdrop --cfg=~/dotfiles/config.yaml'
#
# This is not possible in Fish, so as a quick and dirty workaround, this function 
# does the above in a Zsh subshell. (Zsh because that will be the default shell in
# MacOS 11.)

function dotdrop -d 'Run dotdrop in a zsh subshell'

  # Create a var that is the command string for zsh to execute *plus* any args
  # passed to the function.
  set dd_cmd 'eval $(grep -v "^#" ~/src/dotfiles/.secrets) ~/src/dotfiles/dotdrop.sh '(echo $argv)

  zsh -c $dd_cmd

  set --erase dd_cmd

end
