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

  workon dotdrop

  set DOTDROP_DIR ~/src/dotfiles

  # Set vars. For some fish-related reason we have to export them to
  # make them available to dotdrop below.
  gpg --decrypt --quiet $DOTDROP_DIR/.fish-env.gpg | source

  $DOTDROP_DIR/dotdrop.sh $argv

  # Unset vars
  for var in (gpg --decrypt --quiet $DOTDROP_DIR/.fish-env.gpg | egrep -v "^\$|^#" | cut -d" " -f3)
      set -e $var
  end

end
