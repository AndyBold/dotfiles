# Disable virtualenv prompt to avoid clashes with the theme
set -x VIRTUAL_ENV_DISABLE_PROMPT 1

####
# Theme settings for bobthefish
####
set -g theme_nerd_fonts yes
set -g theme_title_display_process yes
set -g theme_display_vagrant yes

set theme_color_scheme solarized-dark


####
# Start SSH agent
#### 
if test -z "$SSH_ENV"
    set -xg SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
end

# Use autoenv for .env parsing, if it is installed
# Currently disabled as it's more trouble that it's worth
# [ -e /usr/local/opt/autoenv_fish/activate.fish ] && source /usr/local/opt/autoenv_fish/activate.fish

# Use autovenv for auto Python venv enable/disable
set -U autovenv_enable yes
set -U autovenv_announce yes

# Configure enhancd
set -U ENHANCD_FILTER fzf
