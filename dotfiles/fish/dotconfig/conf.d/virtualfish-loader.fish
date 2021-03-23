set -g VIRTUALFISH_VERSION 2.4.0
set -g VIRTUALFISH_PYTHON_EXEC {$HOME}/.pyenv/versions/3.8.2/bin/python3.8
set -g VIRTUALFISH_HOME {$HOME}/src/virtualenv
source {$HOME}/.pyenv/versions/3.8.2/lib/python3.8/site-packages/virtualfish/virtual.fish
source {$HOME}/.pyenv/versions/3.8.2/lib/python3.8/site-packages/virtualfish/compat_aliases.fish
emit virtualfish_did_setup_plugins
