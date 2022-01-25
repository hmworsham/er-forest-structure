#!/bin/bash

# Install pyenv-virtualenv as plugin
git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
echo '

# Add init to shell to auto-activate virtual envs
echo 'eval "$(pyenv virtualenv-init -)"' >> ~/.bashrc
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

if command -v pyenv 1>/dev/null 2>&1; then
	eval "$(pyenv init -)"
	eval "$(pyenv virtualenv-init -)" # Enable auto-activation of virtualenvs
fi' >> ~/.bashrc
	source ~/.bashrc
	exec "$SHELL"
