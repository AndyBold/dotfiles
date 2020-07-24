#!/usr/bin/env bash

# Use Ansible to bootstrap my Mac and keep it up to date.
# This script actually lives in a dotfiles repo and uses
# dotdrop to do the first raw set up. This script is
# concerned with getting the right tools installed.

# Bootstrapping Ansible
xcode-select --install
sudo easy_install pip
sudo pip install ansible

cd ~

# Create ~/src if it doesn't already exist
[ ! -e src ] &&   mkdir src

cd src

# Setup my personal repo/src space
[ ! -e me ] && mkdir me

# Checkout the playbook folder if it doesn't already exist
[ ! -e mac-dev-playbook ] && git clone git@github.com:AndyBold/mac-dev-playbook.git

cd mac-dev-playbook

ansible-galaxy install -r requirements.yml
