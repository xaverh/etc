# change $ZDOTDIR to use different folder for .zshrc

echo 'export ZDOTDIR="$HOME"/.config/zsh' | sudo tee -a /etc/zshenv

# deactivate OpenSSH password authentication

In `/etc/ssh/sshd_config` set to `PasswordAuthentication no`.