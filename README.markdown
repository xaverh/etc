# change $ZDOTDIR to use different folder for .zshrc

```sh
echo 'export ZDOTDIR="$HOME"/.config/zsh' | sudo tee -a /etc/zshenv
```

# deactivate OpenSSH password authentication

In `/etc/ssh/sshd_config` set to `PasswordAuthentication no`.

# Firefox

```
browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned: amazon
browser.newtabpage.pinned: [null,{"url":"https://smile.amazon.de","label":"@amazon","searchTopSite":true}]
extensions.pocket.enabled: false
```