if test -f /usr/lib/ssh/x11-ssh-askpass
then
	SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass ssh-add < /dev/null
fi

if test -f /usr/libexec/openssh/x11-ssh-askpass
then
 	SSH_ASKPASS=/usr/libexec/openssh/x11-ssh-askpass ssh-add < /dev/null
fi
