# Emacs setup
mkdir ~/.emacs.d/
ln -sf ~/dotfiles/editors/tty-dev.el ~/.emacs.d/init.el
sudo apt remove --autoremove emacs emacs-common
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs28-nox

# Tmux
ln -sf ~/dotfiles/.tmux.conf ~/.tmux.conf

