# Emacs setup
mkdir ~/.emacs.d/
ln -sf ~/dotfiles/editors/tty-dev.el ~/.emacs.d/init.el
sudo apt-get -y remove --autoremove emacs emacs-common
sudo add-apt-repository -y ppa:kelleyk/emacs
sudo apt-get -y update
sudo apt-get -y install emacs28-nox

nvm install -y node
npm install -g typescript typescript-language-server typescript-eslint-language-service prettier
gem install --user-install solargraph
# Tmux
ln -sf ~/dotfiles/.tmux.conf ~/.tmux.conf

ln -sf ~/dotfiles/.zshrc ~/spin_zshrc

