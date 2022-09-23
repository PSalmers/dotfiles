sudo apt-get install -y ripgrep
sudo apt-get install -y zoxide
sudo apt-get install -y fzf
sudo apt-get install -y universal-ctags
sudo apt-get install -y fd-find

sudo add-apt-repository ppa:kelleyk/emacs
sudo apt install emacs28
mkdir ~/.emacs.d
cp ~/dotfiles/editors/org-custom-init.el ~/.emacs.d/init.el

sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
 
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# I link after in case installs create any configs I wish to overwrite
ln -sf ~/dotfiles/.zshrc ~/.zshrc
mkdir ~/.config/nvim/
 
ln -sf ~/dotfiles/editors/init.vim ~/.config/nvim/init.vim
nvim +PlugInstall +qall
 
# Dirty checking slows down the prompt on large repos, so I disable it
git config --global --add oh-my-zsh.hide-dirty 1
git config --global --add oh-my-zsh.hide-status 1
