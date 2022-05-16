ln -sf ~/dotfiles/editors/init.vim ~/.config/nvim/init.vim
ln -sf ~/dotfiles/.zshrc ~/.zshrc

if !command -v rg &> /dev/null; then
    sudo apt-get install -y ripgrep
fi

if !command -v fzf &> /dev/null; then
    sudo apt-get install -y fzf
fi

sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

