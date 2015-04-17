ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install python
pip install virtualenv
brew install git
brew install emacs --with-cocoa --with-gnutls --with-glib
brew linkapps emacs
git clone git@github.com:minimal/dotfiles.git
cd
ln -s chrism/dotfiles/.emacs.d .
brew install caskroom/cask/brew-cask
brew-cask install amethyst
brew-cask install quicksilver
pip install flake8
