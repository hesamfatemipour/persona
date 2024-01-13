# Theme
ZSH_THEME="fishy"

# Plugins
plugins=(git zsh-autosuggestions)

# Paths
export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH="$HOME/.oh-my-zsh"

eval $(/opt/homebrew/bin/brew shellenv)

source $ZSH/oh-my-zsh.sh
export PATH="/opt/homebrew/opt/php@7.4/bin:$PATH"
export PATH="/opt/homebrew/opt/php@7.4/sbin:$PATH"
export PATH="/opt/homebrew/opt/php@7.4/bin:$PATH"
export PATH="/opt/homebrew/opt/php@7.4/sbin:$PATH"


[ -f "/Users/hesam/.ghcup/env" ] && source "/Users/hesam/.ghcup/env" # ghcup-env

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
