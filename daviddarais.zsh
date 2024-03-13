eval "$(/opt/homebrew/bin/brew shellenv)"

export PATH=/usr/local/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/bin:$PATH

export PATH=$HOME/Library/Python/3.9/bin:$PATH

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

bindkey -v

export EDITOR=vim

# alias vim='env nvim'
# alias lvim='env vim'

# opam configuration
[[ ! -r /Users/daviddarais/.opam/opam-init/init.zsh ]] || source /Users/daviddarais/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# yarn configuration
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# ghcup configuration
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
