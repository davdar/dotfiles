export PATH=/usr/local/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/bin:$PATH

bindkey -v

export EDITOR=vim

# alias vim='env nvim'
# alias lvim='env vim'

# opam configuration
test -r /Users/darais/.opam/opam-init/init.zsh && . /Users/darais/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# yarn configuration
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# ghcup configuration
[ -f "/Users/darais/.ghcup/env" ] && source "/Users/darais/.ghcup/env" # ghcup-env
