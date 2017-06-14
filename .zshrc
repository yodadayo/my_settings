# -- essential
#source "$DOTPATH/scripts/vital.sh" || return
autoload -Uz is-at-least
autoload -Uz add-zsh-hook
autoload -Uz colors && colors
zmodload zsh/complist

# -- TERM
if [[ "$TERM" = xterm ]]; then
  source "$DOTPATH/scripts/xterm-color-count.sh"
  [[ "$XTERM_COLOR_COUNT" = 256 ]] && TERM=xterm-256color
fi

echo 'now loading...'

# -- rc
for s in "$HOME/.zsh/rc"/*.zsh(N); do source "$s"; done
for s in "$HOME/local/etc/zsh/rc"/*.zsh(N); do source "$s"; done

# -- zplug load
if [ -f "$HOME/.zplug/init.zsh" ]; then
  export ZPLUG_LOADFILE="$HOME/.zsh/zplug.zsh"
  source "$HOME/.zplug/init.zsh"

  if ! zplug check --verbose; then
    printf 'Install? [Y/n]'
    read; [[ "$REPLY" =~ ^([yY]|)$ ]] && zplug install
  fi

  zplug load
fi

# -- after/rc
for s in "$HOME/.zsh/after/rc"/*.zsh(N); do source "$s"; done
for s in "$HOME/local/etc/zsh/after/rc"/*.zsh(N); do source "$s"; done

# -- logo
function () {
  local i j s
  s='!Z!'
  [ $(tput cols) -ge 45 ] &>/dev/null && s='!ZSH!'
  [ $(tput cols) -ge 64 ] &>/dev/null && s='!ZSHRC!'
  for i in {0..1}; do
    printf "\e[0;${i}m$s"
    for j in {0..7}; do
      printf "\e[${i};3${j}m$s\e[0m"
    done
    echo
  done
}

if has zprof; then
  zprof | less
fi
