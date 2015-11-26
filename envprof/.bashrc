#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#PS1
RET='$(Ret=$? ; [ ${Ret} -eq 0 ] && echo -n "\[\e[32;1m\]▪" || echo -n "\[\e[31;1m\]▪")'
#RET='$(Ret=$? ; [ ${Ret} -eq 0 ] && echo -n "\[\e[32;1m\]▪" || echo -n "\[\e[31;1m\]${Ret}")'

# PS1
red="\[\e[0;32m\]"
yellow="\[\e[0;34m\]"
if [ `id -u` -eq "0" ]; then
    root="\[\e[0;31m\]"
else
    root="\[\e[0;32m\]"
fi
PS1="\[\e[0;37m\]┌─[${root}\u\[\e[0;37m\]][\[\e[0;96m\]\h\[\e[0;37m\]][\[\e[0;32m\]\w\[\e[0;37m\]]\n\[\e[0;37m\]└──╼ \[\e[0m\]"
PS2="\[\e[0;37m\]• \[\e[0m\]"


# test -n "$SSH_CLIENT" && PS1="$(hostname|cut -b 1-3)" || PS1=
# PS1="${PS1}% "

# Bash tweaks for command history
shopt -s histappend
PROMPT_COMMAND='history -a'

# Use up and down for partial completion of yet typed command
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\C-\e[A": previous-history'
bind '"\C-\e[B": next-history'

export TEXMFLOCAL=$HOME/.local/share/texmf
export LC_ALL=C
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
source ~/.bash_aliases

export SUDO_EDITOR="em"
