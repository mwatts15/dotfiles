# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' original true
zstyle :compinstall filename '/home/twock/.zshrc'

# VCS Info plugin configuration#
zstyle ':vcs_info:*' enable git hg svn
zstyle ':vcs_info:*' actionformats \
    '%F{blue}%s%f:%F{2}%b|%a%f%f'
zstyle ':vcs_info:*' formats       \
    '%F{blue}%s%f:%F{2}%b%f%f'


autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=4096
SAVEHIST=4096
setopt autocd extendedglob notify nohup
setopt appendhistory
setopt auto_continue
setopt hist_ignore_all_dups

bindkey -v
# End of lines configured by zsh-newuser-install

# autoload -U promptinit
autoload zkbd
autoload -Uz vcs_info
autoload promptinit
autoload compinit
autoload -Uz history-beginning-search-menu
zle -N history-beginning-search-menu
autoload -Uz add-zsh-hook
setopt auto_resume
setopt AUTO_PUSHD
setopt prompt_subst

# For projects, see below
MPROJ_RUN_DIR=$HOME/.mproj.run
MPROJ_BACKUPS_DIR=$HOME/.mproj_bkp
MPROJ_TARGETS_FILE=$HOME/.mproj_targets
MPROJ_NBACKUPS=4

promptinit
compinit

precmd()
{
    vcs_info
    PR_FILLBAR=""
    PR_PWDLEN=""
    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))

    prompt_wd="$PWD"
    prompt_start='('
    prompt_sep=')('
    prompt_end=')'
    if [ ${vcs_info_msg_0_} ] ; then
        local UNSTAGED_CHANGES
        local STAGED_CHANGES
        local CHANGES
        local ua
        local ud
        local sa
        local sd

        UNSTAGED_CHANGES=
        STAGED_CHANGES=
        ua=
        ud=
        git diff --numstat | while read a d _; do
            ua=$((a + ua))
            ud=$((d + ud))
            UNSTAGED_CHANGES=1
        done

        sa=
        sd=
        git diff --numstat --staged | while read a d _ ; do
            sa=$((a + sa))
            sd=$((d + sd))
            STAGED_CHANGES=1
        done
        [ x$sa = x0 ] && sa=
        [ x$sd = x0 ] && sd=
        [ x$ua = x0 ] && ua=
        [ x$ud = x0 ] && ud=
        ua=${ua:+"%F{green}+$ua%f"}
        ud=${ud:+"%F{red}-$ud%f"}
        sa=${sa:+"%F{green}+$sa%f"}
        sd=${sd:+"%F{red}-$sd%f"}
        SC=${STAGED_CHANGES:+$sa$sd}
        UC=${UNSTAGED_CHANGES:+$ua$ud}
        if [ x$SC != x -a x$UC != x ] ; then
            SC="$SC;"
        fi

        if [ $UC$SC ] ; then 
            CHANGES="|$SC$UC"
        else
            CHANGES=
        fi
        prompt_vcs="${prompt_start}${vcs_info_msg_0_}$CHANGES${prompt_end}"
    else
        prompt_vcs="" 
    fi

    if [ $VIRTUAL_ENV ] ; then
        pyvenv=${prompt_start}"%F{blue}pyenv%f:%F{green}"$(basename $VIRTUAL_ENV)"%f"${prompt_end}
    else
        pyvenv=
    fi
    prompt='${pyvenv}${prompt_vcs}${prompt_start}%F{yellow}%n@%U%M%u%f$prompt_sep%F{cyan}zsh:%L%f$prompt_sep$prompt_wd$prompt_end
$prompt_start%B%F{red}%!%f%b$prompt_end%# '
    local prompt_no_color="$(echo ${(%%)prompt}|head -n1|sed 's/.\[[0-9]\+m//g')"
    local promptsize="${#prompt_no_color}"
    local pwdsize=${#PWD}
    ((promptsize = promptsize - pwdsize))
    ((PR_PWDLEN = $TERMWIDTH - $promptsize))
    jobs
    case $TERM in
        xterm*|*rxvt*)
            if [[ $TERM =~ "xterm.*" ]] ; then
                escape_num=0
            elif [[ $TERM =~ ".*rxvt.*" ]] ; then
                escape_num=2
            fi
            print -nP "\e]$escape_num;%y: %1~: --\a"
            ;;
    esac
    prompt_wd="%F{magenta}$(shorten_path.pl $PWD $PR_PWDLEN)%f"
    pwd > $HOME/.last-directory
    . $HOME/.project_aliases
}

preexec()
{
    case $TERM in
        xterm*|*rxvt*)
            if [[ $TERM =~ "xterm.*" ]] ; then
                escape_num=0
            elif [[ $TERM =~ ".*rxvt.*" ]] ; then
                escape_num=2
            fi
            print -nP "\e]$escape_num;%y: %c: $(echo $1 | grep -o '^[a-zA-Z0-9._]\+')\a"
            ;;
    esac
}

gocs ()
{
    if [ "${1}x" = "atolx" ] ; then
        ssh rick@atol.csres.utexas.edu
    else
        ssh markw@$1.cs.utexas.edu
    fi
}

#PS1="\${vcs_info_msg_0_}(%F{yellow}%n@%U%M%u%f)(%F{cyan}zsh:%L%f)(\$prompt_wd)
#(%B%!%b)%# "
PS1="\$prompt"
RPROMPT="%(?.%T.%F{white}%K{black}%?%f%k)"

[[ ! -f ${ZDOTDIR:-$HOME}/.zkbd/linux-unknown-linux-gnu ]] && zkbd
source ${ZDOTDIR:-$HOME}/.zkbd/linux-unknown-linux-gnu

[[ -n ${key[Backspace]} ]] && bindkey "${key[Backspace]}" backward-delete-char
[[ -n ${key[Insert]} ]] && bindkey "${key[Insert]}" overwrite-mode
[[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n ${key[PageUp]} ]] && bindkey "${key[PageUp]}" up-line-or-history
[[ -n ${key[Delete]} ]] && bindkey "${key[Delete]}" delete-char
[[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
[[ -n ${key[PageDown]} ]] && bindkey "${key[PageDown]}" down-line-or-history
[[ -n ${key[Up]} ]] && bindkey "${key[Up]}" up-line-or-search
[[ -n ${key[Left]} ]] && bindkey "${key[Left]}" backward-char
[[ -n ${key[Down]} ]] && bindkey "${key[Down]}" down-line-or-search
[[ -n ${key[Right]} ]] && bindkey "${key[Right]}" forward-char
bindkey "vv" vi-cmd-mode
bindkey '^R' history-beginning-search-backward
bindkey '^X^X' history-beginning-search-menu

# {{{ Aliases

eval `/usr/bin/dircolors -b ~/.dircolors`
alias bc='bc -il ~/.bc/*.bc'
alias ren='mv -n'
alias help=man
alias cl='clisp -repl ~/.cl/*.cl'
alias l='ls'
alias ll='ls -l'
alias ls='/bin/ls --group-directories-first --color=auto -F'
alias rm="rm -I"
alias go='cd'
alias back='popd'
alias xclip="xclip -sel clipboard"
alias wicd-client="wicd-client -n"
alias setcur='set_current_project'
alias defp='define_project'
alias lstp='list_projects'
alias rmp='delete_project'
alias xlock='xscreensaver-command --lock'
alias grepl='grep --color=auto -n'
alias e=$EDITOR
alias kilall=killall
alias ↑=up
alias cd-='cd -'
alias py=python
alias ipy=ipython
alias py3=python3
alias ipy3=ipython3
alias xsetkbmap=setxkbmap
alias gah=cat

alias ge='ssh markw@envy.cs.utexas.edu'
alias v=vim
which finger >/dev/null 2>&1
if [ $? -ne 0 ] ; then
    which pinky >/dev/null 2>&1
    if [ $? -ne 0 ] ; then
        alias finger=pinky
    fi
fi

# }}}

#{{{ Wrapper functions

# Creates the directory in every case
#  so annoying to have the files blow up everywhere
#  TODO: make this work for gzip'd and bzip'd files
function unzip ()
{
    local real_unzip="/usr/bin/unzip"
    if [ ${#@} -eq 1 -a -f $1 -a -z "${1:#*.zip}" ] ; then
        exdir=`dirname "$1"`/`basename "$1" .zip`
        echo unzipping to $exdir
        $real_unzip $1 -d $exdir 
    else
        # Hopefully I can find a way to use
        # "system" unzip without explictly saying it
        # but this is fine for now
        $real_unzip "$@"
    fi
}

function bc ()
{
    local bcfiles
    bcdir="$HOME/.bc/"
    if [ -d $bcdir ] ; then
        /usr/bin/bc -q -il $bcdir/*.bc
    fi
}

function mkcd ()
{
    mkdir $1; cd $1;
}

function up ()
{
    local levels=1 
    if [ $1 ] ; then
        if [[ $1 =~ ^[0-9]+$ ]] ; then
            levels=$1
            spaces=`printf "%*s" levels`
            dots=${spaces// /../}
            cd $dots
        elif [[ $PWD =~ $1 ]] ; then
            realDir=`echo $PWD | /bin/grep -o "[^/]*$1[^/]*" | tail -n 1`
            cd ${PWD%$realDir*}$realDir
        fi
    else
        cd ..
    fi
}

function cdcd ()
{
    if [ $2 ] ; then
        cd "$1"; cd "$2" || cd - > /dev/null
    else
        cd "$1"
    fi
}

function sqlitedump ()
{
    sqlite3 $1 .dump
}

#function ln ()
#{
    #if [ ${#@} -eq 1 ] ; then
        #local newarr=()
        #for a in "${@}" ; do
            #if [ -e "$a" ] ; then
                #a=`readlink -f $a`
            #fi
            #newarr=(${newarr[@]} $a)
        #done
        #echo -n "${newarr[@]}" .
        #/bin/ln "${newarr[@]}" .
    #else
        #/bin/ln "${@}"
    #fi
#}

#}}}

#{{{ Other functions
function str_to_lines ()
{
    sed -r 's/[[:space:]]+/\n/g'
}

function ctd ()
{
    cd "$(td $1)"
}
#}}}

export CVS_RSH=ssh
export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib
CDPATH=.:$HOME