# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
    export PATH
fi

if [ -d "$HOME/work/racket-5.3.6/bin" ] ; then
    PATH="$HOME/work/racket-5.3.6/bin:$PATH"
    export PATH
fi
PATH=$PATH:$HOME/.local/bin

if [ -d "$HOME/.gem/ruby/" ] ; then
    for x in "$HOME/.gem/ruby/"* ; do
        if [ -d "$x/bin" ] ; then
            PATH="$x/bin:$PATH"
        fi
    done
fi

if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

PATH="$HOME/.cabal/bin:$HOME/.local/bin:$PATH"
export DXMMS2_FONT_WIDTH=10
export DMENU_FONT="Noto Sans Mono CJK JP Regular"
export PYTHONPATH=$HOME/.python
export DYNAMIC_COLORS_ROOT=$HOME/bin/term-dynamic-colors
export PERL5LIB=$HOME/.local/lib/perl:$HOME/.local/lib/perl5
export PERLLIB=$PERL5LIB
export PATH
export DEBEMAIL=watts.mark2015@gmail.com
export DEBFULLNAME="Mark Watts"
export CDRIP_RHOST=starstation.home
export NO_AT_BRIDGE=1
