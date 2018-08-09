export EDITOR=/usr/bin/vim

export RUBYLIB=/lib/ruby/site_ruby/1.8:/lib/ruby/site_ruby/1.8/x86_64-linux:$HOME/.ruby/
export RUBYPATH=/lib/ruby/site_ruby/1.8:/lib/ruby/site_ruby/1.8/x86_64-linux
export ANT_HOME=/opt/apache-ant
export RUBYOPT=""
if [ -e "$HOME/.zshenv.local" ] ;then
    source "$HOME/.zshenv.local"
fi
export DYNAMIC_COLORS_ROOT=$HOME/bin/term_dynamic_colors
export PERL5LIB=$HOME/.local/lib/perl:$HOME/.local/lib/perl5
export PERLLIB=$PERL5LIB
