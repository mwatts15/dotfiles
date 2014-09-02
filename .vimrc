set nocompatible
" {{{ Vundle
     filetype off                   " required!

     set rtp+=~/.vim/bundle/vundle/
     call vundle#rc()

     " let Vundle manage Vundle
     " required! 
     Bundle 'gmarik/vundle'

     " My Bundles here:
     "
     " original repos on github
     Bundle 'tpope/vim-fugitive'
     Bundle 'scrooloose/syntastic'
     Bundle 'scrooloose/nerdcommenter'
     Bundle 'scrooloose/nerdtree'

     Bundle 'Lokaltog/vim-easymotion'
     Bundle 'Lokaltog/vim-powerline'
     
     Bundle 'altercation/vim-colors-solarized'
     Bundle 'spf13/vim-colors'
     Bundle 'flazz/vim-colorschemes'

     Bundle 'tpope/vim-surround'
     "Bundle 'Valloric/YouCompleteMe'
     Bundle 'plasticboy/vim-markdown'
     Bundle 'MultipleSearch'
     Bundle 'msanders/snipmate.vim'

     filetype plugin indent on     " required!
     "
     " Brief help
     " :BundleList          - list configured bundles
     " :BundleInstall(!)    - install(update) bundles
     " :BundleSearch(!) foo - search(or refresh cache first) for foo
     " :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
     "
     " see :h vundle for more details or wiki for FAQ
     " NOTE: comments after Bundle command are not allowed..
" }}}
"
if &term == 'xterm' || &term == 'screen-bce'
    set t_Co=256                 " Enable 256 colors to stop the CSApprox warning and make xterm vim shine
endif
scriptencoding utf-8
set tabstop=4
set nowrap
set shiftwidth=4
set textwidth=0
set cinkeys-=0#
set indentkeys-=0#
set tags=./TAGS,tags;/
set noequalalways

set formatoptions+=croql
set shortmess+=filmnrxoOtT      " abbrev. of messages (avoids 'hit enter')
set cinoptions+=t0,j1,L0
set nu
set ssop-=buffers
set scrolloff=3
set winminheight=0
set autoindent
set expandtab
set incsearch
set ignorecase
set smartcase
set nohlsearch
set ignorecase
set smartcase
set title
set mouse=a
set backspace=indent,eol,start
set secure
set wildignore=*.o,*.class,*.pyc
syntax on
"set background=dark
set foldmethod=marker
set grepprg=grep\ -nH\ $*\ /dev/null

let g:solarized_termtrans=0
let g:solarized_contrast="normal"
let g:solarized_visibility="normal"
let g:solarized_termcolors=128
let g:tex_flavor='latex'
colorscheme solarized

let g:EasyMotion_leader_key = ','
let g:EasyMotion_keys = '123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQ'

let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_filetype_specific_completion_to_disable = ["lisp", "racket", "scheme"]
let g:syntastic_c_check_header = 1
let g:syntastic_c_config_file = '.syntastic_c_config'

let NERDTreeIgnore=['\.o$', '\.sw[a-z]$', '\.hi$']

set linebreak
set display+=lastline
"{{{Key mappings
imap <up> <C-O>gk
imap <down> <C-O>gj
nmap <up> gk
nmap <down> gj
"XXX: to get this to work correctly, need to seek back for a surrounding char
"nmap di% %%d%
nnoremap j gj
nnoremap k gk
nnoremap Y y$
"call togglebg#map("<F5>")
vmap <up> gk
vmap <down> gj
map Q <Nop>
map <F1> <Nop>
imap <F1> <Nop>
inoremap vv <Esc>`^
inoremap <c-u> <c-g>u<c-u>
nmap . .`[
vnoremap <silent> . :normal .<CR>
inoremap { {}<Left>
inoremap {<Right> {
inoremap {<CR>  {<CR>}<Esc>O
inoremap {} {}
nmap zO zR
"map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
inoremap <expr> }  strpart(getline('.'), col('.')-1, 1) == "}" ? "\<Right>" : "}"
inoremap <expr> {  strpart(getline('.'), col('.')-1, 1) == "}" ? "\<Left>" :"{"

nmap <C-J> <C-W>j<C-W>_
nmap <C-K> <C-W>k<C-W>_
nmap <C-L> <C-W>l<C-W>\|
nmap <C-H> <C-W>h<C-W>\|

cmap Sh sh
set notimeout
set ttimeout
set ttimeoutlen=500

" Fix home and end keybindings for screen, particularly on mac
" - for some reason this fixes the arrow keys too. huh.
map [F $
imap [F $
map [H g0
imap [H g0
"}}}
set showmode                    " display the current mode

set cursorline                  " highlight current line

if has('cmdline_info')
    set ruler                   " show the ruler
    set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " a ruler on steroids
    set showcmd                 " show partial commands in status line and
    " selected characters/lines in visual mode
endif

if has("cscope")
    set csto=0
    set cst
    set nocsverb
    " add any database in current directory
    if filereadable("cscope.out")
        cs add "cscope.out"
        " else add database pointed to by environment
    elseif $CSCOPE_DB != ""
        cs add $CSCOPE_DB
    endif
    set csverb
    map g<C-]> :cs find 3 <C-R>=expand("<cword>")<CR><CR>
    map g<C-\> :cs find 0 <C-R>=expand("<cword>")<CR><CR>
endif
let mapleader = ','

if has('statusline')
    set laststatus=2

    " Broken down into easily includeable segments
    set statusline=%<%f\    " Filename
    set statusline+=%w%h%m%r " Options
    set statusline+=%{fugitive#statusline()} "  Git Hotness
    set statusline+=\ [%{&ff}/%Y]            " filetype
    set statusline+=\ [%{getcwd()}]          " current dir
    set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
endif

"functions
function! SuperCleverTab()
    " col('.') is the location of the character right *after* the cursor
    " position.
    " col('.') - 2 is the position of the character before the one you're
    " on
    let mystr = strpart(getline('.'), col('.') - 2 , 1)
    if (mystr =~ '\s' || mystr == "")
         return "\<Tab>"
    else
        return "\<C-N>"
    endif
endfunction

set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)

" Remove trailing whitespaces and ^M chars
autocmd FileType c,cpp,java,php,javascript,python,twig,xml,yml autocmd BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))
autocmd! BufWritePost *.rst silent ! rst2html <afile> 2>/dev/null > rst_out.html

inoremap <Tab> <C-R>=SuperCleverTab()<cr>
inoremap <S-Tab> <C-P>

nnoremap <silent> <buffer> <Plug>SexpCloseParenthesis  :call SlimvCloseForm()<CR>
"-------------------------------------------------------------------
" Close open parenthesis
" Taken from the Slimv plugin by Tamas Kovacs. Released in the
" public domain by the original author.
"-------------------------------------------------------------------

" Count the opening and closing parens or brackets to determine if they match
function! s:GetParenCount( lines )
    let paren = 0
    let inside_string = 0
    let i = 0
    while i < len( a:lines )
        let inside_comment = 0
        let j = 0
        while j < len( a:lines[i] )
            if inside_string
                " We are inside a string, skip parens, wait for closing '"'
                if a:lines[i][j] == '"'
                    let inside_string = 0
                endif
            elseif inside_comment
                " We are inside a comment, skip parens, wait for end of line
            else
                " We are outside of strings and comments, now we shall count parens
                if a:lines[i][j] == '"'
                    let inside_string = 1
                endif
                if a:lines[i][j] == ';'
                    let inside_comment = 1
                endif
                if a:lines[i][j] == '(' || a:lines[i][j] == '['
                    let paren = paren + 1
                endif
                if a:lines[i][j] == ')' || a:lines[i][j] == ']'
                    let paren = paren - 1
                    if paren < 0
                        " Oops, too many closing parens in the middle
                        return paren
                    endif
                endif
            endif
            let j = j + 1
        endwhile
        let i = i + 1
    endwhile
    return paren
endfunction

function! LoadTemplate()
  let template_name = expand("%:t")
  let template_extension = expand("%:e")
  let templates_directory = $HOME . "/.vim/templates"
  let whole_name_template = templates_directory . "/" . template_name
  let extension_template = templates_directory . "/" . template_extension
  if filereadable(whole_name_template)
      let original_template = whole_name_template
  elseif filereadable(extension_template)
      let original_template = extension_template
  else
      return
  end

  " Note: Use syscmd() macro to run arbitrary
  " system commands for things like time of day, platform, etc.
  let defs = {}
  let defs['FILE_BASENAME'] = expand("%:t:r")
  let defs['USER'] = $USER
  let m4_args = ""
  for [k,v] in items(defs)
      let m4_args = m4_args . " -D" . k . "=" . v
  endfor

  let tmp = tempname()
  let _ = system("cat " . original_template . "| m4 " . m4_args . " >" . tmp)
  silent! execute "0read " tmp
  " Highlight %VAR% placeholders with the Todo colour group
  syn match Todo "%\u\+%" containedIn=ALL
  normal /START_HERE/
  normal dw
endfunction

let s:bgfile = $HOME . "/.vimbg"

function! MySetBG(v)
    if a:v == 0
        set bg=dark
    else
        set bg=light
    endif
    call system("echo " . a:v . " > " . s:bgfile)
    return a:v
endfunction

function! MyReadBG()
    return system("cat " . s:bgfile)
endfunction

call MySetBG(MyReadBG())

function! MyToggleBG()
    let v = MyReadBG()
    echo v
    echo 'MyToggleBG ' . v 
    if v == 0
        call MySetBG(1)
    else
        call MySetBG(0)
    endif
endfunction

"inoremap <F5> <C-O>:call MyToggleBG()<CR>
"nmap <F5> :call MyToggleBG()<CR>

" Close current top level form by adding the missing parens
function! SlimvCloseForm()
    let l2 = line( '.' )
    normal 99[(
    let l1 = line( '.' )
    let form = []
    let l = l1
    while l <= l2
        call add( form, getline( l ) )
        let l = l + 1
    endwhile
    let paren = s:GetParenCount( form )
    if paren > 0
        " Add missing parens
        let lastline = getline( l2 )
        while paren > 0
            let lastline = lastline . ')'
            let paren = paren - 1
        endwhile
        call setline( l2, lastline )
    endif
    normal %
endfunction

nmap <buffer> <LocalLeader>cp      <Plug>SexpCloseParenthesis
imap <buffer> <C-X>0               <C-O><LocalLeader>cp


autocmd! BufNewFile * call LoadTemplate()
"Jump between %VAR% placeholders in Normal mode with
" <Ctrl-p>
nnoremap <c-p> /%\u.\{-1,}%<cr>c/%/e<cr>
"Jump between %VAR% placeholders in Insert mode with
" <Ctrl-p>
inoremap <c-p> <ESC>/%\u.\{-1,}%<cr>c/%/e<cr>
if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif
