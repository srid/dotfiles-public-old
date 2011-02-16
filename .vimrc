" This setting prevents vim from emulating the original vi's bugs and limitations.
set nocompatible

set showmatch

" Get rid of the toolbar
set guioptions-=T

" Flash instead of beeping in command errors
set vb t_vb=

set ruler
set incsearch

" Highlight overlapping text
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.*/

" Will allow you to use :w!! to write to a file using sudo if you forgot to
" 'sudo vim file' (it will prompt for sudo password when writing)
cmap w!! %!sudo tee > /dev/null %

syn on
colorscheme wombat

filetype plugin indent on
filetype on
