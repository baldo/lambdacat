" This file helps you by writing code that matches our style guidelines.
" Feel free to imporve it.

" You can use this file by installing the localvimrc plugin:
" http://lh-vim.googlecode.com/svn/misc/trunk/plugin/local_vimrc.vim

" Lines have a maximum length of 78 characters.
set textwidth=78

if version >= 730
    " Mark the 79th column.
    set colorcolumn=+1
endif

" Treat tab as 4 whitespaces.
set softtabstop=4
set shiftwidth=4
set expandtab

" Mark tabs, trailing spaces on a line, lambdas with trailing spaces and
" various missing spaces.
highlight WarnWhitespace ctermbg=red guibg=red
match WarnWhitespace /\t\|\s\+$\|\\\s\+\|,\S\|--|\|\S\s--/

