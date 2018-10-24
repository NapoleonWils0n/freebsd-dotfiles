" execute pathogen#infect()
syntax on
filetype plugin indent on
set number
set ignorecase
set tabstop=3
set shiftwidth=3
set smartindent
set autoindent
set hlsearch

" change the mapleader from \ to ,
let mapleader=","

" Press Space to turn off highlighting and clear any message already displayed.
:nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

" mpv
"nmap ,m :if getline('.') =~ '^\s*#' \| call search('\n[^#]*\zshttp:') \| endif<cr>Y:exec '!rip-play-mpv ' .shellescape(getreg('"'), 1)<cr>

" aspell - press T in normal mode to spell check
map T :w!<CR>:!aspell --lang=en_GB --dont-backup -c %<CR>:e! %<CR>

" set clipboard
set clipboard=unnamedplus

" gundo undo tree
nnoremap <F5> :GundoToggle<CR>

" show invisible characters
nmap <leader>l :set list!<CR>

" Use the same symbols as TextMate for tabstops and EOLs
"set listchars=tab:▸\ ,eol:¬

"Invisible character colors 
highlight NonText guifg=#4a4a59
highlight SpecialKey guifg=#4a4a59

" disable code folding
set nofoldenable

"Turn backup off, since most stuff is in git
set nobackup
set nowb
set noswapfile

" write the old file when switching between files
set autowrite
"
"set spilt below
set splitbelow

"Display current cursor position in lower right corner.
set ruler

"Show command in bottom right portion of the screen
set showcmd

"Better line wrapping 
set wrap
set textwidth=79
set formatoptions=qrn1

" vim-airline
set encoding=utf-8
set guifont=Inconsolata\ for\ Powerline\ 13
let g:airline_powerline_fonts = 1
set laststatus=2
set t_Co=256

"Shortcut to fold tags with leader (usually \) + ft
nnoremap <leader>ft Vatzf

"For autocompletion
set wildmode=list:longest

"toggle nerdtree and press enter with <CR>
nmap ,x :NERDTreeToggle<CR>

"nerdtree show hidden files
let NERDTreeShowHidden=1

"make nerdtree pretty
let NERDChristmasTree=1

"nerdtree use + for directories
let NERDTreeDirArrows=0

"map code completion to , + tab
imap ,tab <C -x><C -o>

" markdown md file extension
"autocmd BufRead,BufNew *.md set filetype=markdown

"Folding colours and remove dashes
set foldmethod=marker
set fillchars="fold: " 
hi Folded cterm=bold ctermfg=DarkBlue ctermbg=none
hi FoldColumn cterm=bold ctermfg=DarkBlue ctermbg=none

" SpellBad
highlight clear SpellBad
highlight SpellBad term=standout ctermfg=1 term=underline cterm=underline

" vim pandoc syntax
hi! link Conceal Special

" vim preview with w3m
"map <Leader>, :w<cr>:!pandoc -f markdown -t html -s -S % \| lynx -stdin<cr>:redraw!<cr>
map <Leader>, :w<cr>:!pandoc -f markdown -t html -s -S % \| w3m -T 'text/html'<cr>:redraw!<cr>

" vim preview with chromium
map <Leader>m :w<cr>:!pandoc -f markdown -t html -s -S % \| chromium --log-level=3 "data:text/html;charset=utf-8;base64,`base64`"<cr>:redraw!<cr>
