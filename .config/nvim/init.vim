" Changes to defaults
let mapleader=" "
set mouse=a " the mouse can be used
set expandtab " the tab keys inserts spaces
set shiftwidth=2
set tabstop=2
set fillchars=vert:\│ " makes vertical lines a pretty unbroken line
set noshowmode
set wildmode=list:longest,full

set ignorecase " /, ? and friends are case insensitive
set smartcase " the above setting is toggled of if the search word contains upper-case caracters

set splitbelow " create new splits below and to the right
set splitright

tnoremap <Esc> <C-\><C-n>

" Install Vim Plug if not installed
if empty(glob('~/.config/nvim/autoload/plug.vim'))
silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin()

function! DoRemote(arg)
  UpdateRemotePlugins
endfunction

" Appearence
Plug 'bling/vim-airline'
Plug 'tomasr/molokai'
Plug 'altercation/vim-colors-solarized'
Plug 'rakr/vim-two-firewatch'
Plug 'morhetz/gruvbox'
Plug 'mhinz/vim-startify'

" Plug 'ludovicchabant/vim-gutentags'
Plug 'machakann/vim-highlightedyank'
Plug '/usr/share/vim/vimfiles' " fzf's vim files are installed here by Arch
Plug 'ctrlpvim/ctrlp.vim'
Plug '/usr/local/opt/fzf' " fzf's vim files are installed here by Brew
Plug 'junegunn/fzf.vim'

" Git related things
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

" Navigation
Plug 'farmergreg/vim-lastplace' " Reopen files to last position
Plug 'scrooloose/nerdtree'
" Plug 'henrik/vim-indexed-search' " Shows number of matches with /
Plug 'easymotion/vim-easymotion'
Plug 'ludovicchabant/vim-gutentags' " Automatically manages tags
Plug 'airblade/vim-rooter' " Change the working directory to project root
Plug 'yuttie/comfortable-motion.vim' " Make scrolling bindings animate
Plug 'majutsushi/tagbar'

" # Editing
" Plugins that aid in the insertion and changing of characters
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs' " automatically inserts matching quotes, parenthesis, etc.
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-surround'
Plug 'SirVer/ultisnips'
" Plug 'honza/vim-snippets'
Plug 'terryma/vim-multiple-cursors'

" General language plugins
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') } " Autocompletion
Plug 'shougo/echodoc.vim' " Display function signature in echo area
Plug 'w0rp/ale' " General purposelinting
Plug 'nathanaelkane/vim-indent-guides' " Shows indentation levels
Plug 'janko-m/vim-test' " Runs test
Plug 'scrooloose/nerdcommenter' " Comments code
Plug 'tpope/vim-commentary' " More comments

" Markdown
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

" Javascript
Plug 'othree/yajs.vim'
Plug 'pangloss/vim-javascript'
Plug 'Quramy/vim-js-pretty-template'

" Typescript
Plug 'HerringtonDarkholme/yats.vim'
Plug 'mhartington/nvim-typescript', { 'branch': 'fix-121' }

" Python
Plug 'zchee/deoplete-jedi'

Plug 'ryanoasis/vim-devicons' " Should be called after other plugins
call plug#end()

" Change color theme
set termguicolors
let g:gruvbox_italic=1
colorscheme gruvbox

" Comfortable motion
" This config will scroll proportionally to the window height and was taken
" from the readme.
let g:comfortable_motion_no_default_key_mappings = 1
let g:comfortable_motion_impulse_multiplier = 1  " Feel free to increase/decrease this value.
nnoremap <silent> <C-d> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * 2)<CR>
nnoremap <silent> <C-u> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * -2)<CR>
nnoremap <silent> <C-f> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * 4)<CR>
nnoremap <silent> <C-b> :call comfortable_motion#flick(g:comfortable_motion_impulse_multiplier * winheight(0) * -4)<CR>

" Devicons and Airline
let g:airline_powerline_fonts = 1

" Devicon and Startify
let entry_format = "'   ['. index .']'. repeat(' ', (3 - strlen(index)))"
if exists('*WebDevIconsGetFileTypeSymbol')  " support for vim-devicons
  let entry_format .= ". WebDevIconsGetFileTypeSymbol(entry_path) .' '.  entry_path"
else
  let entry_format .= '. entry_path'
endif

" UltiSnips

let g:UltiSnipsSnippetsDir="~/.config/nvim/UltiSnips"
let g:UltiSnipsSnippetDirectories=['UltiSnips']
" let g:UltiSnipsSnippetDirectories=$HOME.'/.config/nvim/UltiSnips'

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<M-n>"
let g:UltiSnipsJumpBackwardTrigger="<M-p>"

" Deoplete settings
set completeopt-=preview
let g:deoplete#enable_at_startup = 1
call deoplete#custom#source('typescript', 'min_pattern_length', 1)
let g:deoplete#auto_complete_delay = 0
let g:echodoc_enable_at_startup=1

" Ale configuration
" Register the following fixers for the specified files
let g:ale_fixers = {
\   'typescript': ['prettier'],
\   'python': ['yapf'],
\}
" \   'javascript': ['prettier'],

" When fzf starts in a terminal buffer, hide the statusline of the containing buffer.
" autocmd! FileType fzf
" autocmd  FileType fzf set laststatus=0 noshowmode noruler
"   \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" Ale applies fixes on save
let g:ale_fix_on_save = 1

" EasyMotion
let g:EasyMotion_smartcase = 1 " Turn on case insensitive feature

" Markdown
let g:vim_markdown_new_list_item_indent = 2 " indent with 2 spaces
let g:vim_markdown_folding_disabled = 1 " vim-markdown has weird folding defaults

" TypeScript
let g:nvim_typescript#type_info_on_hold = 1
let g:nvim_typescript#default_mappings = 1
let g:nvim_typescript#max_completion_detail=100
let g:nvim_typescript#completion_mark=''

let g:nvim_typescript#kind_symbols = {
      \ 'keyword': 'keyword',
      \ 'class': '',
      \ 'interface': '',
      \ 'script': 'script',
      \ 'module': '',
      \ 'local class': 'local class',
      \ 'type': '',
      \ 'enum': '',
      \ 'enum member': '',
      \ 'alias': '',
      \ 'type parameter': 'type param',
      \ 'primitive type': 'primitive type',
      \ 'var': '',
      \ 'local var': '',
      \ 'property': '',
      \ 'let': '',
      \ 'const': '',
      \ 'label': 'label',
      \ 'parameter': 'param',
      \ 'index': 'index',
      \ 'function': 'λ',
      \ 'local function': 'local function',
      \ 'method': '',
      \ 'getter': '',
      \ 'setter': '',
      \ 'call': 'call',
      \ 'constructor': '',
\}

" Mappings

" fzf
nnoremap <leader>p :GFiles<CR>
nnoremap <leader>P :Files<CR>
nnoremap <leader>l :BTags<CR>
nnoremap <leader>L :BLines<CR>
nnoremap <leader>m :Buffers<CR>
nnoremap <leader>b :History<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>i :TSType<CR>
nnoremap <leader><Enter> :terminal<CR>

nnoremap <leader>w :w<CR>
nnoremap <leader>. :edit ~/.config/nvim/init.vim<CR>
" Toggle between light and dark background
nnoremap <leader>0 :let &background = ( &background == "dark"? "light" : "dark" )<CR>

nmap s <Plug>(easymotion-overwin-f2)

map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
