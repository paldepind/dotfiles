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
set inccommand="nosplit"

set hidden " Hidden buffers are preserved

set cursorline " Highlight current line

set splitbelow " create new splits below and to the right
set splitright
let &t_ut='' " Fixis Kitty glitches
map Y y$ " Make
let g:Guifont="Source Code Pro:h10"
if exists('g:GuiLoaded')
  Guifont Source Code Pro:h10
endif

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
Plug 'morhetz/gruvbox'
Plug 'mhinz/vim-startify'
Plug 'yggdroot/indentline'

Plug 'machakann/vim-highlightedyank'
Plug '/usr/share/vim/vimfiles' " fzf's vim files are installed here by Arch Linux
Plug '/usr/local/opt/fzf' " fzf's vim files are installed here by Brew on OSX
Plug 'junegunn/fzf.vim'

" Git related things
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-rhubarb'

" Navigation
Plug 'farmergreg/vim-lastplace' " Reopen files to last position
Plug 'scrooloose/nerdtree'
" Plug 'henrik/vim-indexed-search' " Shows number of matches with /
Plug 'easymotion/vim-easymotion'
" Plug 'ludovicchabant/vim-gutentags' " Automatically manages tags
Plug 'airblade/vim-rooter' " Change the working directory to project root
Plug 'yuttie/comfortable-motion.vim' " Make scrolling bindings animate
Plug 'majutsushi/tagbar'

Plug 't9md/vim-choosewin'
let g:choosewin_overlay_enable = 0
map <Leader>o <Plug>(choosewin)
map <A-o> <Plug>(choosewin)
let g:choosewin_label = 'ARSTNEIOVMDHC,PLFUWY'

" # Editing
" Plugins that aid in the insertion and changing of characters
Plug 'machakann/vim-sandwich'
Plug 'jiangmiao/auto-pairs' " automatically inserts matching quotes, parenthesis, etc.
Plug 'ntpeters/vim-better-whitespace'
Plug 'SirVer/ultisnips'
" Plug 'honza/vim-snippets'
Plug 'terryma/vim-multiple-cursors'

Plug 'junegunn/goyo.vim'
Plug 'reedes/vim-pencil'

" General language plugins
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') } " Autocompletion
Plug 'shougo/echodoc.vim' " Display function signature in echo area
Plug 'w0rp/ale' " General purpose linting
Plug 'janko-m/vim-test' " Runs test
Plug 'tpope/vim-commentary' " More comments

" Markdown
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

" HTML/CSS
Plug 'mustache/vim-mustache-handlebars'
Plug 'digitaltoad/vim-pug'

" Javascript
" Plug 'othree/yajs.vim'
Plug 'pangloss/vim-javascript'
let g:javascript_plugin_jsdoc = 1

Plug 'Quramy/vim-js-pretty-template'

" Typescript
Plug 'leafgarland/typescript-vim'
" Plug 'HerringtonDarkholme/yats.vim'
" Plug 'mhartington/nvim-typescript', { 'branch': 'fix-121' }

" PureScript
Plug 'purescript-contrib/purescript-vim'
Plug 'frigoeu/psc-ide-vim'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }

Plug 'ryanoasis/vim-devicons' " Should be called after other plugins

call plug#end()

" Change color theme
set termguicolors
let g:gruvbox_italic=1
colorscheme gruvbox
set background=light

let g:indentLine_char = '┆'

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
let g:airline_powerline_fonts = 0

" Devicon and Startify
function! StartifyEntryFormat()
  return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
endfunction
" let entry_format = "'   ['. index .']'. repeat(' ', (3 - strlen(index)))"
" if exists('*WebDevIconsGetFileTypeSymbol')  " support for vim-devicons
"   let entry_format .= ". WebDevIconsGetFileTypeSymbol(entry_path) .' '.  entry_path"
" else
"   let entry_format .= '. entry_path'
" endif

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
\   'javascript': ['prettier'],
\   'python': ['yapf'],
\}
" \   'javascript': ['prettier'],

" Ale applies fixes on save
let g:ale_fix_on_save = 1

let g:LanguageClient_serverCommands = {
    \ 'typescript': ['javascript-typescript-stdio'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ }

nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <leader>r :call LanguageClient_textDocument_rename()<CR>
nnoremap <leader>l :call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <leader>L :call LanguageClient_workspace_symbol()<CR>

" EasyMotion
let g:EasyMotion_smartcase = 1 " Turn on case insensitive feature

" Markdown
let g:vim_markdown_new_list_item_indent = 2 " indent with 2 spaces
let g:vim_markdown_folding_disabled = 1 " vim-markdown has weird folding defaults
let g:vim_markdown_conceal = 0
let g:vim_markdown_math = 1

" " TypeScript
" let g:nvim_typescript#type_info_on_hold = 1
" let g:nvim_typescript#default_mappings = 1
" let g:nvim_typescript#max_completion_detail=100
" let g:nvim_typescript#completion_mark=''
" let g:nvim_typescript#kind_symbols = {
"       \ 'keyword': 'keyword',
"       \ 'class': '',
"       \ 'interface': '',
"       \ 'script': 'script',
"       \ 'module': '',
"       \ 'local class': 'local class',
"       \ 'type': '',
"       \ 'enum': '',
"       \ 'enum member': '',
"       \ 'alias': '',
"       \ 'type parameter': 'type param',
"       \ 'primitive type': 'primitive type',
"       \ 'var': '',
"       \ 'local var': '',
"       \ 'property': '',
"       \ 'let': '',
"       \ 'const': '',
"       \ 'label': 'label',
"       \ 'parameter': 'param',
"       \ 'index': 'index',
"       \ 'function': 'λ',
"       \ 'local function': 'local function',
"       \ 'method': '',
"       \ 'getter': '',
"       \ 'setter': '',
"       \ 'call': 'call',
"       \ 'constructor': '',
" \}

" Mappings

" Emacs-style command list
nnoremap <M-x> :Commands<CR>

" fzf
nnoremap <leader>p :GFiles<CR>
nnoremap <leader>P :Files<CR>
nnoremap <leader>m :Buffers<CR>
nnoremap <leader>b :History<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>i :TSType<CR>
nnoremap <leader><Enter> :terminal<CR>

nnoremap <leader>w :w<CR>
nnoremap <leader>. :edit ~/.config/nvim/init.vim<CR>
" Toggle between light and dark background
nnoremap <leader>0 :let &background = ( &background == "dark"? "light" : "dark" )<CR>

nmap <Leader>s <Plug>(easymotion-overwin-f2)

map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
