" Changes to defaults
let mapleader=" "
set mouse=a " the mouse can be used
set expandtab " the tab keys inserts spaces
set shiftwidth=2
set tabstop=2
set fillchars=vert:\│ " makes vertical lines a pretty unbroken line
set noshowmode
" set wildmode=list:longest,full
set nojoinspaces " Prefer a single space after `.`, `?`, etc.

set spell spelllang=en_us " Turn on spell checking

set ignorecase " /, ?, etc. are case insensitive
set smartcase " the above setting is toggled of if the search word contains upper-case caracters
set inccommand="split"

set hidden " Hidden buffers are preserved

set cursorline " Highlight current line

set splitbelow " create new splits below and to the right
set splitright
map Y y$ " Make Y yank to end of line

set guifont=Fira_Code:h10

autocmd TermOpen * tnoremap <Esc> <c-\><c-n>
" autocmd FileType fzf tunmap <Esc>

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
" Plug 'yggdroot/indentline'

Plug 'machakann/vim-highlightedyank'
Plug '/usr/share/vim/vimfiles' " fzf's vim files are installed here by Arch Linux
Plug '/usr/local/opt/fzf' " fzf's vim files are installed here by Brew on OSX
Plug 'junegunn/fzf.vim'
" Plug 'Shougo/denite.nvim'

" Git related things
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-rhubarb'
Plug 'jreybert/vimagit'

" Navigation
Plug 'farmergreg/vim-lastplace' " Reopen files to last position
Plug 'scrooloose/nerdtree'
" Plug 'henrik/vim-indexed-search' " Shows number of matches with /
Plug 'easymotion/vim-easymotion'
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
Plug 'wellle/targets.vim'
Plug 'andymass/vim-matchup' " even better %

" For prose
let g:goyo_height = '100%'
Plug 'junegunn/goyo.vim'
Plug 'reedes/vim-pencil'

" General language plugins
" Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') } " Autocompletion
" Plug 'shougo/echodoc.vim' " Display function signature in echo area
" Plug 'w0rp/ale' " General purpose linting
Plug 'janko-m/vim-test' " Runs test
Plug 'tpope/vim-commentary' " More comments

Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}

" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
" begin COC dump

" Better display for messages
" set cmdheight=2

" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
" set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Show signature help on placeholder jump
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

" Use `[e` and `]e` for navigate diagnostics
nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
vmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Use `:Format` for format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` for fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add diagnostic info for Airline
let g:airline_section_error = '%{airline#util#wrap(airline#extensions#coc#get_error(),0)}'
let g:airline_section_warning = '%{airline#util#wrap(airline#extensions#coc#get_warning(),0)}'

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
" nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>l  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
" end COC dump
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Markdown
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install' }

" HTML/CSS
Plug 'mustache/vim-mustache-handlebars'
Plug 'digitaltoad/vim-pug'

" Javascript
Plug 'pangloss/vim-javascript'
let g:javascript_plugin_jsdoc = 1

Plug 'Quramy/vim-js-pretty-template'

" Typescript
" Plug 'leafgarland/typescript-vim'
Plug 'HerringtonDarkholme/yats.vim'

" PureScript
Plug 'purescript-contrib/purescript-vim'
Plug 'frigoeu/psc-ide-vim'

Plug 'ryanoasis/vim-devicons' " Should be called after other plugins

" Go
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

call plug#end()

" Change color theme
set termguicolors
let g:gruvbox_italic=1
" let g:gruvbox_sign_column = 'bg0'
colorscheme gruvbox
set background=light

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

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<M-n>"
let g:UltiSnipsJumpBackwardTrigger="<M-p>"

" Ale configuration
" Register the following fixers for the specified files
" let g:ale_fixers = {
" \   'typescript': ['prettier'],
" \   'javascript': ['prettier'],
" \   'css': ['prettier'],
" \   'html': ['prettier'],
" \   'python': ['yapf'],
" \}

" " Ale applies fixes on save
" let g:ale_fix_on_save = 1

" EasyMotion

let g:EasyMotion_smartcase = 1 " Turn on case insensitive feature

" Markdown
let g:vim_markdown_new_list_item_indent = 2 " indent with 2 spaces
let g:vim_markdown_folding_disabled = 1 " vim-markdown has weird folding defaults
let g:vim_markdown_conceal = 0
let g:vim_markdown_math = 1

" Mappings

" Emacs-style command list
nnoremap <M-x> :Commands<CR>

" fzf
nnoremap <leader>p :GFiles<CR>
nnoremap <leader>P :Files<CR>
" Recent files and open buffers
nnoremap <leader>m :History<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader><Enter> :terminal<CR>

nnoremap <leader>w :w<CR>
nnoremap <leader>. :edit ~/.config/nvim/init.vim<CR>
" Toggle between light and dark background
nnoremap <leader>0 :let &background = ( &background == "dark"? "light" : "dark" )<CR>

nmap <Leader>s <Plug>(easymotion-overwin-f2)

map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
