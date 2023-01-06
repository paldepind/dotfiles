-- Changes to defaults
vim.g.mapleader = " "

vim.opt.expandtab = true -- the tab keys inserts spaces
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
-- vim.api.nvim_set_option('updatetime', 500)
vim.o.updatetime = 400
--
-- set noshowmode
-- " set wildmode=list:longest,full
-- set nojoinspaces " Prefer a single space after `.`, `?`, etc.
-- 
-- " set spell spelllang=en_us " Turn on spell checking
-- 
-- set ignorecase " /, ?, etc. are case insensitive
-- set smartcase " the above setting is toggled of if the search word contains upper-case caracters
-- set inccommand="split"

vim.opt.cursorline = true -- Highlight the current line

vim.opt.splitbelow = true -- create new splits below and to the right
vim.opt.splitright = true
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "80"

-- set colorcolumn=80
-- 
vim.opt.guifont = "FiraCode_Nerd_Font_Mono:h12"
-- vim.opt.guifont = "JetBrains_Mono:h12"
-- 
vim.keymap.set('t', '<Esc>', '<C-\\><C-n>', {})

require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  use {
    'kosayoda/nvim-lightbulb',
    -- requires = 'antoinemadec/FixCursorHold.nvim', -- Remove at some point when upstream fix is released
  }

  -- Color schemes
	use 'morhetz/gruvbox'
  use 'rmehri01/onenord.nvim'
  use { "catppuccin/nvim", as = "catppuccin" }
  use "savq/melange"

  -- UI/UX related plugins
  use 'karb94/neoscroll.nvim' -- Animated scrolling.
  -- use 'mhinz/vim-startify'
  -- use 'lukas-reineke/indent-blankline.nvim'
  --
  use {
    'goolord/alpha-nvim',
    requires = { 'nvim-tree/nvim-web-devicons' },
    config = function ()
        require'alpha'.setup(require'alpha.themes.startify'.config)
    end
  }

  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
  }

  -- General editing
	use 'tpope/vim-commentary' -- Comment action

  use({
    "kylechui/nvim-surround",
    tag = "*", -- Use for stability; omit to use `main` branch for the latest features
    config = function()
      require("nvim-surround").setup({
          -- Configuration here, or leave empty to use defaults
      })
    end
  })

  use {
    "windwp/nvim-autopairs",
      config = function() require("nvim-autopairs").setup {} end
  }

  use 'ntpeters/vim-better-whitespace'

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }

  use {
    'abecodes/tabout.nvim',
    config = function()
      require('tabout').setup {
        tabkey = '<Tab>', -- key to trigger tabout, set to an empty string to disable
        backwards_tabkey = '<S-Tab>', -- key to trigger backwards tabout, set to an empty string to disable
        act_as_tab = true, -- shift content if tab out is not possible
        act_as_shift_tab = false, -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
        default_tab = '<C-t>', -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
        default_shift_tab = '<C-d>', -- reverse shift default action,
        enable_backwards = true, -- well ...
        completion = true, -- if the tabkey is used in a completion pum
        tabouts = {
          {open = "'", close = "'"},
          {open = '"', close = '"'},
          {open = '`', close = '`'},
          {open = '(', close = ')'},
          {open = '[', close = ']'},
          {open = '{', close = '}'}
        },
        ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
        exclude = {} -- tabout will ignore these filetypes
      }
    end,
    wants = {'nvim-treesitter'}, -- or require if not used so far
    after = {'nvim-cmp'} -- if a completion plugin is using tabs load it before
  }

  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'

  use 'https://gitlab.com/yorickpeterse/nvim-window.git'

  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
  -- or                            , branch = '0.1.x',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  use { "nvim-telescope/telescope-file-browser.nvim" }

  use 'airblade/vim-rooter' -- Change the working directory to project root
  -- use {
  --   "ahmedkhalf/project.nvim",
  --   config = function()
  --     require("project_nvim").setup {}
  --   end
  -- }

  -- Git related.
  use 'lewis6991/gitsigns.nvim'
  -- " use 'tpope/vim-fugitive'
  -- " use 'airblade/vim-gitgutter'
  -- " use 'jreybert/vimagit'
  -- " use 'rhysd/git-messenger.vim'

  use 'neovim/nvim-lspconfig' -- Configurations for Nvim LSP

  -- Auto-completion
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'
  use 'hrsh7th/nvim-cmp'

  -- Rust
  use 'simrat39/rust-tools.nvim'

end)

require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  ensure_installed = { "lua", "rust" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
  auto_install = false,

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}

local rt = require("rust-tools")
rt.setup({
  server = {
    on_attach = function(_, bufnr)
      -- Hover actions
      vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
})

require('nvim-lightbulb').setup({autocmd = {enabled = true}})

local cmp = require('cmp')

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    -- { name = 'vsnip' }, -- For vsnip users.
    { name = 'luasnip' }, -- For luasnip users.
    -- { name = 'ultisnips' }, -- For ultisnips users.
    -- { name = 'snippy' }, -- For snippy users.
  }, {
    { name = 'buffer' },
  })
})

-- use 'tpope/vim-repeat' " Makes it possible for plugins to be repeatable
-- 
-- " Color Schemes
-- use 'morhetz/gruvbox'
-- use 'altercation/vim-colors-solarized'
-- use 'arzg/vim-colors-xcode'
-- use 'navarasu/onedark.nvim'
-- use 'rose-pine/neovim'
-- 
-- highlight yanked text
vim.cmd[[
  augroup highlight_yank
  autocmd!
  au TextYankPost * silent! lua vim.highlight.on_yank({ timeout = 500, on_visual = false })
  augroup END
]]
-- 
-- " Navigation
-- use 'farmergreg/vim-lastplace' " Reopen files to last position
-- " use 'henrik/vim-indexed-search' " Shows number of matches with /
-- use 'easymotion/vim-easymotion'
-- use 'ggandor/leap.nvim'
-- " use 'majutsushi/tagbar'
-- 
vim.keymap.set('n', '<leader>o', require('nvim-window').pick, {})
-- 
-- " # Editing
-- " Plugins for general editing that aids in changing text
-- 
-- " use 'machakann/vim-sandwich'
-- use 'ntpeters/vim-better-whitespace' " highlight trailing whitespace, offers :StripWhitespace
-- " use 'matze/vim-move' " Adds <A-j> and <A-k> to move lines up and down
-- use 'wellle/targets.vim'
-- use 'andymass/vim-matchup' " even better %
-- 
-- " For prose
-- " let g:goyo_height = '100%'
-- " use 'junegunn/goyo.vim'
-- " use 'reedes/vim-pencil'
-- 
-- " General language plugins
-- 
-- " use 'Shougo/deoplete.nvim', { 'do': function('DoRemote') } " Autocompletion
-- " use 'shougo/echodoc.vim' " Display function signature in echo area
-- " use 'w0rp/ale' " General purpose linting
-- " use 'janko-m/vim-test' " Runs test
-- " use 'meain/vim-package-info', { 'do': 'npm install' }
-- 
-- " use 'RRethy/vim-illuminate' " Highlight word under cursor
-- 
-- " Markdown
-- " use 'godlygeek/tabular'
-- " use 'plasticboy/vim-markdown'
-- " use 'sidofc/mkdx'
-- " let g:mkdx#settings = { 'highlight': { 'enable': 1 },
-- "                     \ 'enter': { 'shift': 1 },
-- "                     \ 'links': { 'external': { 'enable': 1 } },
-- "                     \ 'fold': { 'enable': 0 } }
-- " use 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install' }
-- 
-- " HTML/CSS
-- " use 'mustache/vim-mustache-handlebars'
-- " use 'digitaltoad/vim-pug'
-- " use 'rrethy/vim-hexokinase'
-- 
-- " Javascript
-- " use 'pangloss/vim-javascript'
-- " let g:javascript_plugin_jsdoc = 1
-- " use 'Quramy/vim-js-pretty-template'
-- 
-- " Typescript
-- " use 'leafgarland/typescript-vim'
-- " use 'HerringtonDarkholme/yats.vim'
-- 
-- " Coq
-- filetype plugin indent on
-- use 'whonore/coqtail'
-- use 'joom/latex-unicoder.vim'
-- 
-- " LaTeX
-- 
-- use 'lervag/vimtex'
-- let g:tex_flavor = 'latex'
-- let g:vimtex_view_method = 'skim'
-- nmap <localleader>v <plug>(vimtex-view)
-- 
-- use 'ryanoasis/vim-devicons' " Should be called after other plugins
-- 
-- call plug#end()
-- 
-- " Change color theme
-- set termguicolors
-- let g:gruvbox_italic=1
-- " let g:gruvbox_sign_column = 'bg0'
vim.opt.background = "light" -- or "light" for light mode
vim.cmd([[colorscheme onenord]])

-- Neoscroll
require('neoscroll').setup()
-- 
-- " Devicon and Startify
-- function! StartifyEntryFormat()
--   return 'WebDevIconsGetFileTypeSymbol(absolute_path) ." ". entry_path'
-- endfunction
-- " let entry_format = "'   ['. index .']'. repeat(' ', (3 - strlen(index)))"
-- " if exists('*WebDevIconsGetFileTypeSymbol')  " support for vim-devicons
-- "   let entry_format .= ". WebDevIconsGetFileTypeSymbol(entry_path) .' '.  entry_path"
-- " else
-- "   let entry_format .= '. entry_path'
-- " endif
-- 
-- " EasyMotion
-- let g:EasyMotion_smartcase = 1 " Turn on case insensitive feature
-- let g:EasyMotion_keys = 'arsdheiqwfpgjluy;zxcvbkmtno'
-- let g:EasyMotion_startofline = 0 " keep cursor colum JK motion
-- 
-- " Markdown
-- let g:vim_markdown_new_list_item_indent = 2 " indent with 2 spaces
-- let g:vim_markdown_folding_disabled = 1 " vim-markdown has weird folding defaults
-- let g:vim_markdown_conceal = 0
-- let g:vim_markdown_math = 1

-- " Mappings
-- 
vim.api.nvim_set_keymap("n", "<leader>q", ":q<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader><Enter>", ":terminal<CR>", { noremap = true })
-- 
vim.api.nvim_set_keymap("n", "<leader>w", ":w<CR>", { noremap = true })
--
vim.api.nvim_set_keymap("n", "<leader>,", ":edit ~/.config/nvim/init.lua<CR>", { noremap = true })

-- Toggle between light and dark background
vim.keymap.set('n', '<leader>0', (function ()
  if vim.opt.background:get() == "light" then
    vim.opt.background = "dark"
  else
    vim.opt.background = "light"
  end
end), { noremap = true })
-- 
-- nmap <Leader>s <Plug>(easymotion-overwin-f2)
-- 
-- map <Leader>j <Plug>(easymotion-j)
-- map <Leader>k <Plug>(easymotion-k)
-- 
-- require('lualine').setup({
--   options = { section_separators = '', component_separators = '' }
-- })
-- 
-- require('leap').set_default_keymaps()
-- 
require('nvim-window').setup({
  -- The characters available for hinting windows.
  chars = {
    'a', 'r', 's', 't', 'n', 'e', 'i', 'o', 'v', 'm', 'd', 'h', 'c',
    ',', 'p', 'l', 'f', 'u', 'w', 'y'
  }
})
-- 
require('gitsigns').setup()
-- 
require'lspconfig'.ocamllsp.setup{}

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client.server_capabilities.hoverProvider then
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = args.buf })
    end

    vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, { buffer = args.buf })
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = args.buf })
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer = args.buf })

    if client.server_capabilities.documentHighlightProvider then
      vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
      vim.api.nvim_clear_autocmds { buffer = bufnr, group = "lsp_document_highlight" }
      vim.api.nvim_create_autocmd("CursorHold", {
          callback = vim.lsp.buf.document_highlight,
          buffer = bufnr,
          group = "lsp_document_highlight",
          desc = "Document Highlight",
      })
      vim.api.nvim_create_autocmd("CursorMoved", {
          callback = vim.lsp.buf.clear_references,
          buffer = bufnr,
          group = "lsp_document_highlight",
          desc = "Clear All the References",
      })
    end
  end,
})

local telescope = require('telescope')
-- telescope.load_extension('projects')
telescope.load_extension('file_browser')
telescope.setup {
  pickers = {
    buffers = {
      ignore_current_buffer = true,
      sort_mru = true,
    },
  },
}
local builtin = require('telescope.builtin')

-- require'telescope'.extensions.projects.projects{}
-- vim.keymap.set('n', '<leader>p', telescope.extensions.projects.projects, {})
vim.keymap.set('n', '<leader>f', builtin.find_files, {})
-- vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>m', builtin.buffers, {})
-- vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
vim.keymap.set('n', '<leader>pc', builtin.colorscheme, {})

vim.api.nvim_set_keymap("n", "<space>F", ":Telescope file_browser<CR>", { noremap = true })

-- Emacs-style command list
-- nnoremap <M-x> :Commands<CR>
vim.keymap.set('n', 'M-x', builtin.commands, {})

require('lualine').setup()
