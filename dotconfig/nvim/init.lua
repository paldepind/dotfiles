-- Changes to defaults
vim.g.mapleader = " "

vim.opt.expandtab = true -- the tab keys inserts spaces
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.updatetime = 500 -- changes the time it takes to trigger "CursorHold"
vim.opt.scrolloff = 5 --
vim.opt.formatoptions = "tcqjro"

-- set noshowmode
-- " set wildmode=list:longest,full
-- set nojoinspaces " Prefer a single space after `.`, `?`, etc.
--
-- " set spell spelllang=en_us " Turn on spell checking
--
-- set ignorecase " /, ?, etc. are case insensitive
vim.opt.ignorecase = true
-- set smartcase " the above setting is toggled of if the search word contains upper-case caracters
-- set inccommand="split"

vim.opt.cursorline = true -- Highlight the current line

vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Go to left window", remap = true })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Go to lower window", remap = true })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Go to upper window", remap = true })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Go to right window", remap = true })

vim.opt.splitbelow = true -- create new splits below and to the right
vim.opt.splitright = true
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "80"

-- vim.opt.guifont = "FiraCode_Nerd_Font_Mono:h12"
-- vim.opt.guifont = "FantasqueSansMono_Nerd_Font:h14"
-- vim.opt.guifont = "Fantasque_Sans_Mono:h14"
-- vim.opt.guifont = "Jetbrains_Mono:h12"
vim.opt.guifont = "Source_Code_Pro:h12"
-- vim.opt.guifont = "Comic_Code_Ligatures_Medium_Nerd_Font_Complete:h13"
--
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", {})

-- Bootstrap lazy
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
-- Now lazy is certain to be present.

local on_attach

require("lazy").setup({
  {
    "kosayoda/nvim-lightbulb",
    dependencies = "antoinemadec/FixCursorHold.nvim", -- Remove at some point when upstream fix is released
  },

  -- Color schemes
  {
    "rmehri01/onenord.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("onenord").setup({
        styles = {
          comments = "italic",
        },
      })
      vim.cmd([[colorscheme onenord]])
    end,
  },
  {
    "svrana/neosolarized.nvim",
    lazy = true,
    config = function()
      require('neosolarized').setup({
        comment_italics = true,
      })
    end,
    dependencies = { "tjdevries/colorbuddy.nvim" },
  },
  { "morhetz/gruvbox", lazy = true },
  { "catppuccin/nvim", name = "catppuccin", lazy = true },
  { "savq/melange", lazy = true },
  { "rose-pine/neovim", name = "rose-pine", lazy = true },
  { "EdenEast/nightfox.nvim", lazy = true },

  -- UI/UX related plugins
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },
  {
    'echasnovski/mini.files',
    main = 'mini.files',
    version = false,
    opts = {},
  },

  -- General editing
  "numToStr/Comment.nvim",

  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end,
  },

  {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({})
    end,
  },

  "ntpeters/vim-better-whitespace",

  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
  },

  {
    "abecodes/tabout.nvim",
    config = function()
      require("tabout").setup({
        tabkey = "<Tab>", -- key to trigger tabout, set to an empty string to disable
        backwards_tabkey = "<S-Tab>", -- key to trigger backwards tabout, set to an empty string to disable
        act_as_tab = true, -- shift content if tab out is not possible
        act_as_shift_tab = false, -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
        default_tab = "<C-t>", -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
        default_shift_tab = "<C-d>", -- reverse shift default action,
        enable_backwards = true, -- well ...
        completion = true, -- if the tabkey is used in a completion pum
        tabouts = {
          { open = "'", close = "'" },
          { open = '"', close = '"' },
          { open = "`", close = "`" },
          { open = "(", close = ")" },
          { open = "[", close = "]" },
          { open = "{", close = "}" },
        },
        ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
        exclude = {}, -- tabout will ignore these filetypes
      })
    end,
    dependencies = { "nvim-treesitter" }, -- or require if not used so far
    -- after = { "nvim-cmp" }, -- if a completion plugin is using tabs load it before
  },

  "L3MON4D3/LuaSnip",
  "saadparwaiz1/cmp_luasnip",

  "https://gitlab.com/yorickpeterse/nvim-window.git",

  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = { { "nvim-lua/plenary.nvim" } },
  },
  { "nvim-telescope/telescope-file-browser.nvim" },
  { "nvim-telescope/telescope-project.nvim" },

  "airblade/vim-rooter", -- Change the working directory to project root
  -- use {
  --   "ahmedkhalf/project.nvim",
  --   config = function()
  --     require("project_nvim").setup {}
  --   end
  -- }

  -- Git related.
  "lewis6991/gitsigns.nvim",
  -- " use 'tpope/vim-fugitive'
  -- " use 'rhysd/git-messenger.vim'
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",         -- required
      "nvim-telescope/telescope.nvim", -- optional
      "sindrets/diffview.nvim",        -- optional
      "ibhagwan/fzf-lua",              -- optional
    },
    config = true,
    cmd = "Neogit"
  },

  "neovim/nvim-lspconfig", -- Configurations for Nvim LSP
  {
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function()
      require("lsp_lines").setup()
    end,
    keys = {
      { "<leader>D", function()
        local new_value = not vim.diagnostic.config().virtual_lines
        vim.diagnostic.config({ virtual_lines = new_value, virtual_text = not new_value })
      end }
    }
  },

  -- Auto-completion
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/cmp-buffer",
  "hrsh7th/cmp-path",
  "hrsh7th/cmp-cmdline",
  "hrsh7th/nvim-cmp",

  -- Rust
  {
    "simrat39/rust-tools.nvim",
    ft = "rust",
    config = function()
      local rt = require("rust-tools")
      rt.setup {
        server = {
          on_attach = on_attach,
          -- on_attach = function(client, bufnr)
          --   vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
          --   vim.keymap.set('n', '<C-k>', vim.lsp.buf.hover, bufopts)
          --   print(on_attach)
          --   on_attach(client, bufnr)
          -- end,
        }
      }
    end
  },

  -- TypeScript & JavaScript
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {},
    config = function()
      local tt = require("typescript-tools")
      tt.setup {
        on_attach = function()
          -- -- Enable completion triggered by <c-x><c-o>
          -- vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

          -- Mappings.
          local bufopts = { noremap=true, silent=true, buffer=bufnr }
          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
          vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
          vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
          vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
          vim.keymap.set('n', '<M-K>', vim.lsp.buf.signature_help, bufopts)
          -- vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bufopts)
          -- vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
          -- vim.keymap.set('n', '<leader>wl', function()
          --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          -- end, bufopts)
          -- vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
          --
          vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, bufopts)
          vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
          vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
        end
        -- on_attach = on_attach,
      }
    end
  },

  -- Coq
  -- filetype plugin indent on
  {
    "whonore/coqtail",
    ft = "coq",
  },
  {
    -- dir = "~/projects/nvim-cmp-lua-latex-symbols",
    "kdheepak/cmp-latex-symbols",
    ft = "coq",
  },
  {
    "phaazon/hop.nvim",
    branch = "v2", -- optional but strongly recommended
    config = function()
      -- you can configure Hop the way you like here; see :h hop-config
      require("hop").setup({ keys = "arstneiovmdhc,plfuwy" })
    end,
  },
  -- 'ggandor/leap.nvim'

  {
    "ethanholz/nvim-lastplace",
     opts = {}
  },

  -- Fish shell
  { "khaveesh/vim-fish-syntax" },

  -- Import everything in the plugins directory
  { import = "plugins" }
})

vim.g.coqtail_noimap = true

require("Comment").setup {
  ignore = "^$",
}

require("nvim-treesitter.configs").setup({
  -- A list of parser names, or "all"
  ensure_installed = { "lua", "vim", "rust", "markdown", "markdown_inline" },

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
})


require("nvim-lightbulb").setup({ autocmd = { enabled = true } })

local cmp = require("cmp")

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "luasnip" }, -- snippet plugin to use
    {
      name = "latex_symbols",
      option = {
        strategy = 0, -- mixed
      },
    },
  }, {
    { name = "buffer" },
  })
})

-- highlight yanked text
vim.cmd([[
  augroup highlight_yank
  autocmd!
  au TextYankPost * silent! lua vim.highlight.on_yank({ timeout = 500, on_visual = false })
  augroup END
]])

vim.keymap.set("n", "<leader>o", require("nvim-window").pick, {})

-- luasnip setup
require("luasnip").config.set_config({ -- Setting LuaSnip config

  -- Enable autotriggered snippets
  enable_autosnippets = true,

  -- Use Tab (or some other key if you prefer) to trigger visual selection
  store_selection_keys = "<Tab>",

  update_events = "TextChanged,TextChangedI", -- repeats are updated on every change
})

require("luasnip.loaders.from_lua").lazy_load({ paths = "~/.config/nvim/luasnippets/" })

vim.cmd([[
  " Use Tab to expand and jump through snippets
  imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>'
  smap <silent><expr> <Tab> luasnip#jumpable(1) ? '<Plug>luasnip-jump-next' : '<Tab>'

  " Use Shift-Tab to jump backwards through snippets
  imap <silent><expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'
  smap <silent><expr> <S-Tab> luasnip#jumpable(-1) ? '<Plug>luasnip-jump-prev' : '<S-Tab>'

  imap <silent><expr> <C-f> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-f>'
  smap <silent><expr> <C-f> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-f>'
]])

--
-- " # Editing
-- " Plugins for general editing that aids in changing text
--
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
-- " use 'janko-m/vim-test' " Runs test
-- " use 'meain/vim-package-info', { 'do': 'npm install' }
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
-- " LaTeX
--
-- use 'lervag/vimtex'
-- let g:tex_flavor = 'latex'
vim.g.vimtex_view_method = 'skim'
-- vim.g.vimtex_view_method = 'zathura'
-- nmap <localleader>v <plug>(vimtex-view)
vim.g.vimtex_quickfix_open_on_warning = false

vim.opt.background = "light" -- or "dark" for dark mode

-- " Markdown
-- let g:vim_markdown_new_list_item_indent = 2 " indent with 2 spaces
-- let g:vim_markdown_folding_disabled = 1 " vim-markdown has weird folding defaults
-- let g:vim_markdown_conceal = 0
-- let g:vim_markdown_math = 1

-- Toggle between light and dark background
vim.keymap.set("n", "<leader>0", function()
  if vim.opt.background:get() == "light" then
    vim.opt.background = "dark"
  else
    vim.opt.background = "light"
  end
end, { noremap = true })

require("nvim-window").setup({
  -- The characters available for hinting windows.
  chars = {
    "a", "r", "s", "t", "n", "e", "i", "o", "v", "m", "d", "h", "c", ",", "p",
    "l", "f", "u", "w", "y",
  },
})
--
require("gitsigns").setup()

-- Disable virtual_text since it's redundant due to lsp_lines.
-- vim.diagnostic.config({
--   -- virtual_text = false,
--   virtual_text = true,
--   virtual_lines = false,
-- })

local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Language Servers

on_attach = function(_, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<M-K>', vim.lsp.buf.signature_help, bufopts)
  -- vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  -- vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  -- vim.keymap.set('n', '<leader>wl', function()
  --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  -- end, bufopts)
  -- vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  -- vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end

local lspconfig = require 'lspconfig'
lspconfig.marksman.setup { on_attach = on_attach }
lspconfig.texlab.setup {
  on_attach = on_attach,
  settings = {
    texlab = {
      diagnostics = {
        allowedPatterns = { "$-" }, -- Regex that does not match anything as texlab errors are obnoxious and incorrect for my LaTeX files
      }
    }
  }
}

lspconfig.ltex.setup {
  on_attach = function(client, bufnr)
    on_attach(client, bufnr)
      require("ltex_extra").setup{
        load_langs = { "en-US", "da-DK" },
        init_check = true,
        path = vim.fn.stdpath("config") .. "/spell", -- string : path to store dictionaries. Relative path uses current working directory
        log_level = "none",
      }
    end,
  settings = {
    ltex = {
      -- language = 'en-US',
      diagnosticSeverity = 'information',
    },
  },
}

lspconfig.gopls.setup { on_attach = on_attach }

-- Format with LSP on save.
vim.cmd([[autocmd BufWritePre *.rs lua vim.lsp.buf.format({ async = false })]])

-- Telescope setup

local telescope = require("telescope")
local actions = require("telescope.actions")
local builtin = require("telescope.builtin")

telescope.load_extension("file_browser")
telescope.load_extension("project")

telescope.setup({
  pickers = {
    buffers = {
      ignore_current_buffer = true,
      sort_mru = true,
    },
  },
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = actions.close,
      },
    },
  },
  extensions = {
    project = {
      base_dirs = {
        { "~/projects", max_depth = 3 },
      },
    },
  },
})

-- Leap
-- require('leap').add_default_mappings()
-- Hop
local hop = require("hop")
local directions = require("hop.hint").HintDirection

vim.keymap.set('', 's', function()
  hop.hint_char2({ direction = directions.AFTER_CURSOR })
end, {remap=true})
vim.keymap.set('', 'S', function()
  hop.hint_char2({ direction = directions.BEFORE_CURSOR })
end, {remap=true})

-- _ALL_ leader bindings go here (except for LSP ones).
vim.api.nvim_set_keymap("n", "<leader>q", ":q<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>Q", ":wqa<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader><Enter>", ":terminal<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>w", ":w<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>,", ":edit ~/.config/nvim/init.lua<CR>", { noremap = true })
vim.keymap.set("n", "<leader>g", function() require("neogit").open() end, {})
-- Telescope
vim.keymap.set("n", "<leader>p", builtin.find_files, {})
vim.keymap.set("n", "<leader>P", telescope.extensions.project.project, {})
vim.keymap.set("n", "<leader>/", builtin.live_grep, {})
vim.keymap.set("n", "<leader>m", builtin.buffers, {})
vim.keymap.set("n", "<leader>M", builtin.oldfiles, {})
-- vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
vim.keymap.set("n", "<leader>K", builtin.colorscheme, {})
-- Hop
vim.keymap.set("n", "<leader>j", function()
  hop.hint_lines({ direction = directions.AFTER_CURSOR })
end, {})
vim.keymap.set("n", "<leader>k", function()
  hop.hint_lines({ direction = directions.BEFORE_CURSOR })
end, {})

-- File browser from path of current buffer
vim.keymap.set("n", "<leader>f",
  function() telescope.extensions.file_browser.file_browser { path = "%:p:h", select_buffer = true } end
  , { noremap = true })

-- Emacs-style command list
vim.keymap.set("n", "<M-x>", builtin.commands, {})

require("lualine").setup({
  options = {
    section_separators = "",
    component_separators = "",
    theme = "onenord",
    -- section_separators = { left = '', right = '' },
    -- component_separators = { left = '', right = '' }
  },
})
