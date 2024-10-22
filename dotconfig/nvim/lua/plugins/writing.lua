-- Plugins related to writing things that are not code. I.e., notes, readmes,
-- articles, etc.

return {
  { -- Markdown Tables
    "dhruvasagar/vim-table-mode",
    ft = "markdown"
  },

  {
    'MeanderingProgrammer/render-markdown.nvim',
    -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
    -- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
    dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
    ---@module 'render-markdown'
    ---@type render.md.UserConfig
    opts = {
      sign = { enabled = false },
    },
  },

  -- Browser markdown preview
  {
    'iamcco/markdown-preview.nvim',
    build = function() vim.fn["mkdp#util#install"]() end,
    -- cmd = {
    --   "MarkdownPreview"
    -- },
  },

  { -- Zen Mode
    "folke/zen-mode.nvim",
    opts = {
      window = {
        width = 82,
      },
    },
    cmd = { "ZenMode" }
  },

  { -- LaTeX
    'lervag/vimtex'
  },
  { -- Extra things for LaTeX lsp
    'barreiroleo/ltex-extra.nvim'
  }

}
