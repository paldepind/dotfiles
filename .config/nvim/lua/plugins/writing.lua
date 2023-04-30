-- Plugins related to writing things that are not code. I.e., notes, readmes,
-- articles, etc.

return {
  { -- Markdown Tables
    "dhruvasagar/vim-table-mode",
    ft = "markdown"
  },

  {
    'iamcco/markdown-preview.nvim',
    build = function() vim.fn["mkdp#util#install"]() end,
    -- cmd = {
    --   "MarkdownPreview"
    -- },
  },

  -- { -- Zen Mode
  --   "Pocco81/true-zen.nvim",
  --   opts = {},
  --   cmd = {
  --     "TZAtaraxis",
  --     "TZMinimalist",
  --     "TZNarrow",
  --     "TZFocus",
  --   }
  -- },

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
