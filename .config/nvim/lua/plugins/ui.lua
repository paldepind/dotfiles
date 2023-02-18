-- Plugins focused on chainging the visual appearence of Nvim.

return {
  { -- Animated scrolling.
    "karb94/neoscroll.nvim",
    opts = {},
  },
	"lukas-reineke/indent-blankline.nvim",
	{ -- Startup screen
		"goolord/alpha-nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("alpha").setup(require("alpha.themes.startify").config)
		end,
  },
  { -- Highlights the word under cursor using LSP or regex.
    "RRethy/vim-illuminate",
    event = { "BufReadPost", "BufNewFile" },
    opts = { delay = 200 },
    config = function(_, opts)
      require("illuminate").configure(opts)
      vim.api.nvim_create_autocmd("FileType", {
        callback = function()
          local buffer = vim.api.nvim_get_current_buf()
          pcall(vim.keymap.del, "n", "]]", { buffer = buffer })
          pcall(vim.keymap.del, "n", "[[", { buffer = buffer })
        end,
      })
    end,
    keys = {
      { "]]", function() require("illuminate").goto_next_reference(false) end, desc = "Next Reference", },
      { "[[", function() require("illuminate").goto_prev_reference(false) end, desc = "Prev Reference" },
    },
  },

  { -- file explorer
    "nvim-neo-tree/neo-tree.nvim",
    cmd = "Neotree",
    keys = {
      {
        "<D-S-e>", -- VSCode like mapping for the lulz
        function()
          require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
        end,
        desc = "Toggle NeoTree",
      },
    },
    init = function()
      vim.g.neo_tree_remove_legacy_commands = 1
    end,
    opts = {
      filesystem = {
    --     bind_to_cwd = false,
        follow_current_file = true,
      },
    --   window = {
    --     mappings = {
    --       ["<space>"] = "none",
    --     },
    --   },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    },
  },
}
