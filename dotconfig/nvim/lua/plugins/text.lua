-- Plugins for general purpose text editing and text operations

return {
  {
    "nvim-pack/nvim-spectre",
    dependencies = "nvim-lua/plenary.nvim",
  },
  {
    "gabrielpoca/replacer.nvim",
  },
  {
    "MisanthropicBit/decipher.nvim",
    opts = {
      active_codecs = {
        "base64",
        "base64-url",
      },
      float = {
        padding = 1,
        enter = true,
      }
    },
    keys = {
      {
        "<leader>dd",
        function() require("decipher").decode_motion_prompt({ preview = true }) end,
        "Decode a base64-url encoded text object"
      },
      {
        "<leader>de",
        function() require("decipher").encode_motion_prompt({ preview = true }) end
      }
    }
  }
}
