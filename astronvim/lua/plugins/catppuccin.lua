return {
  "catppuccin/nvim",
  config = function(_, opts)
    require("catppuccin").setup(vim.tbl_deep_extend("force", opts, {
      transparent_background = true,
    }))
    vim.cmd.colorscheme "catppuccin"
  end,
}
