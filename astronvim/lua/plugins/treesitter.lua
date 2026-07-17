-- Customize Treesitter

---@type LazySpec
return {
  "nvim-treesitter/nvim-treesitter",
  opts = {
    highlight = true, -- enable/disable treesitter based highlighting
    indent = true, -- enable/disable treesitter based indentation
    auto_install = true, -- enable/disable automatic installation of detected languages
    ensure_installed = {
      "lua",
      "vim",
      "graphql",
      -- add more arguments for adding more treesitter parsers
    },
  },
}
