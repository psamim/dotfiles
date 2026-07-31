return {
  "romgrk/barbar.nvim",
  dependencies = {
    { import = "astrocommunity.recipes.disable-tabline" }, -- dependency before loading rest of the spec
    {
      "AstroNvim/astrocore",
      opts = {
        mappings = {
          n = {
            ["]b"] = { "<Cmd>BufferNext<CR>", desc = "Next buffer" },
            ["[b"] = { "<Cmd>BufferPrevious<CR>", desc = "Previous buffer" },
            [">b"] = { "<Cmd>BufferMoveNext<CR>", desc = "Move buffer tab right" },
            ["<b"] = { "<Cmd>BufferMovePrevious<CR>", desc = "Move buffer tab left" },
            ["<Leader>bb"] = { "<Cmd>BufferPick<CR>", desc = "Navigate to buffer tab with interactive picker" },
            ["<Leader>bc"] = { "<Cmd>BufferCloseAllButCurrent<CR>", desc = "Close all buffers except the current" },
            ["<Leader>bl"] = {
              "<Cmd>BufferCloseBuffersLeft<CR>",
              desc = "Close all buffers to the left of the current",
            },
            ["<Leader>br"] = {
              "<Cmd>BufferCloseBuffersRight<CR>",
              desc = "Close all buffers to the right of the current",
            },
            ["<Leader>bp"] = { "<Cmd>BufferPin<CR>", desc = "Toggle pin buffer" },
          },
        },
      },
    },
  },
  event = "VeryLazy",
  init = function() vim.g.barbar_auto_setup = false end,
  opts = {
    icons = {
      filetype = { enabled = true },
    },
  },
}
