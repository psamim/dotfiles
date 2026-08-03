return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        -- first key is the mode
        n = {
          ["<Leader>fs"] = { ":w!<cr>", desc = "Save File" }, -- change description but the same command
          ["<A-l>"] = { "<Cmd>BufferNext<CR>", desc = "Next buffer" },
          ["<A-h>"] = { "<Cmd>BufferPrevious<CR>", desc = "Previous buffer" },
          ["<Leader>bd"] = { function() require("astrocore.buffer").close() end, desc = "Close buffer" },
          ["<Leader>ff"] = {
            function() require("fff").find_files() end,
            desc = "Find files",
          },
          ["<Leader>fw"] = {
            function() require("fff").live_grep_under_cursor() end,
            desc = "Search word/selection",
          },
          ["<Leader>fp"] = {
            function()
              local path = vim.fn.expand "%:p"
              vim.fn.setreg("+", path)
              vim.notify(path)
            end,
            desc = "Copy full file path",
          },
          ["<Leader>d"] = { "<Cmd>Yazi<CR>", desc = "Open yazi at the current file" },
        },
        x = {
          ["<Leader>fw"] = {
            function() require("fff").live_grep_under_cursor() end,
            desc = "Search selection",
          },
        },
        t = {
          ["jk"] = [[<C-\><C-n>]],
        },
      },
    },
  },
  {
    "AstroNvim/astrolsp",
    ---@type AstroLSPOpts
    opts = {
      mappings = {
        n = {
          -- condition for only server with declaration capabilities
          gD = {
            function() vim.lsp.buf.declaration() end,
            desc = "Declaration of current symbol",
            cond = "textDocument/declaration",
          },
        },
      },
    },
  },
}
