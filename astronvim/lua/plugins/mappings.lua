return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        -- first key is the mode
        n = {
          ["<Leader>fs"] = { ":w!<cr>", desc = "Save File" }, -- change description but the same command
          ["<A-l>"] = { function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
          ["<A-h>"] = { function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },
          ["<Leader>bd"] = { function() require("astrocore.buffer").close() end, desc = "Close buffer" },
          ["<A-q>"] = { function() require("astrocore.buffer").close() end, desc = "Close buffer" },
          ["<C-p>"] = {
            function()
              require("snacks").picker.smart {
                multi = { "buffers", "recent", "files" },
                format = "file", -- use `file` format for all sources
                matcher = {
                  fuzzy = true, -- use fuzzy matching
                  smartcase = true, -- use smartcase
                  ignorecase = true, -- use ignorecase
                  cwd_bonus = true, -- boost cwd matches
                  frecency = true, -- use frecency boosting
                  sort_empty = true, -- sort even when the filter is empty
                },
                transform = "unique_file",
              }
            end,
            desc = "Find commands",
          },
          ["<Leader>d"] = {
            function()
              if vim.bo.filetype == "neo-tree" then
                vim.cmd.wincmd "p"
              else
                vim.cmd.Neotree "focus"
              end
            end,
            desc = "Toggle Explorer Focus",
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
