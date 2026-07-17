-- AstroCore provides a central place to modify mappings, vim options, autocommands, and more!
-- Configuration documentation can be found with `:h astrocore`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- Configure core features of AstroNvim
    -- features = {
    --   large_buf = { size = 1024 * 256, lines = 10000 }, -- set global limits for large files for disabling features like treesitter
    --   autopairs = true, -- enable autopairs at start
    --   cmp = true, -- enable completion at start
    --   diagnostics = { virtual_text = true, virtual_lines = false }, -- diagnostic settings on startup
    --   highlighturl = true, -- highlight URLs at start
    --   notifications = true, -- enable notifications at start
    -- },
    -- -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
    -- diagnostics = {
    --   virtual_text = true,
    --   underline = true,
    -- },
    -- passed to `vim.filetype.add`
    -- filetypes = {
    --   -- see `:h vim.filetype.add` for usage
    --   extension = {
    --     foo = "fooscript",
    --   },
    --   filename = {
    --     [".foorc"] = "fooscript",
    --   },
    --   pattern = {
    --     [".*/etc/foo/.*"] = "fooscript",
    --   },
    -- },
    -- vim options can be configured here
    options = {
      opt = { -- vim.opt.<key>
        number = true, -- sets vim.opt.number
        relativenumber = false, -- sets vim.opt.relativenumber
        spell = true, -- sets vim.opt.spell
        signcolumn = "yes", -- sets vim.opt.signcolumn to yes
        wrap = false, -- sets vim.opt.wrap
      },
      -- g = { -- vim.g.<key>
      --   -- configure global vim variables (vim.g)
      --   -- NOTE: `mapleader` and `maplocalleader` must be set in the AstroNvim opts or before `lazy.setup`
      --   -- This can be found in the `lua/lazy_setup.lua` file
      -- },
    },
  },
}
