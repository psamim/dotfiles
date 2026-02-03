-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  -- AI
  { import = "astrocommunity.completion.copilot-vim" },
  { import = "astrocommunity.programming-language-support.csv-vim" },
  { import = "astrocommunity.ai.sidekick-nvim" },
  -- { import = "astrocommunity.completion.copilot-lua-cmp" },
  -- { import = "astrocommunity.completion.avante-nvim" },

  -- Languages
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.pack.php" },
  { import = "astrocommunity.pack.kotlin" },

  -- Lint & Test
  { import = "astrocommunity.pack.eslint" },
  { import = "astrocommunity.test.vim-test" },
  { import = "astrocommunity.diagnostics.tiny-inline-diagnostic-nvim" },
  -- { import = "astrocommunity.pack.prettier" },

  -- Utility
  { import = "astrocommunity.utility.noice-nvim" },
  { import = "astrocommunity.colorscheme.catppuccin" },
  { import = "astrocommunity.motion.nvim-surround" },
  { import = "astrocommunity.scrolling.mini-animate" },
  { import = "astrocommunity.git.gitlinker-nvim" },
  -- { import = "astrocommunity.scrolling.satellite-nvim" },
  -- { import = "astrocommunity.editing-support.nvim-treesitter-context" },
}
