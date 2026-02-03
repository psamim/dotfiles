-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  -- { import = "astrocommunity.completion.copilot-lua-cmp" },
  { import = "astrocommunity.completion.copilot-vim" },
  { import = "astrocommunity.programming-language-support.csv-vim" },
  -- { import = "astrocommunity.completion.avante-nvim" },
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.pack.php" },
  { import = "astrocommunity.pack.kotlin" },
  -- { import = "astrocommunity.pack.prettier" },
  { import = "astrocommunity.pack.eslint" },
  { import = "astrocommunity.test.vim-test" },
  { import = "astrocommunity.utility.noice-nvim" },
  { import = "astrocommunity.colorscheme.catppuccin" },
  { import = "astrocommunity.motion.nvim-surround" },
  -- { import = "astrocommunity.scrolling.satellite-nvim" },
  { import = "astrocommunity.scrolling.mini-animate" },
  { import = "astrocommunity.diagnostics.tiny-inline-diagnostic-nvim" },
  -- { import = "astrocommunity.editing-support.nvim-treesitter-context" },
  { import = "astrocommunity.git.gitlinker-nvim" },
  { import = "astrocommunity.ai.sidekick-nvim" },
  -- import/override with your plugins folder
}
