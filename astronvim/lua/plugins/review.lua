-- review.nvim: code review annotations for codediff.nvim, optimized for AI feedback loops
-- https://github.com/georgeguimaraes/review.nvim

---@type LazySpec
return {
  "georgeguimaraes/review.nvim",
  version = "*",
  dependencies = {
    -- pinned: codediff.nvim v2.50.0+ refactored to a typed Path object for
    -- get_paths(), which review.nvim v1.9.1's hooks.lua doesn't handle yet,
    -- crashing on `:Review` (attempt to call method 'gsub' on a table)
    {
      "esmuellert/codediff.nvim",
      version = "v2.49.2",
      -- review.nvim only forwards `readonly` from its own `codediff` opts
      -- table to review.nvim internals; it never calls codediff.setup()
      -- with the rest, so codediff-native options must be set here.
      opts = {
        explorer = { view_mode = "tree" }, -- "list" (flat file list) or "tree" (directory tree)
        history = { view_mode = "tree" }, -- "list" or "tree" for files under commits
      },
    },
    "MunifTanjim/nui.nvim",
  },
  cmd = { "Review" },
  keys = {
    { "<leader>rr", "<cmd>Review<cr>", desc = "Review: open" },
    { "<leader>rg", "<cmd>Review commits<cr>", desc = "Review: commits" },
    { "<leader>rt", "<cmd>Review toggle<cr>", desc = "Review: toggle editable" },
    { "<leader>rs", "<cmd>Review sidekick<cr>", desc = "Review: send to sidekick" },
    { "<leader>rl", "<cmd>Review list<cr>", desc = "Review: list comments" },
  },
  opts = {
    codediff = {
      -- start in edit mode so the <leader>c* comment keymaps below are
      -- registered immediately; review.nvim only binds them when NOT
      -- readonly (readonly mode uses bare i/d/e/c instead). Toggle back
      -- to readonly with <leader>rt if you want buffers non-editable.
      readonly = true,
    },
    keymaps = {
      -- Under the <leader>r review prefix instead of bare <leader>c, since
      -- <leader>c (close buffer) is a common global keymap and racing its
      -- ambiguous-mapping timeout was closing the buffer more often than
      -- not. <leader>rc (open commits picker) is a much rarer accidental
      -- hit while already inside a review session.
      add_comment = "<leader>rcc",
      add_note = "<leader>rcn",
      add_suggestion = "<leader>rcs",
      add_issue = "<leader>rci",
      add_praise = "<leader>rcp",
      delete_comment = "<leader>rcd",
      edit_comment = "<leader>rce",
    },
  },
}
