---@type LazySpec
return {
  "nvimtools/none-ls.nvim",
  opts = function(_, opts)
    local null_ls = require "null-ls"

    -- Check supported formatters and linters
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/formatting
    -- https://github.com/nvimtools/none-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics

    -- Only insert new sources, do not replace the existing ones
    -- (If you wish to replace, use `opts.sources = {}` instead of the `list_insert_unique` function)
    opts.sources = require("astrocore").list_insert_unique(opts.sources, {
      -- Set a formatter
      -- null_ls.builtins.formatting.stylua,
      -- null_ls.builtins.formatting.prettier,
      --
      null_ls.builtins.formatting.prettier.with {
        -- Use ONLY the project-local prettier; never fall back to a global
        -- (e.g. Mason's prettier 3.x), which would drift from the version a
        -- project pins and enforces via eslint-plugin-prettier.
        only_local = "node_modules/.bin",
        -- JS/TS/TSX are formatted via `eslint --fix` on save instead (see the
        -- eslint_fix_on_save autocmd in astrolsp.lua). In repos like personio-web,
        -- prettier is enforced THROUGH eslint-plugin-prettier, so running eslint's
        -- fix is the single source of truth and matches CI lint exactly. Letting
        -- none-ls prettier also touch these would risk a version/format mismatch.
        disabled_filetypes = {
          "javascript",
          "javascriptreact",
          "typescript",
          "typescriptreact",
        },
      },
    })
  end,
}
