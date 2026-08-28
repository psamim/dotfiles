---@type LazySpec
return {
  "nvimtools/none-ls.nvim",
  opts = function(_, opts)
    local null_ls = require "null-ls"
    local prettier_disabled_root_prefixes = {}
    local exclusions_file = vim.fn.stdpath "config" .. "/lua/plugins/.prettier-disabled-root-prefixes"
    if vim.fn.filereadable(exclusions_file) == 1 then
      for _, line in ipairs(vim.fn.readfile(exclusions_file)) do
        line = vim.fn.expand(vim.trim(line))
        if line ~= "" and not vim.startswith(line, "#") then
          table.insert(prettier_disabled_root_prefixes, line)
        end
      end
    end

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
        -- Prefer a project-local Prettier, but fall back to the system
        -- formatter for standalone files without a package.json.
        prefer_local = "node_modules/.bin",
        condition = function(utils)
          local root = vim.fs.normalize(utils.root())
          for _, prefix in ipairs(prettier_disabled_root_prefixes) do
            prefix = vim.fs.normalize(prefix):gsub("/$", "")
            if root == prefix or vim.startswith(root, prefix .. "/") then return false end
          end
          return true
        end,
      },
    })
  end,
}
