---@type LazySpec
return {
  -- use mason-tool-installer for automatically installing Mason packages
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    -- overrides `require("mason-tool-installer").setup(...)`
    opts = {
      -- Make sure to use the names found in `:Mason`
      ensure_installed = {
        -- install language servers
        "lua-language-server",

        -- install formatters
        "stylua",

        -- install debuggers
        -- "debugpy",

        -- install any other package
        -- "tree-sitter-cli",
        -- NOTE: prettier intentionally NOT installed here. Projects pin their own
        -- prettier (e.g. personio-web -> 2.8.8); a Mason global (3.x) drifts and
        -- fights the project's eslint-plugin-prettier. none-ls uses only_local.
        "eslint-lsp",
      },
    },
  },
}
