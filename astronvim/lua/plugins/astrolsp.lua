-- AstroLSP allows you to customize the features in AstroNvim's LSP configuration engine
-- Configuration documentation can be found with `:h astrolsp`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrolsp",
  ---@type AstroLSPOpts
  opts = {
    -- Configuration table of features provided by AstroLSP
    features = {
      codelens = true, -- enable/disable codelens refresh on start
      inlay_hints = false, -- enable/disable inlay hints on start
      semantic_tokens = true, -- enable/disable semantic token highlighting
    },
    -- customize lsp formatting options
    formatting = {
      -- control auto formatting on save
      format_on_save = {
        enabled = true, -- enable or disable format on save globally
        allow_filetypes = { -- enable format on save for specified filetypes only
          -- "go",
        },
        ignore_filetypes = { -- disable format on save for specified filetypes
          -- "python",
          -- "javascript",
          -- "javascriptreact",
          -- "typescript",
          -- "typescriptreact",
        },
      },
      disabled = { -- disable formatting capabilities for the listed language servers
        -- disable lua_ls formatting capability if you want to use StyLua to format your lua code
        -- "lua_ls",
        -- vtsls (the TS/JS language server from astrocommunity.pack.typescript) has
        -- its own built-in formatter with different defaults than the project's
        -- Prettier/ESLint config. With format_on_save enabled globally, it would
        -- run alongside the eslint_fix_on_save autocmd below and clobber its
        -- output, reformatting files in a way that fails `eslint`/CI lint.
        -- eslint's fixAll (which enforces Prettier via eslint-plugin-prettier)
        -- must be the only formatter touching these filetypes.
        "vtsls",
      },
      timeout_ms = 1000, -- default format timeout
      -- filter = function(client) -- fully override the default formatting function
      --   return true
      -- end
    },
    -- enable servers that you already have installed without mason
    servers = {
      -- "pyright"
    },
    -- customize language server configuration passed to `vim.lsp.config`
    -- client specific configuration can also go in `lsp/` in your configuration root (see `:h lsp-config`)
    config = {
      -- ["*"] = { capabilities = {} }, -- modify default LSP client settings such as capabilities
    },
    -- customize how language servers are attached
    handlers = {
      -- a function with the key `*` modifies the default handler, functions takes the server name as the parameter
      -- ["*"] = function(server) vim.lsp.enable(server) end

      -- the key is the server that is being setup with `vim.lsp.config`
      -- rust_analyzer = false, -- setting a handler to false will disable the set up of that language server
    },
    -- Configure buffer local auto commands to add when attaching a language server
    autocmds = {
      -- Run `eslint --fix` on save for buffers the eslint LSP attaches to
      -- (js/ts/tsx/...). In personio-web prettier is enforced via
      -- eslint-plugin-prettier, so eslint's autofix IS the formatter and matches
      -- CI lint exactly. Synchronous (buf_request_sync) so the edits are applied
      -- BEFORE the file is written — avoids the async "save writes stale buffer" bug.
      eslint_fix_on_save = {
        cond = function(client) return client.name == "eslint" end,
        {
          event = "BufWritePre",
          desc = "ESLint --fix (incl. prettier/prettier) on save",
          callback = function(args)
            local bufnr = args.buf
            local clients = vim.lsp.get_clients { bufnr = bufnr, name = "eslint" }
            if #clients == 0 then return end
            local client = clients[1]
            local enc = client.offset_encoding or "utf-16"
            -- Ask eslint for its "fix all" code action and apply the returned edit
            -- SYNCHRONOUSLY, so the fixes land in the buffer before it is written.
            -- (executeCommand("eslint.applyAllFixes") returns before its async
            -- workspace/applyEdit arrives, which raced the write — hence the flicker.)
            local params = vim.lsp.util.make_range_params(0, enc)
            params.context = { only = { "source.fixAll.eslint" }, diagnostics = {} }
            local res = vim.lsp.buf_request_sync(bufnr, "textDocument/codeAction", params, 3000)
            if not res then return end
            for _, r in pairs(res) do
              for _, action in pairs(r.result or {}) do
                if action.edit then vim.lsp.util.apply_workspace_edit(action.edit, enc) end
              end
            end
          end,
        },
      },
      -- first key is the `augroup` to add the auto commands to (:h augroup)
      lsp_codelens_refresh = {
        -- Optional condition to create/delete auto command group
        -- can either be a string of a client capability or a function of `fun(client, bufnr): boolean`
        -- condition will be resolved for each client on each execution and if it ever fails for all clients,
        -- the auto commands will be deleted for that buffer
        cond = "textDocument/codeLens",
        -- cond = function(client, bufnr) return client.name == "lua_ls" end,
        -- list of auto commands to set
        {
          -- events to trigger
          event = { "InsertLeave", "BufEnter" },
          -- the rest of the autocmd options (:h nvim_create_autocmd)
          desc = "Refresh codelens (buffer)",
          callback = function(args)
            if require("astrolsp").config.features.codelens then vim.lsp.codelens.enable(true, { bufnr = args.buf }) end
          end,
        },
      },
    },
    -- mappings to be set up on attaching of a language server
    mappings = {
      n = {
        -- a `cond` key can provided as the string of a server capability to be required to attach, or a function with `client` and `bufnr` parameters from the `on_attach` that returns a boolean
        gD = {
          function() vim.lsp.buf.declaration() end,
          desc = "Declaration of current symbol",
          cond = "textDocument/declaration",
        },
        ["<Leader>uY"] = {
          function() require("astrolsp.toggles").buffer_semantic_tokens() end,
          desc = "Toggle LSP semantic highlight (buffer)",
          cond = function(client)
            return client:supports_method "textDocument/semanticTokens/full" and vim.lsp.semantic_tokens ~= nil
          end,
        },
      },
    },
    -- A custom `on_attach` function to be run after the default `on_attach` function
    -- takes two parameters `client` and `bufnr`  (`:h lsp-attach`)
    on_attach = function(client, bufnr)
      -- this would disable semanticTokensProvider for all clients
      -- client.server_capabilities.semanticTokensProvider = nil
    end,
  },
}
