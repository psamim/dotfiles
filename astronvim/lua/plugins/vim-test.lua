-- ~/.config/nvim/lua/plugins/vim-test.lua
return {
  "vim-test/vim-test",
  config = function()
    vim.g["test#javascript#runner"] = "jest"
    vim.g["test#javascript#jest#executable"] = "pnpm exec jest"
    vim.g["test#strategy"] = "neovim"
    vim.g["test#echo_command"] = 1

    -- vim.g["test#javascript#jest#options"] = "-c jest.config.ts"
    -- vim.g["test#javascript#jest#file_pattern"] = [[\v(test|spec)\.(js|jsx|coffee|ts|tsx)$]]
    -- vim.g["test#javascript#jest#file_pattern"] = [[\.(test|spec)\.(js|jsx|ts|tsx)$]]
    -- Force vim-test to recognize your file as a Jest test
    -- vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
    --   pattern = { "*.test.tsx", "*.spec.tsx" },
    --   callback = function() vim.b.test_runner = "jest" end,
    -- })
  end,
}
