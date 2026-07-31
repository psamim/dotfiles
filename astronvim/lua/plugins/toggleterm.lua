---@type LazySpec
return {
  "akinsho/toggleterm.nvim",
  opts = {
    float_opts = {
      width = function() return vim.o.columns end,
      height = function() return vim.o.lines - vim.o.cmdheight - 2 end,
    },
  },
}
