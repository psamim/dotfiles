return {
  "folke/snacks.nvim",
  opts = {
    picker = {
      layout = { preset = "ivy_split" },
      formatters = {
        file = {
          truncate = "center",
          min_width = 100,
          filename_first = true,
        },
      },
    },
  },
}
