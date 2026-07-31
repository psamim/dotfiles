return {
  "dmtrKovalenko/fff.nvim",
  build = function() require("fff.download").download_or_build_binary() end,
  lazy = false,
  opts = {
    layout = {
      width = 1,
      height = 1,
      prompt_position = "bottom",
      preview_position = "top",
      -- preview_size = 0.35,
      path_shorten_strategy = "middle_number",
    },
    preview = {
      enabled = true,
    },
  },
}
