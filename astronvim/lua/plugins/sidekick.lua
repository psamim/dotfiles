return {
  "folke/sidekick.nvim",
  opts = {
    nes = {
      diff = {
        show = "cursor", -- only show inline diff at cursor, less distracting
      },
    },
    cli = {
      mux = {
        enabled = true,
        create = "split",
        split = {
          vertical = true,
          size = 0.4,
        },
        dump = 5000, -- capture more scrollback history (default: 2000)
      },
      -- Custom context: {git_diff} sends staged+unstaged diff to Claude
      context = {
        git_diff = function(ctx)
          local result = vim.system({ "git", "-C", ctx.cwd, "diff", "HEAD" }):wait()
          local out = (result.stdout or ""):gsub("%s+$", "")
          if out == "" then return false end
          return "```diff\n" .. out .. "\n```"
        end,
      },
      prompts = {
        -- Git workflows
        commit = "Write a conventional commit message for these changes:\n{git_diff}",
        pr_description = "Write a pull request description for these changes:\n{git_diff}",
        -- Code workflows
        type_error = "Fix the TypeScript error at {position}:\n{diagnostics}",
        refactor = "Refactor {this} to be cleaner and more maintainable",
        -- Claude-code specific
        plan = "Before making any changes, write a brief plan for how you would implement: ",
      },
    },
  },
  -- Extra keymaps on top of the community plugin mappings
  specs = {
    {
      "AstroNvim/astrocore",
      opts = function(_, opts)
        local maps = opts.mappings
        local prefix = "<Leader>A"

        maps.n[prefix .. "g"] = {
          function() require("sidekick.cli").send { msg = "{git_diff}" } end,
          desc = "Send Git Diff",
        }
        maps.n[prefix .. "c"] = {
          function() require("sidekick.cli").send { prompt = "commit" } end,
          desc = "Write Commit Message",
        }
        maps.n[prefix .. "r"] = {
          function() require("sidekick.cli").send { msg = "{git_diff}", prompt = "pr_description" } end,
          desc = "Write PR Description",
        }
        maps.n[prefix .. "e"] = {
          function() require("sidekick.cli").send { prompt = "type_error" } end,
          desc = "Fix Type Error at Cursor",
        }
      end,
    },
  },
}
