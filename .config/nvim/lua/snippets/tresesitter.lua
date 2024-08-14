local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

ls.add_snippets("lua", {
  s("treesitter-block", {
    t({
      "{",
      "    'nvim-treesitter/nvim-treesitter',",
      "    opts = function(_, opts)",
      "      opts.ensure_installed = opts.ensure_installed or {}",
      "      vim.list_extend(opts.ensure_installed, { '",
    }),
    i(1,), -- Cursor starts here
    t({
      "' })",
      "    end,",
      "  },",
    }),
  }),
})

