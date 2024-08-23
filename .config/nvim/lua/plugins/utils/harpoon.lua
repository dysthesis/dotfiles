return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = {
    'nvim-lua/plenary.nvim',
    -- 'nvim-telescope/telescope.nvim',
  },
  opts = {
    menu = {
      width = vim.api.nvim_win_get_width(0) - 4,
    },
    settings = {
      save_on_toggle = true,
    },
  },

  keys = function()
    local keys = {
      {
        '<leader>H',
        function()
          require('harpoon'):list():add()
        end,
        desc = '[H]arpoon File',
      },
      {
        '<leader>hl',
        function()
          local harpoon = require 'harpoon'
          harpoon.ui:toggle_quick_menu(harpoon:list())
        end,
        desc = '[H]arpoon [L]ist (Quick)',
      },
    }

    for i = 1, 9 do
      table.insert(keys, {
        '<leader>' .. i,
        function()
          require('harpoon'):list():select(i)
        end,
        desc = 'Harpoon to file [' .. i .. ']',
      })
    end
    return keys
  end,
}
