return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
  },
  opts = {
    menu = {
      width = vim.api.nvim_win_get_width(0) - 4,
    },
    settings = {
      save_on_toggle = true,
    },
  },
  -- config = function()
  --   local harpoon = require 'harpoon'
  --   harpoon:setup {}
  --
  --   -- basic telescope configuration
  --   local conf = require('telescope.config').values
  --   local function toggle_telescope(harpoon_files)
  --     local file_paths = {}
  --     for _, item in ipairs(harpoon_files.items) do
  --       table.insert(file_paths, item.value)
  --     end
  --
  --     require('telescope.pickers')
  --       .new({}, {
  --         prompt_title = 'Harpoon',
  --         finder = require('telescope.finders').new_table {
  --           results = file_paths,
  --         },
  --         previewer = conf.file_previewer {},
  --         sorter = conf.generic_sorter {},
  --       })
  --       :find()
  --   end
  --
  --   vim.keymap.set('n', '<leader>hL', function()
  --     toggle_telescope(harpoon:list())
  --   end, { desc = '[H]arpoon [L]ist (Telescope)' })
  -- end,
  keys = function()
    local keys = {
      {
        '<leader>H',
        function()
          require('harpoon'):list():add()
        end,
        desc = 'Harpoon File',
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

    for i = 1, 5 do
      table.insert(keys, {
        '<leader>' .. i,
        function()
          require('harpoon'):list():select(i)
        end,
        desc = 'Harpoon to File ' .. i,
      })
    end
    return keys
  end,
}
