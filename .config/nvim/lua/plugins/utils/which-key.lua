return { -- Useful plugin to show you pending keybinds.
  'folke/which-key.nvim',
  keys = {
    { '<leader>' },
    {
      '<leader>?',
      function()
        require('which-key').show { global = false }
      end,
      desc = 'Buffer Local Keymaps (which-key)',
    },
  },
  config = function()
    local wk = require 'which-key'
    wk.add {
      { '<leader>n', group = '[N]ote', icon = { icon = ' ', color = 'purple' } },
      { '<leader>c', group = '[C]ode', icon = { icon = ' ', color = 'blue' } },
      { '<leader>f', group = '[F]ind', icon = { icon = ' ', color = 'cyan' } },
      { '<leader>g', group = '[G]it', icon = { icon = ' ', color = 'red' } },
      { '<leader>h', group = '[H]arpoon', icon = { icon = '󱡀 ', color = 'azure' } },
      { '<leader>s', group = '[S]how', icon = { icon = ' ', color = 'yellow' } },
      { '<leader>t', group = '[T]oggle', icon = { icon = ' ', color = 'green' } },
      { '<leader>d', group = '[D]ebug', icon = { icon = ' ', color = 'orange' } },
    }
  end,
}
