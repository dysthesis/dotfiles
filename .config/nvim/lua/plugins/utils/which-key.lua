return { -- Useful plugin to show you pending keybinds.
  'folke/which-key.nvim',
  keys = { { '<leader>' } },
  config = function() -- This is the function that runs, AFTER loading
    require('which-key').setup()
  end,
}
