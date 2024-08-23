return {
  'folke/zen-mode.nvim',
  dependencies = {
    {
      'folke/twilight.nvim',
      cmd = { 'Twilight', 'TwilightEnable', 'TwilightDisable' },
      opts = {
        context = 3,
      },
    },
  },
  -- cmd = { 'ZenMode' },
  keys = {
    {
      '<leader>tz',
      '<CMD>ZenMode<CR>',
      desc = '[T]oggle [Z]en',
      mode = 'n',
    },
  },
}
