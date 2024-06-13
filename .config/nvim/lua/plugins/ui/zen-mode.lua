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
      '[T]oggle [Z]en mode',
      mode = 'n',
    },
  },
}
