return {
  {
    'RaafatTurki/hex.nvim',
    config = function()
      require('hex').setup()
    end,
    keys = {
      { '<leader>th', '<CMD>HexToggle<CR>', desc = '[T]oggle [H]ex', mode = 'n' },
    },
  },
}
