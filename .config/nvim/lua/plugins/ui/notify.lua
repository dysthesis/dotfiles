return {
  'rcarriga/nvim-notify',
  config = function()
    require('notify').setup {
      fps = 165,
      background_colour = '#000000',
    }
  end,
}
