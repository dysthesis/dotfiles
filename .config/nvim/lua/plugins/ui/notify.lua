return {
  'rcarriga/nvim-notify',
  enabled = false,
  config = function()
    require('notify').setup {
      fps = 165,
      background_colour = '#000000',
    }
  end,
}
