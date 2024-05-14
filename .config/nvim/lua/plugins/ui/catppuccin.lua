return { -- Colourscheme
  'catppuccin/nvim',
  name = 'catppuccin',
  priority = 1000,
  config = function()
    require('catppuccin').setup {
      transparent_background = true,
      color_overrides = {
        all = {
          text = '#ffffff',
        },
      },
    }
    vim.cmd.colorscheme 'catppuccin'
  end,
}
