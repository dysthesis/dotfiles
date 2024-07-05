return { -- Colourscheme
  'catppuccin/nvim',
  name = 'catppuccin',
  enabled = true,
  priority = 1000,
  config = function()
    require('catppuccin').setup {
      transparent_background = true,
      color_overrides = { all = {
        text = '#ffffff',
        surface0 = '#181825',
      } },
    }
    vim.cmd.colorscheme 'catppuccin'
  end,
}
