return { -- Colourscheme
  'catppuccin/nvim',
  name = 'catppuccin',
  enabled = false,
  priority = 1000,
  config = function()
    require('catppuccin').setup {
      transparent_background = true,
      color_overrides = { all = {
        text = '#ffffff',
        surface0 = '#181825',
      } },
      custom_highlights = function(colors)
        return {
          ['@markup.math'] = { fg = colors.mauve },
          ['@function.latex'] = { fg = colors.mauve },
        }
      end,
    }
    vim.cmd.colorscheme 'catppuccin'
  end,
}
