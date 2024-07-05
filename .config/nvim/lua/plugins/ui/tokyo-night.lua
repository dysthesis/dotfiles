return {
  'folke/tokyonight.nvim',
  enabled = false,
  dependencies = {
    {
      'nvim-lualine/lualine.nvim',
      optional = true,
      config = function()
        require('lualine').setup {
          options = {
            -- ... your lualine config
            theme = 'tokyonight',
            -- ... your lualine config
          },
        }
      end,
    },
  },
  init = function()
    vim.cmd [[colorscheme tokyonight]]
  end,
  lazy = false,
  priority = 1000,
  opts = {
    transparent = true,
    style = 'night',
  },
}
