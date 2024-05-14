return {
  { -- A statusline for neovim
    'nvim-lualine/lualine.nvim',
    event = 'VeryLazy',
    enabled = true,
    dependencies = {
      'nvim-tree/nvim-web-devicons',
    },
    init = function()
      vim.opt.laststatus = 0
    end,
    config = function()
      vim.opt.laststatus = 3
      local lualine = require 'lualine'
      lualine.setup {
        options = {
          component_separators = '',
          section_separators = { left = '█', right = '█' },
          disabled_filetypes = { 'alpha' },
        },
        sections = {
          lualine_a = {
            {
              'mode',
              fmt = fmt_mode,
              icon = { '' },
              separator = { left = '█', right = '█' },
              right_padding = 2,
            },
          },
          lualine_b = {
            'branch',
            {
              'diff',
              color = text_hl,
              icon = { ' ', color = text_hl },
              source = diff_source,
              symbols = { added = ' ', modified = ' ', removed = ' ' },
              diff_color = { added = icon_hl, modified = icon_hl, removed = icon_hl },
            },
            'diagnostics',
          },
          lualine_c = {
            {
              'filename',
              icon = { '  ', color = icon_hl },
              color = text_hl,
            },
          },
          lualine_x = {
            'filetype',
          },
          lualine_z = {
            {
              'location',
              icon = { '', align = 'left' },
            },
            {
              'progress',
              icon = { '', align = 'left' },
              separator = { right = '█', left = '█' },
            },
          },
        },
        extensions = { 'nvim-tree', 'fzf' },
      }
    end,
  },
}
