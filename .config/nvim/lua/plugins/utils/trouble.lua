return {
  'folke/trouble.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {
    action_keys = {
      toggle_fold = { '<Space>' },
    },
    signs = {
      error = ' ',
      warning = ' ',
      hint = ' ',
      information = ' ',
    },
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  },
  keys = {
    {
      '<leader>cd',
      '<cmd>Trouble diagnostics toggle<cr>',
      desc = '[C]ode [D]iagnostics (Trouble)',
    },
    {
      '<leader>xX',
      '<cmd>Trouble diagnostics toggle filter.buf=0<cr>',
      desc = 'Buffer Diagnostics (Trouble)',
    },
    {
      '<leader>cs',
      '<cmd>Trouble symbols toggle focus=false<cr>',
      desc = '[C]ode [S]ymbols (Trouble)',
    },
    {
      '<leader>cl',
      '<cmd>Trouble lsp toggle focus=false win.position=right<cr>',
      desc = 'LSP Definitions / references / ... (Trouble)',
    },
    {
      '<leader>cL',
      '<cmd>Trouble loclist toggle<cr>',
      desc = 'Location List (Trouble)',
    },
    {
      '<leader>cq',
      '<cmd>Trouble qflist toggle<cr>',
      desc = '[C]ode [Q]uickfix List (Trouble)',
    },
  },
}
