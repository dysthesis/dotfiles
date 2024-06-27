return {
  'ibhagwan/fzf-lua',
  -- optional for icon support
  cmd = { 'FzflLua' },
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  keys = {
    { '<leader>ff', '<CMD>FzfLua files<CR>', desc = '[F]ind [F]iles' },
    { '<leader>fw', '<CMD>FzfLua live_grep_native<CR>', desc = '[F]ind [W]ord' },
    { '<leader>fq', '<CMD>FzfLua quickfix<CR>', desc = '[F]ind [Q]uickfix' },
    { '<leader>fd', '<CMD>FzfLua lsp_document_diagnostics<CR>', desc = '[F]ind [D]iagnostic' },
    { '<leader>fg', '<CMD>FzfLua git_commits<CR>', desc = '[F]ind [G]it commits' },
  },
  config = function()
    -- calling `setup` is optional for customization
    require('fzf-lua').setup {
      'fzf-native',
      fzf_colors = true,
    }
  end,
}
