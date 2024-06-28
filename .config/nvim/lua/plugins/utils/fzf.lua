return {
  'ibhagwan/fzf-lua',
  -- optional for icon support
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  keys = function()
    local fzf = require 'fzf-lua'
    return {
      { '<leader>ff', fzf.files, desc = '[F]ind [F]iles' },
      { '<leader>fw', fzf.live_grep_native, desc = '[F]ind [W]ord' },
      { '<leader>fq', fzf.quickfix, desc = '[F]ind [Q]uickfix' },
      { '<leader>fa', fzf.lsp_code_actions, desc = '[F]ind [A]ctions' },
      { '<leader>fdd', fzf.lsp_document_diagnostics, desc = '[F]ind [D]iagnostic in [D]ocuments' },
      { '<leader>fdw', fzf.lsp_workspace_diagnostics, desc = '[F]ind [D]iagnostic in [W]orkspace' },
      { '<leader>fgc', fzf.git_commits, desc = '[F]ind [G]it [C]ommits' },
      { '<leader>fgs', fzf.git_status, desc = '[F]ind [G]it [S]tatus' },
    }
  end,
  config = function()
    -- calling `setup` is optional for customization
    require('fzf-lua').setup {
      'fzf-native',
      fzf_colors = true,
      keymap = {
        builtin = {
          ['<M-k>'] = 'preview-page-up',
          ['<M-j>'] = 'preview-page-down',
        },
        fzf = {
          ['alt-k'] = 'preview-page-up',
          ['alt-j'] = 'preview-page-down',
        },
      },
    }
  end,
}
