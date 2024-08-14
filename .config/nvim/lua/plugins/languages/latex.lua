return {
  {
    'lervag/vimtex',
    ft = { 'tex' },
    init = function()
      -- VimTeX configuration goes here, e.g.
      vim.g.vimtex_view_method = 'zathura'
    end,
    config = function()
      vim.g.vimtex_mappings_disable = { ['n'] = { 'K' } } -- disable `K` as it conflicts with LSP hover
      vim.g.vimtex_quickfix_method = vim.fn.executable 'pplatex' == 1 and 'pplatex' or 'latexlog'
    end,
  },
  {
    'folke/which-key.nvim',
    optional = true,
    opts = {
      defaults = {
        ['<localLeader>l'] = { name = '+[L]atex' },
      },
    },
  },

  -- Correctly setup lspconfig for LaTeX 🚀
  {
    'neovim/nvim-lspconfig',
    optional = true,
    opts = {
      servers = {
        texlab = {
          keys = {
            { '<Leader>K', '<plug>(vimtex-doc-package)', desc = 'Vimtex Docs', silent = true },
          },
        },
      },
    },
  },
  {
    'williamboman/mason.nvim',
    dependencies = { 'williamboman/mason-tool-installer.nvim' },
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { 'texlab' })
      require('mason-tool-installer').setup { ensure_installed = opts.ensure_installed }
    end,
  },
}
