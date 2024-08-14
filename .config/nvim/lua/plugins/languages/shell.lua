return {
  {
    'williamboman/mason.nvim',
    dependencies = {
      'williamboman/mason-tool-installer.nvim',
      opts = function(_, opts)
        require('mason-tool-installer').setup { ensure_installed = opts.ensure_installed }
      end,
    },
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { 'shfmt', 'shellcheck', 'bash-language-server' })
    end,
  },
}
