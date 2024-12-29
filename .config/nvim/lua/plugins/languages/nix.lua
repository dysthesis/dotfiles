return {
  {
    'williamboman/mason.nvim',
    dependencies = {
      'WhoIsSethDaniel/mason-tool-installer.nvim',
      opts = function(_, opts)
        require('mason-tool-installer').setup { ensure_installed = opts.ensure_installed }
      end,
    },
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { 'nixd', 'alejandra' })
    end,
  },
  {
    'neovim/nvim-lspconfig',
    optional = true,
    config = function()
      require('lspconfig').nixd.setup {
        cmd = { 'nixd' },
        settings = {
          nixd = {
            nixpkgs = {
              expr = 'import <nixpkgs> { }',
            },
            formatting = {
              command = { 'alejandra' }, -- or nixfmt or nixpkgs-fmt
            },
            -- options = {
            --   nixos = {
            --       expr = '(builtins.getFlake "/PATH/TO/FLAKE").nixosConfigurations.CONFIGNAME.options',
            --   },
            --   home_manager = {
            --       expr = '(builtins.getFlake "/PATH/TO/FLAKE").homeConfigurations.CONFIGNAME.options',
            --   },
            -- },
          },
        },
      }
    end,
  },
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { 'nix' })
    end,
  },
}
