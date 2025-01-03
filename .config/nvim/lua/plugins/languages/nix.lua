return {
  {
    'neovim/nvim-lspconfig',
    optional = true,
    config = function()
      require('lspconfig').nixd.setup {
        cmd = { 'nix run nixpkgs#nixd' },
        settings = {
          nixd = {
            nixpkgs = {
              expr = 'import <nixpkgs> { }',
            },
            formatting = {
              command = { 'alejandra' }, -- or nixfmt or nixpkgs-fmt
            },
            options = {
              nixos = {
                expr = '(builtins.getFlake "/home/demiurge/Documents/Projects/laplace/").nixosConfigurations.yaldabaoth.options',
              },
            },
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
