return {
  --   -- Stolen from https://github.com/LazyVim/LazyVim/blob/530e94a9fa19577401e968a9673282c3d79f01e3/lua/lazyvim/plugins/extras/lang/haskell.lua
  --   -- Add Haskell to treesitter
  --   {
  --     'nvim-treesitter/nvim-treesitter',
  --     opts = function(_, opts)
  --       if type(opts.ensure_installed) == 'table' then
  --         vim.list_extend(opts.ensure_installed, { 'haskell' })
  --       end
  --     end,
  --   },
  --
  --   {
  --     'mrcjkb/haskell-tools.nvim',
  --     version = '^3',
  --     ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
  --     dependencies = {
  --       { 'nvim-telescope/telescope.nvim', optional = true },
  --     },
  --     keys = function()
  --       local ht = require 'haskell-tools'
  --       local bufnr = vim.api.nvim_get_current_buf()
  --       local opts = { noremap = true, silent = true, buffer = bufnr }
  --       return {
  --         { '<leader>rcl', vim.lsp.codelens.run, desc = '[R]un [C]ode [L]ens',           opts },
  --         -- { '<leader>hs', ht.hoogle.hoogle_signature, desc = '[H]oogle [S]ignature', opts },
  --         { '<leader>ea',  ht.lsp.buf_eval_all,  desc = '[E]valuate [A]ll snippets',     opts },
  --         { '<leader>trp', ht.repl.toggle,       desc = '[T]oggle [R]epl for [P]ackage', opts },
  --         {
  --           '<leader>trb',
  --           function()
  --             ht.repl.toggle(vim.api.nvim_buf_get_name(0))
  --           end,
  --           desc = '[T]oggle [R]epl for [B]uffer',
  --           opts,
  --         },
  --         { '<leader>rq', ht.repl.quit, desc = '[R]epl [Q]uit', opts },
  --       }
  --     end,
  --   },
  --
  --   {
  --     'williamboman/mason.nvim',
  --     opts = function(_, opts)
  --       opts.ensure_installed = opts.ensure_installed or {}
  --       vim.list_extend(opts.ensure_installed, { 'haskell-language-server' })
  --     end,
  --   },
  --
  --   {
  --     'mfussenegger/nvim-dap',
  --     optional = true,
  --     dependencies = {
  --       {
  --         'williamboman/mason.nvim',
  --         dependencies = { 'williamboman/mason-tool-installer.nvim' },
  --         opts = function(_, opts)
  --           opts.ensure_installed = opts.ensure_installed or {}
  --           vim.list_extend(opts.ensure_installed, { 'haskell-debug-adapter' })
  --           require('mason-tool-installer').setup { ensure_installed = opts.ensure_installed }
  --         end,
  --       },
  --     },
  --   },
  --
  --   -- {
  --   --   'nvim-neotest/neotest',
  --   --   optional = true,
  --   --   dependencies = {
  --   --     { 'mrcjkb/neotest-haskell' },
  --   --   },
  --   --   opts = {
  --   --     adapters = {
  --   --       ['neotest-haskell'] = {},
  --   --     },
  --   --   },
  --   -- },
  --
  --   {
  --     'mrcjkb/haskell-snippets.nvim',
  --     dependencies = { 'L3MON4D3/LuaSnip' },
  --     ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
  --     config = function()
  --       local haskell_snippets = require('haskell-snippets').all
  --       require('luasnip').add_snippets('haskell', haskell_snippets, { key = 'haskell' })
  --     end,
  --   },
  --
  --   -- {
  --   --   'luc-tielen/telescope_hoogle',
  --   --   ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
  --   --   dependencies = {
  --   --     { 'nvim-telescope/telescope.nvim' },
  --   --   },
  --   --   config = function()
  --   --     local ok, telescope = pcall(require, 'telescope')
  --   --     if ok then
  --   --       telescope.load_extension 'hoogle'
  --   --     end
  --   --   end,
  --   -- },
  --
  --   -- Make sure lspconfig doesn't start hls,
  --   -- as it conflicts with haskell-tools
  --   {
  --     'neovim/nvim-lspconfig',
  --     opts = {
  --       setup = {
  --         hls = function()
  --           return true
  --         end,
  --       },
  --     },
  --   },
}
