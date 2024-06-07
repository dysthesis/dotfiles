return {
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      if type(opts.ensure_installed) == 'table' then
        vim.list_extend(opts.ensure_installed, { 'markdown', 'markdown_inline' })
      end
    end,
  },
  -- {
  --   'williamboman/mason.nvim',
  --   opts = function(_, opts)
  --     opts.ensure_installed = opts.ensure_installed or {}
  --     vim.list_extend(opts.ensure_installed, { 'markdownlint', 'marksman' })
  --   end,
  -- },
  -- {
  --   'nvimtools/none-ls.nvim',
  --   optional = true,
  --   opts = function(_, opts)
  --     local nls = require 'null-ls'
  --     opts.sources = vim.list_extend(opts.sources or {}, {
  --       nls.builtins.diagnostics.markdownlint,
  --     })
  --   end,
  -- },
  -- {
  --   'mfussenegger/nvim-lint',
  --   optional = true,
  --   opts = {
  --     linters_by_ft = {
  --       markdown = { 'markdownlint' },
  --     },
  --   },
  -- },
  -- {
  --   'neovim/nvim-lspconfig',
  --   opts = {
  --     servers = {
  --       marksman = {},
  --     },
  --   },
  -- },

  -- Markdown preview
  {
    'iamcco/markdown-preview.nvim',
    cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview', 'MarkdownPreviewStop' },
    build = function()
      vim.fn['mkdp#util#install']()
    end,
    keys = {
      {
        '<leader>cp',
        ft = 'markdown',
        '<cmd>MarkdownPreviewToggle<cr>',
        desc = 'Markdown Preview',
      },
    },
    config = function()
      vim.cmd [[do FileType]]
    end,
  },

  {
    'lukas-reineke/headlines.nvim',
    ft = { 'markdown' },
    config = function(_, opts)
      -- PERF: schedule to prevent headlines slowing down opening a file
      vim.schedule(function()
        require('headlines').setup(opts)
        require('headlines').refresh()
      end)
    end,
  },
  -- {
  --   'jakewvincent/mkdnflow.nvim',
  --   config = function()
  --     require('mkdnflow').setup {
  --       -- Config goes here; leave blank for defaults
  --       modules = {
  --         cmp = true,
  --         -- bib = false,
  --       },
  --       links = {
  --         transform_explicit = function(text)
  --           -- text = text:gsub(" ", "-")
  --           -- text = text:lower()
  --           -- text = os.date('%Y-%m-%d_')..text
  --           return text
  --         end,
  --         create_on_follow_failure = true,
  --       },
  --     }
  --   end,
  -- },
}
