return {
  {
    'nvim-treesitter/nvim-treesitter',
    opts = function(_, opts)
      if type(opts.ensure_installed) == 'table' then
        vim.list_extend(opts.ensure_installed, { 'markdown', 'markdown_inline' })
      end
    end,
  },
  -- Markdown preview
  {
    'iamcco/markdown-preview.nvim',
    ft = { 'markdown' },
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
    'williamboman/mason.nvim',
    dependencies = { 'WhoIsSethDaniel/mason-tool-installer.nvim' },
    optional = true,
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { 'markdownlint', 'glow' })
      require('mason-tool-installer').setup { ensure_installed = opts.ensure_installed }
    end,
  },
  {
    'OXY2DEV/markview.nvim',
    lazy = false, -- Recommended
    -- ft = "markdown" -- If you decide to lazy-load anyway

    dependencies = {
      -- You will not need this if you installed the
      -- parsers manually
      -- Or if the parsers are in your $RUNTIMEPATH
      'nvim-treesitter/nvim-treesitter',
      'nvim-tree/nvim-web-devicons',
    },
    config = function()
      require('markview').setup {
        modes = { 'n', 'i', 'no', 'c' },
        hybrid_modes = { 'i' },

        -- This is nice to have
        callbacks = {
          on_enable = function(_, win)
            vim.wo[win].conceallevel = 2
            vim.wo[win].concealcursor = 'nc'
          end,
        },
        headings = {
          enable = true,
          shift_width = 0,

          heading_1 = { icon = ' 󰫈 ' },
          heading_2 = { icon = ' 󰫇 ' },
          heading_3 = { icon = ' 󰫆 ' },
          heading_4 = { icon = ' 󰫅 ' },
          heading_5 = { icon = ' 󰫄 ' },
          heading_6 = { icon = ' 󰫃 ' },
        },
        code_blocks = { hl = 'CursorColumn' },
        inline_codes = { hl = 'CursorColumn' },
        list_items = { shift_width = 2 },
      }
    end,
  },
  {
    'ellisonleao/glow.nvim',
    keys = {
      { '<leader>mp', '<CMD>Glow<CR>', desc = '[M]arkdown [P]review', mode = 'n' },
    },
    config = function()
      require('glow').setup {
        style = 'dark',
        width = 120,
        width_ratio = 0.9,
        height_ratio = 0.9,
      }
    end,
  },
}
