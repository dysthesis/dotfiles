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

  -- {
  --   'lukas-reineke/headlines.nvim',
  --   ft = { 'markdown' },
  --   config = function(_, opts)
  --     -- PERF: schedule to prevent headlines slowing down opening a file
  --     vim.schedule(function()
  --       require('headlines').setup(opts)
  --       require('headlines').refresh()
  --     end)
  --   end,
  -- },
  --
  {
    'MeanderingProgrammer/markdown.nvim',
    ft = { 'markdown' },
    main = "render-markdown",
    name = 'render-markdown', -- Only needed if you have another plugin named markdown.nvim
    dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' },
    opts = {
      heading = {
        icons = { ' 󰫈 ', ' 󰫇 ', ' 󰫆 ', ' 󰫅 ', ' 󰫄 ', ' 󰫃 ' },
      },
      bullet = {
        right_pad = 1
      },
      code = {
        right_pad = 3,
        left_pad = 3
      }
    }
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
