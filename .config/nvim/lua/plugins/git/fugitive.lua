return {
  'tpope/vim-fugitive',
  dependencies = {
    {
      'tpope/vim-rhubarb',
      keys = {
        -- Open GitHub source
        { '<Leader>gh', '<cmd>GBrowse!<CR>:GBrowse<CR>' },
        { '<Leader>gh', '<cmd>GBrowse!<CR>gv:GBrowse<CR>', mode = 'v' },
      },
    },
  },
  keys = {
    -- Write to a git staging area
    { '<leader>gw', ':Git<CR>' },
    { '<leader>gb', ':Gblame<CR>' },
  },
}
