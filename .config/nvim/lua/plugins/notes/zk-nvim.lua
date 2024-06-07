return {
  'zk-org/zk-nvim',
  keys = {
    { '<leader>nn', "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>", desc = 'Zk New', mode = 'n' },
    { '<leader>nn', '<Cmd>ZkNewFromTitleSelection<CR>', desc = 'Zk New', mode = 'v' },
    { '<leader>no', "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", desc = 'Zk Notes' },
    { '<leader>nt', '<Cmd>ZkTags<CR>', desc = 'Zk Tags' },
    { '<leader>ng', ":'<,'>ZkMatch<CR>", mode = 'x', desc = 'Zk Match' },
    { '<leader>nb', '<Cmd>ZkBacklinks<CR>', desc = 'Zk Backlinks' },
    { '<leader>nl', '<Cmd>ZkLinks<CR>', desc = 'Zk Links' },
    { '<localleader>nf', '<cmd>ZkNotes<CR>', desc = 'Zk notes' },
  },
  config = function()
    require('zk').setup {
      picker = 'telescope',
    }
  end,
}
