local path = vim.fn.expand '~' .. '/Documents/Notes'
return {
  'zk-org/zk-nvim',
  event = {
    -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
    'BufReadPre '
      .. path
      .. '**.md',
    'BufNewFile ' .. path .. '/**.md',
  },
  keys = {
    {
      '<S-CR>',
      '<Cmd>lua vim.lsp.buf.definition()<CR>',
      desc = 'Follow link',
      mode = 'n',
    },
    {
      '<C-CR>',
      "viw:'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<CR>",
      desc = 'New note for word under cursor',
      mode = 'n',
    },
    {
      '<C-CR>',
      ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<CR>",
      desc = 'New note from selection',
      mode = 'v',
    },
    {
      '<leader>nn',
      "<Cmd>ZkNew { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>",
      desc = '[N]ew [N]ote',
      mode = 'n',
    },
    {
      '<leader>nnt',
      ":'<,'>ZkNewFromTitleSelection { dir = vim.fn.expand('%:p:h') }<CR>",
      desc = '[N]ew [N]ote from [T]itle',
      mode = 'v',
    },
    {
      '<leader>nnc',
      ":'<,'>ZkNewFromContentSelection { dir = vim.fn.expand('%:p:h'), title = vim.fn.input('Title: ') }<CR>",
      desc = '[N]ew [N]ote from [C]ontent',
      mode = 'v',
    },
    {
      '<leader>nb',
      '<CMD>ZkBacklinks<CR>',
      desc = '[N]ote [B]acklinks',
      mode = 'n',
    },
    {
      '<leader>nl',
      '<CMD>ZkLinks<CR>',
      desc = '[N]ote [L]inks',
      mode = 'n',
    },
    {
      '<leader>nf',
      '<CMD>ZkNotes<CR>',
      desc = '[N]ote [F]ind',
      mode = 'n',
    },
    {
      '<leader>nt',
      '<CMD>ZkTags<CR>',
      desc = '[N]ote [T]ags',
      mode = 'n',
    },
  },

  config = function()
    require('zk').setup {
      picker = 'fzf_lua',
    }
  end,
}
