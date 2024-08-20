return {
  'danymat/neogen',

  config = function()
    require('neogen').setup { snippet_engine = 'luasnip' }
  end,

  keys = {
    { '<leader>cc', ":lua require('neogen').generate()<CR>", { noremap = true, silent = true } },
  },
  -- Uncomment next line if you want to follow only stable versions
  version = '*',
}
