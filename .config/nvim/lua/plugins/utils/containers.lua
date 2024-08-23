return {
  'jamestthompson3/nvim-remote-containers',
  cmd = {
    'AttachToContainer',
    'BuildImage',
    'StartImage',
    'ComposeUp',
    'ComposeDown',
    'ComposeDestroy',
  },
  config = function()
    vim.api.nvim_set_hl(0, 'Container', { fg = '#BADA55', bg = 'Black' })
    vim.opt.statusline:append '%#Container#%{g:currentContainer}'
  end,
}
