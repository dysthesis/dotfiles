vim.g.mapleader = " "

-- Set up lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    -- Plugins
    -- Import the lua/plugins directory
    -- Git related plugins
    'tpope/vim-fugitive',
    'tpope/vim-rhubarb',

    -- Detect tabstop and shiftwidth automatically
    'tpope/vim-sleuth',
    { 'folke/which-key.nvim', opts = {} },
    { import = 'plugins' },
  },
  {
    -- Options
  })

-- Modules
require("config.keybinds")
require("config.autocomplete")
require("config.lsp")
require("config.telescope")
require("config.treesitter")
