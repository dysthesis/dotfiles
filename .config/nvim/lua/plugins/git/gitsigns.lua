return { -- Adds git related signs to the gutter, as well as utilities for managing changes
  'lewis6991/gitsigns.nvim',
  event = 'BufReadPost',
  opts = {
    signs = {
      add = { text = '│' },
      change = { text = '│' },
      delete = { text = '󰍵' },
      topdelete = { text = '‾' },
      changedelete = { text = '~' },
      untracked = { text = '┆' },
    },

    current_line_blame = true,

    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns

      local function opts(desc)
        return { buffer = bufnr, desc = desc }
      end

      local map = vim.keymap.set

      map('n', '<leader>rh', gs.reset_hunk, opts 'Reset Hunk')
      map('n', '<leader>ph', gs.preview_hunk, opts 'Preview Hunk')
      map('n', '<leader>gb', gs.blame_line, opts 'Blame Line')
    end,
  },
}
