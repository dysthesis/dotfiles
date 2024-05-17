-- Define where the vault is locasted here.
-- Note that the string at the end is relative to the home directory, and the true path
-- can be found by prepending a ~ to it. For example, '/Documents/Episteme' translates to
-- '~/Documents/Episteme'
local vault_path = vim.fn.expand '~' .. '/Documents/Episteme'

return {
  'epwalsh/obsidian.nvim',
  enabled = true,
  version = '*', -- recommended, use latest release instead of latest commit
  -- ft = 'markdown', -- Load obsidian.nvim on any markdown file
  event = {
    -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
    'BufReadPre '
      .. vault_path
      .. '**.md',
    'BufNewFile ' .. vault_path .. '/**.md',
  }, -- Load obsidian.nvim only on a markdown path on the vault
  dependencies = {
    -- Required.
    'nvim-lua/plenary.nvim',
    'hrsh7th/nvim-cmp',
    'nvim-telescope/telescope.nvim',
    'nvim-treesitter/nvim-treesitter',
    -- see below for full list of optional dependencies 👇
  },
  keys = {
    {
      '<LocalLeader>on',
      ':ObsidianNew ',
      desc = 'Obsidian: New note from template',
    },
    {
      '<Leader>os',
      '<cmd>ObsidianSearch<cr>',
      desc = 'Obsidian: search',
    },
    {
      '<Leader>ob',
      '<cmd>ObsidianBacklinks<cr>',
      desc = 'Obsidian: Find backlinks',
    },
    {
      '<Leader>ol',
      '<cmd>ObsidianLinks<cr>',
      desc = 'Obsidian: Find backlinks',
    },
    {
      '<Leader>ot',
      '<cmd>ObsidianToday<cr>',
      desc = "Obsidian: Today's daily note",
    },
    {
      '<Leader>oy',
      '<cmd>ObsidianYesterday<cr>',
      desc = "Obsidian: Yesterday's daily note",
    },
    {
      '<Leader>om',
      '<cmd>ObsidianTomorrow<cr>',
      desc = "Obsidian: Tomorrow's daily note",
    },
  },
  opts = {
    workspaces = {
      {
        name = 'Notes',
        path = vault_path,
      },
    },
    daily_notes = {
      -- Optional, if you keep daily notes in a separate directory.
      folder = 'Dailies',
      -- Optional, if you want to change the date format for the ID of daily notes.
      date_format = '%Y-%m-%d',
      -- Optional, if you want to change the date format of the default alias of daily notes.
      alias_format = '%B %-d, %Y',
      -- Optional, if you want to automatically insert a template from your template directory like 'daily.md'
      template = nil,
    },

    -- Optional, completion of wiki links, local markdown links, and tags using nvim-cmp.
    completion = {
      -- Set to false to disable completion.
      nvim_cmp = true,
      -- Trigger completion at 2 chars.
      min_chars = 2,
    },
    -- Optional, configure key mappings. These are the defaults. If you don't want to set any keymappings this
    -- way then set 'mappings = {}'.
    mappings = {
      -- Overrides the 'gf' mapping to work on markdown/wiki links within your vault.
      ['gf'] = {
        action = function()
          return require('obsidian').util.gf_passthrough()
        end,
        opts = { noremap = false, expr = true, buffer = true },
      },
      -- Toggle check-boxes.
      ['<leader>ch'] = {
        action = function()
          return require('obsidian').util.toggle_checkbox()
        end,
        opts = { buffer = true },
      },
      -- Smart action depending on context, either follow link or toggle checkbox.
      ['<cr>'] = {
        action = function()
          return require('obsidian').util.smart_action()
        end,
        opts = { buffer = true, expr = true },
      },
    },
    -- Optional, configure additional syntax highlighting / extmarks.
    -- This requires you have `conceallevel` set to 1 or 2. See `:help conceallevel` for more details.
    ui = {
      enable = true, -- set to false to disable all additional syntax features
      update_debounce = 200, -- update delay after a text change (in milliseconds)
      -- Define how various check-boxes are displayed
      checkboxes = {
        -- NOTE: the 'char' value has to be a single character, and the highlight groups are defined below.
        [' '] = { char = '󰄱', hl_group = 'ObsidianTodo' },
        ['x'] = { char = '', hl_group = 'ObsidianDone' },
        ['>'] = { char = '', hl_group = 'ObsidianRightArrow' },
        ['~'] = { char = '󰰱', hl_group = 'ObsidianTilde' },

        -- You can also add more custom ones...
      },
      -- Use bullet marks for non-checkbox lists.
      bullets = { char = '•', hl_group = 'ObsidianBullet' },
      external_link_icon = { char = '', hl_group = 'ObsidianExtLinkIcon' },
      -- Replace the above with this if you don't have a patched font:
      -- external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
      reference_text = { hl_group = 'ObsidianRefText' },
      highlight_text = { hl_group = 'ObsidianHighlightText' },
      tags = { hl_group = 'ObsidianTag' },
      block_ids = { hl_group = 'ObsidianBlockID' },
    },
    -- Optional, customize how note IDs are generated given an optional title.
    ---@param title string|?
    ---@return string
    note_id_func = function(title)
      -- Create note IDs in a Zettelkasten format with a timestamp and a suffix.
      -- In this case a note with the title 'My new note' will be given an ID that looks
      -- like '1657296016-my-new-note', and therefore the file name '1657296016-my-new-note.md'
      local suffix = ''
      if title ~= nil then
        -- If title is given, transform it into valid file name.
        suffix = title:gsub(' ', '-'):gsub('[^A-Za-z0-9-]', ''):lower()
      else
        -- If title is nil, just add 4 random uppercase letters to the suffix.
        for _ = 1, 4 do
          suffix = suffix .. string.char(math.random(65, 90))
        end
      end
      return tostring(os.time()) .. '-' .. suffix
    end,

    -- Optional, customize how note file names are generated given the ID, target directory, and title.
    ---@param spec { id: string, dir: obsidian.Path, title: string|? }
    ---@return string|obsidian.Path The full path to the new note.
    note_path_func = function(spec)
      local file_name
      if spec.title and spec.title ~= '' then
        file_name = spec.title
        file_name = file_name:match '^%s*(.-)%s*$'
        file_name = file_name:gsub('[<>:"/\\|?*]', '_')
      else
        file_name = tostring(spec.id)
      end
      local path = spec.dir / file_name
      return path:with_suffix '.md'
    end,
  },
}
