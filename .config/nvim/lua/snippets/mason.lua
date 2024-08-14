local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

ls.add_snippets('lua', {
	s('masonconfig', {
		t({
			"{",
			"    'williamboman/mason.nvim',",
			"    dependencies = {",
			"      'WhoIsSethDaniel/mason-tool-installer.nvim',",
			"      opts = function(_, opts)",
			"        require('mason-tool-installer').setup { ensure_installed = opts.ensure_installed }",
			"      end,",
			"    },",
			"    opts = function(_, opts)",
			"      opts.ensure_installed = opts.ensure_installed or {}",
			"      vim.list_extend(opts.ensure_installed, { ",
		}),
		i(1), -- Placeholder for tools
		t({
			" })",
			"    end,",
			"  },",
		}),
	}),
})
