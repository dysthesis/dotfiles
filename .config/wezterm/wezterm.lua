-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config = {
	colors = {
		foreground = '#ffffff',
		background = '#000000',
		cursor_bg = '#ffffff',
		cursor_fg = '#272727',
		cursor_border = '#ffffff',
		selection_fg = '#000000',
		selection_bg = '#a4b5b5',
		split = '#131313',
		ansi = {
    		'#161a1e',
    		'#e5a3a1',
    		'#b4e3ad',
    		'#ece3b1',
    		'#a3cbe7',
    		'#ceace8',
    		'#c9d4ff',
    		'#eeeff0',
  		},
  		brights = {
    		'#43474b',
    		'#f9b7b5',
    		'#c8f7c1',
    		'#fff7c5',
    		'#b7dffb',
    		'#e2c0fc',
    		'#dde8ff',
    		'#f8f9fa',
	  	},
	},
	enable_tab_bar = false,
	cell_width = 0.9,
	font = wezterm.font("JetBrainsMono Nerd Font"),
	font_size = 12.0,
	window_padding = {
		left = 15,
		right = 15,
		top = 15,
		bottom = 15,
	},
}

-- and finally, return the configuration to wezterm
return config
