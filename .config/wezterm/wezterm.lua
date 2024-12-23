local wezterm = require("wezterm")

local config = {}

if wezterm.config_builder then
	config = wezterm.config_builder()
end

config.enable_wayland = true
config.term = "wezterm"
config.colors = {
	background = "#000000",
	foreground = "#ffffff",
}
config.font = wezterm.font({
	family = "JetBrainsMono Nerd Font",
	weight = "Regular",
	stretch = "Normal",
	style = "Normal",
	harfbuzz_features = {
		"calt",
		"clig",
		"liga",
		"ss20",
		"cv02",
		"cv03",
		"cv04",
		"cv05",
		"cv06",
		"cv07",
		"cv11",
		"cv14",
		"cv15",
		"cv16",
		"cv17",
	},
})
config.max_fps = 120
config.font_size = 12
config.line_height = 1.2
config.color_scheme = "Catppuccin Mocha"
config.window_background_opacity = 0.75
config.warn_about_missing_glyphs = false
config.enable_scroll_bar = false
config.enable_tab_bar = false
config.window_padding = {
	left = 25,
	right = 25,
	top = 25,
	bottom = 25,
}
config.check_for_updates = false
config.automatically_reload_config = true

config.window_close_confirmation = "NeverPrompt"
config.disable_default_key_bindings = true

local act = wezterm.action

config.keys = {
	{
		key = "c",
		mods = "CTRL|SHIFT",
		action = act({ CopyTo = "ClipboardAndPrimarySelection" }),
	},
	{
		key = "v",
		mods = "CTRL|SHIFT",
		action = act({ PasteFrom = "Clipboard" }),
	},
	{
		key = "_",
		mods = "CTRL|SHIFT",
		action = act.DecreaseFontSize,
	},
	{
		key = "+",
		mods = "CTRL|SHIFT",
		action = act.IncreaseFontSize,
	},
	-- zk.nvim keybinds
	{
		key = "Enter",
		mods = "CTRL",
		action = act({ SendString = "\x1b[13;5u" }),
	},
	{
		key = "Enter",
		mods = "SHIFT",
		action = act({ SendString = "\x1b[13;2u" }),
	},
}

wezterm.on("user-var-changed", function(window, pane, name, value)
	local overrides = window:get_config_overrides() or {}
	if name == "ZEN_MODE" then
		local incremental = value:find("+")
		local number_value = tonumber(value)
		if incremental ~= nil then
			while number_value > 0 do
				window:perform_action(wezterm.action.IncreaseFontSize, pane)
				number_value = number_value - 1
			end
			overrides.enable_tab_bar = false
		elseif number_value < 0 then
			window:perform_action(wezterm.action.ResetFontSize, pane)
			overrides.font_size = nil
			overrides.enable_tab_bar = true
		else
			overrides.font_size = number_value
			overrides.enable_tab_bar = false
		end
	end
	window:set_config_overrides(overrides)
end)
return config
