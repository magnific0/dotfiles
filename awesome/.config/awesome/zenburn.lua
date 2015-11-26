-------------------------------
--  "Zenburn" awesome theme  --
--    By Adrian C. (anrxc)   --
-------------------------------

-- Alternative icon sets and widget icons:
--  * http://awesome.naquadah.org/wiki/Nice_Icons

-- {{{ Main
theme = {}
theme.confdir =  awful.util.getdir("config") .. "/zenburn/"
-- theme.wallpaper_cmd = { "awsetbg " .. theme.confdir .. " zenburn-background.png" }
-- }}}


-- {{{ Styles
-- theme.font      = "DejaVu Sans 8"
theme.font      = "Sans 8"

-- {{{ Colors
theme.fg_normal = "#DCDCCC"
theme.fg_focus  = "#8cd0d3"
theme.fg_urgent = "#CC9393"
theme.bg_normal = "#3F3F3F"
theme.bg_focus  = "#1E2320"
theme.bg_urgent = "#3F3F3F"

theme.fg_black  = "#1E2320"
theme.fg_red    = "#705050"
theme.fg_green  = "#60b48a"
theme.fg_yellow = "#dfaf8f"
theme.fg_blue   = "#506070"
theme.fg_purple = "#dc8cc3"
theme.fg_cyan   = "#8cd0d3"
theme.fg_white  = "#dcdccc"
theme.fg_brightblack  = "#709080"
theme.fg_brightred    = "#cc9393"
theme.fg_brightgreen  = "#7f9f7f"
theme.fg_brightyellow = "#f0dfaf"
theme.fg_brightblue   = "#94bff3"
theme.fg_brightpurple = "#ec93d3"
theme.fg_brightcyan   = "#93e0e3"
theme.fg_brightwhite  = "#ffffff"

-- }}}

-- {{{ Borders
theme.border_width  = "1"
theme.border_normal = "#606060"
theme.border_focus  = "#aaaaaa"
theme.border_marked = "#CC9393"
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = "#3F3F3F"
theme.titlebar_bg_normal = "#3F3F3F"
-- }}}

-- {{{ Taskslist
theme.tlsm_focus  = "#dc8cc3"
theme.tlsm_normal = "#dcdccc"
-- }}}


-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:

-- {{{ Taglist
-- theme.taglist_bg_focus = "#1E2320"
-- }}}
-- }}}

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.fg_widget        = "#AECF96"
--theme.fg_center_widget = "#88A175"
--theme.fg_end_widget    = "#FF5656"
--theme.bg_widget        = "#494B4F"
--theme.border_widget    = "#3F3F3F"
theme.widget_progress_bg = "#606060"
theme.widget_font = "Sans 7.5"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#CC9393"
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "15"
theme.menu_width  = "100"
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = theme.confdir .. "taglist/squarefz.png"
theme.taglist_squares_unsel = theme.confdir .. "taglist/squarez.png"

--theme.taglist_squares_resize = "false"
-- }}}

-- {{{  Widget icons
-- theme.widget_sep       = "seperator.png"
theme.widget_uptime    = "ac_01.png"
theme.widget_cpu       = "cpu.png"
theme.widget_temp      = "temp.png"
theme.widget_mem       = "mem.png"
theme.widget_fs        = "fs_02.png"
-- theme.widget_disk      = "usb.png"
-- theme.widget_myip      = "net_down_01.png"
-- theme.widget_spkr      = "spkr.png"
-- theme.widget_head      = "phones.png"
theme.widget_netdown   = "net_down_03.png"
theme.widget_netup     = "net_up_03.png"
theme.widget_netupdown = "net_updown_03.png"
theme.widget_gmail     = "mail.png"
theme.widget_sys       = "arch_10x10.png"
-- theme.widget_newmail   = "newmail.png"
theme.widget_pac       = "pacman.png"
-- theme.widget_newpackage = "newpackage.png"
theme.widget_batt_full = "bat_full_01.png"
theme.widget_batt_low  = "bat_low_01.png"
theme.widget_batt_crit = "bat_empty_01.png"
theme.widget_clock     = "clock.png"
theme.widget_mpd       = "note.png"
theme.widget_play      = "play.png"
theme.widget_pause     = "pause.png"
theme.widget_stop      = "stop.png"
theme.widget_prev      = "prev.png"
theme.widget_next      = "next.png"
theme.widget_vol       = "spkr_01.png"
theme.widget_volmute   = "spkr_02.png"
theme.widget_wifi      = "wifi_02.png"
theme.widget_wired     = "net_wired.png"
theme.widget_secure    = "fox.png"
theme.widget_pandora   = "phones.png"

-- }}}


-- {{{ Misc
theme.awesome_icon           = theme.confdir .. "awesome-icon.png"
theme.menu_submenu_icon      = "/usr/share/awesome/themes/default/submenu.png"
theme.tasklist_floating_icon = theme.confdir .. "tasklist/floating.png"
theme.tasklist_ontop_icon = theme.confdir .. "tasklist/top.png"

-- }}}

-- {{{ Layout
theme.layout_tile       = theme.confdir .. "layouts/tile.png"
theme.layout_tileleft   = theme.confdir .. "layouts/tileleft.png"
theme.layout_tilebottom = theme.confdir .. "layouts/tilebottom.png"
theme.layout_tiletop    = theme.confdir .. "layouts/tiletop.png"
theme.layout_fairv      = theme.confdir .. "layouts/fairv.png"
theme.layout_fairh      = theme.confdir .. "layouts/fairh.png"
theme.layout_spiral     = theme.confdir .. "layouts/spiral.png"
theme.layout_dwindle    = theme.confdir .. "layouts/dwindle.png"
theme.layout_max        = theme.confdir .. "layouts/max.png"
theme.layout_fullscreen = theme.confdir .. "layouts/fullscreen.png"
theme.layout_magnifier  = theme.confdir .. "layouts/magnifier.png"
theme.layout_floating   = theme.confdir .. "layouts/floating.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = theme.confdir .. "titlebar/close_focus.png"
theme.titlebar_close_button_normal = theme.confdir .. "titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = theme.confdir .. "titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = theme.confdir .. "titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = theme.confdir .. "titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = theme.confdir .. "titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = theme.confdir .. "titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = theme.confdir .. "titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = theme.confdir .. "titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = theme.confdir .. "titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = theme.confdir .. "titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = theme.confdir .. "titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = theme.confdir .. "titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = theme.confdir .. "titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = theme.confdir .. "titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = theme.confdir .. "titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.confdir .. "titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.confdir .. "titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
