--                                           ___                __     
--                                     __  /'___\ __          /'__`\   
--   ___ ___      __       __     ___ /\_\/\ \__//\_\    ___ /\ \/\ \  
-- /' __` __`\  /'__`\   /'_ `\ /' _ `\/\ \ \ ,__\/\ \  /'___\ \ \ \ \ 
-- /\ \/\ \/\ \/\ \L\.\_/\ \L\ \/\ \/\ \ \ \ \ \_/\ \ \/\ \__/\ \ \_\ \
-- \ \_\ \_\ \_\ \__/.\_\ \____ \ \_\ \_\ \_\ \_\  \ \_\ \____\\ \____/
--  \/_/\/_/\/_/\/__/\/_/\/___L\ \/_/\/_/\/_/\/_/   \/_/\/____/ \/___/ 
--                         /\____/                                     
--                         \_/__/
--                                      
-- configuration script based on example rc.lua, modified by magnfici0.
-- Licensed under GPLv3

-- Standard awesome library
gears = require("gears")
awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
wibox = require("wibox")
vicious = require("vicious")
-- Theme handling library
beautiful = require("beautiful")
-- Notification library
naughty = require("naughty")
menubar = require("menubar")

onebar = true

-- Widgets
confdir   = awful.util.getdir("config")
homedir   = os.getenv("HOME")
user      = os.getenv("USER")


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(confdir .. "/zenburn.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
xsudo = "gksudo"
editor = os.getenv("EDITOR") or "emacs"
editor_cmd = editor
function runner(name,incli,asroot) 
   incli = incli or false
   asroot = asroot or false
   if incli then
      name = terminal .. " -e " .. name
   end
   if asroot then
      name = xsudo .. " '" .. name .. "'"
   end
   awful.util.spawn_with_shell(name)
end
function runnerwu(name,widgettoupdate,incli,asroot) 
   incli = incli or false
   asroot = asroot or false
   runner(name .. "; echo 'vicious.force({" .. widgettoupdate .. "})' | awesome-client", incli, asroot) 
end

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
--    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
--    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
--    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- widget icons
function wicon(name, colour)
   if name == nil then name = ""; end
   return confdir .. "/icons/widgets/" .. colour .. "/" .. name .. ""
end

-- task / topbar icons
function ticon(name, colour)
   if name == nil then name = ""; end
   return confdir .. "/icons/theme/" .. colour .. "/" .. name .. ""
end

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tagiconnames = { "layers_1", "globe_2", "star", "doc_edit", "bug", "book", "headphones", "spechbubble_2"}
tagicons = {}
for i = 1, 8 do
   tagicons[i] = ticon(tagiconnames[i] .. ".png","white")
end
tags = {
   names  = { "", "", "", "", "", "", "", ""},
   --mes  = { "term", "net", "park", "write", "arena", "shelf", "media", "link"}
   icons  = tagicons,
   layout = { layouts[2],layouts[5],layouts[1],layouts[6],layouts[1],layouts[6],layouts[2],layouts[2]}
}
 
for s = 1, screen.count() do
   -- Each screen has its own tag table.
   
   tags[s] = awful.tag(tags.names, s, tags.layout)
   
   for i, t in ipairs(tags[s]) do
      awful.tag.seticon(tags.icons[i], t)
   end
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox

display_bars = false


-- Seperators and the like
-- Interesting characters: ¦·‹›«»–—…×÷•¤
sepl = wibox.widget.textbox()
sepc_text = wibox.widget.textbox()
sepr = wibox.widget.textbox()
space = wibox.widget.textbox()
sepl:set_text("")        --"« "
sepc_text:set_text(" | ")
sepr:set_text(" ")        --"» "
sepc = wibox.widget.background()
sepc:set_widget(sepc_text)
sepc:set_fg(beautiful.widget_progress_bg)
spacetext = " "
space:set_text(spacetext)

-- -- Weather widget.
-- weatherwidget = wibox.widget.textbox()
-- weatherwidget:set_font(beautiful.widget_font)
-- weathericon = wibox.widget.imagebox()
-- weathericon:set_image(ticon("cloud.png","white"))
-- weathertooltip = awful.tooltip({ objects = { weatherwidget, weathericon },})
 
-- vicious.register(weatherwidget, 
-- 		 vicious.widgets.weather,
--                  function (widget, args)
-- 		    if args["{tempc}"] == "N/A" then
-- 		       weathericon.visible = false
-- 		       weatherwidget.visible = false
-- 		    else
-- 		       weathericon.visible = true
-- 		       weatherwidget.visible = true
-- 		       weathertooltip:set_text("City: " .. args["{city}"] .."\nWind: " .. args["{windkmh}"] .. " km/h " .. args["{wind}"] .. "\nSky: " .. args["{sky}"] .. "\nHumidity: " .. args["{humid}"] .. "%")
-- 		    end
-- 		    return args["{tempc}"] .. "°C" .. spacetext
--                  end, 300, "NLXX0015")
-- --    {windkmh}, {sky}, {weather}, {tempf}, {tempc}, {humid}, {press}

-- Create a textclock widget
clockicon = wibox.widget.imagebox()
clockicon:set_image(ticon("clock.png","white"))
mytextclock = awful.widget.textclock()

-- -- System
-- sysicon = wibox.widget.imagebox()
-- sysicon:set_image(ticon("arch_linux.png","white"))
-- sysicon.align = "middle"

-- syswidget = wibox.widget.textbox()
-- syswidget:set_font(beautiful.widget_font)
-- vicious.register( syswidget, vicious.widgets.os, "$1 $2", 60)

-- awful.widget.layout.margins[sysicon] = { top = 0 }

-- -- Pacman Icon
pacicon = wibox.widget.imagebox()
pacicon:set_image(wicon(beautiful.widget_pac,"brightblue"))
-- -- Pacman Widget
pacwidget = wibox.widget.textbox()
vicious.register(pacwidget, vicious.widgets.pkg,
		 function(widget,args)
		    local io  = io.open("/tmp/pacman.count", "r")
		    if io ~= nil then 
		       local s = io:read()
		       io.close()
		       return s
		    else
		       return ""
		    end
 		 end, 120, "Arch")

paciconbuttonscheme = awful.util.table.join(
   awful.button({ }, 1, function () runnerwu("pacaur -rSu","pacwidget",true) end),
   awful.button({ }, 3, function () runnerwu("pacaur -aSu","pacwidget",true) end)) 

pacicon:buttons(paciconbuttonscheme)
pacwidget:buttons(paciconbuttonscheme)

-- Uptime

exitmenu = awful.menu(
   {items = {
       { "Sleep" ,     function () runner("sysctl suspend") end },
       { "Hibernate" , function () runner("sysctl hibernate") end },
       { "Shutdown" ,  function () runner("sysctl poweroff") end },
       { "Restart" ,   awesome.restart },
       { "Reboot" ,    function () runner("sysctl reboot") end },
       { "Quit" ,      awesome.quit }
   }
   })

uptimeicon = wibox.widget.imagebox()
uptimeicon:set_image(wicon(beautiful.widget_clock,"brightblue"))

uptimewidget = wibox.widget.textbox()
uptimewidget:set_font(beautiful.widget_font)
vicious.register( uptimewidget, vicious.widgets.uptime, "$2.$3'")

uptimeicon:buttons(awful.util.table.join(
		      awful.button({ }, 1, function () exitmenu:toggle() end )
					))

uptimewidget:buttons(awful.util.table.join(
			awful.button({ }, 1, function () exitmenu:toggle() end )
					  ))

-- Temp Icon
tempicon = wibox.widget.imagebox()
tempicon:set_image(wicon(beautiful.widget_temp,"brightblue"))
-- Temp Widget
tempwidget = wibox.widget.textbox()
tempwidget:set_font(beautiful.widget_font)
vicious.register(tempwidget, vicious.widgets.thermal, "$1°C", 9, {"coretemp.0","core"})

tempicon:buttons(awful.util.table.join(
    awful.button({ }, 1, function () runner("powertop",true,true) end)
))


-- -- email widget 
mailiconbuttonscheme = awful.util.table.join(
   awful.button({ }, 1, function () runner("emacs -gnus") end))

mailwidget = wibox.widget.textbox()
mailwidget:set_font(beautiful.widget_font)
vicious.register(mailwidget, 
		 vicious.widgets.uptime,
		 function(widget,args)
		    awful.util.spawn(homedir .. "/.scripts/mailcheck/checkunread.pl")
		    local io  = assert(io.open(homedir .. "/.scripts/mailcheck/mailstatus", "r"))
 		    local s = io:read()
		    io.close()
		    return s
		 end, 60)
mailicon = wibox.widget.imagebox()
mailicon:set_image(wicon(beautiful.widget_gmail,"brightblue"))
mailicon:buttons(mailiconbuttonscheme)
mailwidget:buttons(mailiconbuttonscheme)

-- CPU Icon
cpuicon = wibox.widget.imagebox()
cpuicon:set_image(wicon(beautiful.widget_cpu,"brightblue"))

-- CPU Widget
cpubarsep = wibox.widget.textbox()
cpubarsep:set_font("Sans 2")
cpubarsep:set_text(" ")

cpucores = 4
cpubar = {}
for i = 1, cpucores do
   cpubar[i] = awful.widget.progressbar()
   cpubar[i]:set_width(6)
   cpubar[i]:set_height(13)
   cpubar[i]:set_max_value(100)
   cpubar[i]:set_vertical(true)
   cpubar[i]:set_background_color(beautiful.widget_progress_bg)
   cpubar[i]:set_color({ type = "linear", from = { 0, 0 }, to = { 0, 20 }, stops = { { 0, beautiful.fg_red }, { 0.5, beautiful.fg_yellow }, { 1, beautiful.fg_green } }})
end

-- Cpu usage
cpuwidget = wibox.widget.textbox()
cpuwidget:set_font(beautiful.widget_font)
vicious.register( cpuwidget, vicious.widgets.cpu, 
		  function (widget, args)
		     local s = ""
		     for i = 1, cpucores do
			cpubar[i]:set_value(args[i+1])
		     end
		     return "" --s
		  end, 5)

cpuicon:buttons(awful.util.table.join(
		   awful.button({ }, 1, function () runner("htop",true) end)
				     ))

-- BATT Icon
baticon = wibox.widget.imagebox()

-- Battery usage
powermenu = awful.menu({items = {
			   { "Auto" ,        function () runner("cpufreq-set -r -g ondemand",   false,true) end },
			   { "Ondemand" ,    function () runner("cpufreq-set -r -g ondemand",   false,true) end },
			   { "Powersave" ,   function () runner("cpufreq-set -r -g powersave",  false,true) end },
			   { "Performance" , function () runner("cpufreq-set -r -g performance",false,true) end }
				}})

-- Initialize BATT widget progressbar
if display_bars then
   batbar = awful.widget.progressbar()
   batbar:set_width(50)
   batbar:set_height(6)
   batbar:set_vertical(false)
   batbar:set_background_color(beautiful.widget_progress_bg)
   batbar:set_border_color(nil)
   batbar:set_gradient_colors({ beautiful.fg_normal, beautiful.fg_normal, beautiful.fg_normal, beautiful.bar })
   --awful.widget.layout.margins[batbar.widget] = { top = 5 }
   vicious.register( batbar, vicious.widgets.bat, "$2", 1, "BAT1" )
end

batwidget = wibox.widget.textbox()
batwidget:set_font(beautiful.widget_font)
batteryCriticalLevel = 10
batteryLowLevel = 20
-- Register widget
vicious.register(batwidget, 
		 vicious.widgets.bat, 
		 function (widget, args)
		    if args[2] <= batteryCriticalLevel then
		       baticon:set_image(wicon(beautiful.widget_batt_crit,"brightred"))
		    elseif args[2] <= batteryLowLevel then
		       baticon:set_image(wicon(beautiful.widget_batt_low,"brightyellow"))
		    else
		       baticon:set_image(wicon(beautiful.widget_batt_full,"brightgreen"))
		    end
		    timeleft = ""
		    if args[3] ~= "N/A" then 
		       timeleft = spacetext .. "(" .. args[3] .. ")" 
		    end
		    return args[2] .. "%" .. timeleft .. spacetext .. args[1]
		 end, 3, "BAT1")

baticon:buttons(
   awful.util.table.join(
      awful.button({ }, 1, function () powermenu:toggle() end )
			))

batwidget:buttons(
   awful.util.table.join(
      awful.button({ }, 1, function () powermenu:toggle() end )
			))

-- Vol Icon and buttons
volicon = wibox.widget.imagebox()

volbuttonscheme = awful.util.table.join(
   awful.button({ }, 1, function () runnerwu("amixer -q sset Master toggle","volumewidget") end),
   awful.button({ }, 3, function () runner("alsamixer",true) end),
   awful.button({ }, 4, function () runnerwu("amixer -q sset Master 2%+","volumewidget") end),
   awful.button({ }, 5, function () runnerwu("amixer -q sset Master  2%-","volumewidget") end))

-- Vol bar Widget
if display_bars then
   volbar = awful.widget.progressbar()
   volbar:set_width(50)
   volbar:set_height(6)
   volbar:set_vertical(false)
   volbar:set_background_color(beautiful.widget_progress_bg)
   volbar:set_border_color(nil)
   --volbar:set_color({ type = "linear", from = { 0, 0 }, to = { 0, 20 }, stops = { { 0, "#ff0000" }, { 0.5, "#00ff00" }, { 1, "#0000ff" } }})
   volbar:set_color(beautiful.fg_normal)
   --awful.widget.layout.margins[volbar.widget] = { top = 6 }
   vicious.register(volbar, vicious.widgets.volume, "$1", 5, "Master" )
   volbar.widget:buttons(volbuttonscheme)
end

-- Sound volume
volumewidget = wibox.widget.textbox()
volumewidget:set_font(beautiful.widget_font)
vicious.register(volumewidget, 
		 vicious.widgets.volume,  
		 function (widget, args)
		    if args[2] == "♩" then
		       volicon:set_image(wicon(beautiful.widget_volmute,"red"))
		    else
		       volicon:set_image(wicon(beautiful.widget_vol,"brightblue"))
		    end
		    return args[1] .. "%"
		 end, 10, "Master")

volicon:buttons(volbuttonscheme)
volumewidget:buttons(volbuttonscheme)

-- 
-- Net Widget
--
netsep = wibox.widget.textbox()
netsep:set_text(" · ")

netint = { "eth0", "wlan0", "ppp0" }
netico = { beautiful.widget_wired, beautiful.widget_wifi, beautiful.widget_secure }
netwfi = { false, true, false } -- distinct certain actions, currently used to display ssid for wifi connections
netact = {nil,
	  awful.util.table.join(
	     awful.button({ }, 2, function () runner("wicd-cli -xy") end),
	     awful.button({ }, 1, function () runner("wicd-curses",true) end)),
	  awful.util.table.join(
	     awful.button({ }, 1, function () runner("poff ninsei || pon ninsei",false,true) end))}

netupdownicon = {}
netupdowniconc = {}
netdowninfo_text = {}
netupinfo_text = {}
netdowninfo = {}
netupinfo = {}
netinticon = {}
netintinfo = {}
netintpretext = {}

for i = 1, #netint do

   -- icons etcetera, currently one shared icon for up and down, looks amazing

   netupdownicon[i] = wibox.widget.imagebox()
   netupdownicon[i]:set_image(wicon(beautiful.widget_netupdown,"special"))
   netupdownicon[i].align = "middle"
   netupdowniconc[i] = wibox.layout.margin(netupdownicon[i])

   netintpretext[i] = wibox.widget.textbox()
   netupinfo_text[i] = wibox.widget.textbox()
   netdowninfo_text[i] = wibox.widget.textbox()
   netintpretext[i]:set_font(beautiful.widget_font)
   netupinfo_text[i]:set_font(beautiful.widget_font)
   netdowninfo_text[i]:set_font(beautiful.widget_font)

   netupinfo[i] = wibox.widget.background()
   netupinfo[i]:set_widget(netupinfo_text[i])
   netupinfo[i]:set_fg(beautiful.fg_red)

   netdowninfo[i] = wibox.widget.background()
   netdowninfo[i]:set_widget(netdowninfo_text[i])
   netdowninfo[i]:set_fg(beautiful.fg_green)

   netinticon[i] = wibox.widget.imagebox()
   netinticon[i].align = "middle"
 
   netintinfo[i] = wibox.widget.textbox()
   vicious.register(netintinfo[i], 
		    vicious.widgets.net,
		    function (widget, args)    
		       function ip_addr()
			  local ip = io.popen("ip addr show " .. netint[i] ..  " | grep 'inet '")
			  local addr = ip:read("*a")
			  ip:close()
			  addr = string.match(addr, "%d+.%d+.%d+.%d+")
			  return addr
		       end
		       if ip_addr() == nil then
			  netintpretext[i]:set_text("") -- could say eth0: disconnect or whatever
			  netupdowniconc[i]:set_widget(nil)
			  netdowninfo[i]:set_widget(nil)
			  netupinfo[i]:set_widget(nil)
			  netinticon[i]:set_image(wicon(netico[i],"grey"))    
			  return ""
		       else
			  netintpretext[i]:set_text("") -- could say eth0: 10.0.0.11 or whatever
			  if netwfi[i] then
			     netintpretext[i]:set_text(vicious.widgets.wifi("",spacetext .. netint[i])["{ssid}"] .. spacetext)
			  end
			  netupdowniconc[i]:set_widget(netupdownicon[i])
			  netdowninfo[i]:set_widget(netdowninfo_text[i])
			  netupinfo[i]:set_widget(netupinfo_text[i])
			  netinticon[i]:set_image(wicon(netico[i],"brightblue"))
			  netdowninfo_text[i]:set_text(string.format("%.1f", args["{" .. netint[i] .. " down_kb}"]))
			  netupinfo_text[i]:set_text(string.format("%.1f", args["{" .. netint[i] .. " up_kb}"]))
			  return ""
		       end
		    end, 10, netint[i])
   
   netinticon[i]:buttons(
      awful.util.table.join(
	 awful.button({ }, 3, function () runner(homedir .. "/.scripts/toggledev.sh",false,true) end)
			   ))

   if netact[i] ~= nil then
      netinticon[i]:buttons(netact[i])
   end
end   

--
-- Memory
--
-- MEM icon
memicon = wibox.widget.imagebox()
memicon:set_image(wicon(beautiful.widget_mem,"brightblue"))

-- Initialize MEMBar widget
if display_bars then
   membar = awful.widget.progressbar()
   membar:set_width(50)
   membar:set_height(6)
   membar:set_vertical(false)
   membar:set_background_color(beautiful.widget_progress_bg)
   membar:set_border_color(nil)
   --membar:set_color({ type = "linear", from = { 0, 0 }, to = { 0, 20 }, stops = { { 0, "#ff0000" }, { 0.5, "#00ff00" }, { 1, "#0000ff" } }})
   membar:set_color(beautiful.fg_normal)
   --awful.widget.layout.margins[membar.widget] = { top = 6 }
   vicious.register(membar, vicious.widgets.mem, "$1", 1)
end

-- Memory usage
memwidget = wibox.widget.textbox()
memwidget:set_font(beautiful.widget_font)
vicious.register(memwidget, vicious.widgets.mem, "$2/$3MB", 1)

memicon:buttons(awful.util.table.join(
		   awful.button({ }, 1, function () runner("saidar -c",true) end)
				     ))

--
-- File Systems
--
-- Arguments:
--  {/ size_mb}, {/ size_gb}, {/ used_mb}, {/ used_gb}, {/ used_p},
--  {/ avail_mb}, {/ avail_gb}, {/ avail_p}, {/home size_mb} etc.
fslist = { "/", "/home", "/var"}
-- FS icon
fsicon = wibox.widget.imagebox()
fsicon:set_image(wicon(beautiful.widget_fs,"brightblue"))

-- Text and bars
fsbar  = {}
fspretext = {}
fssuftext = {}
fssep = wibox.widget.textbox()
fssep:set_text(" · ")
for i = 1, #fslist do
   if display_bars then
      fsbar[i] = awful.widget.progressbar()
      fsbar[i]:set_width(50)
      fsbar[i]:set_height(6)
      fsbar[i]:set_max_value(100)
      fsbar[i]:set_vertical(false)
      fsbar[i]:set_background_color(beautiful.widget_progress_bg)
      fsbar[i]:set_border_color(nil)
      --fsbar[i]:set_color({ type = "linear", from = { 0, 0 }, to = { 0, 20 }, stops = { { 0, "#ff0000" }, { 0.5, "#00ff00" }, { 1, "#0000ff" } }})
      fsbar[i]:set_color(beautiful.fg_normal)
      --awful.widget.layout.margins[fsbar[i].widget] = { top = 6 }
   end
   fspretext[i] = wibox.widget.textbox()
   fspretext[i]:set_font(beautiful.widget_font)
   fssuftext[i] = wibox.widget.textbox()
   fssuftext[i]:set_font(beautiful.widget_font)
end

-- Widget
fswidget = wibox.widget.textbox()
fswidget:set_font(beautiful.widget_font)
vicious.register(fswidget, vicious.widgets.fs, 
		 function (widget, args)
		    local s = ""
		    for i = 1, #fslist do
		       if display_bars then
			  fsbar[i]:set_value(args["{" .. fslist[i] .. " used_p}"])
		       end 
		       fspretext[i]:set_text(fslist[i] .. ":" .. spacetext)
		       fssuftext[i]:set_text(spacetext .. args["{" .. fslist[i] .. " used_gb}"] .. "/" .. args["{" .. fslist[i] .. " size_gb}"] .. "GB")
		    end
		    return ""
		 end, 10)
--
-- MPD Daemon Control
--
-- MPD controls
music_play = wibox.widget.imagebox()
music_play:set_image(wicon(beautiful.widget_play,"white"))
music_playc = wibox.layout.margin(music_play)
music_playc:buttons(awful.util.table.join(
		      awful.button({ }, 1, function () runnerwu("mpc toggle","mpdwidget") end)
					))

music_pause = wibox.widget.imagebox()
music_pause:set_image(wicon(beautiful.widget_pause,"white"))
music_pausec = wibox.layout.margin(music_pause)
music_pausec:buttons(awful.util.table.join(
		       awful.button({ }, 1, function () runnerwu("mpc toggle","mpdwidget") end)
					 ))


music_stop = wibox.widget.imagebox()
music_stop:set_image(wicon(beautiful.widget_stop,"white"))
music_stopc = wibox.layout.margin(music_stop)
music_stopc:buttons(awful.util.table.join(
		      awful.button({ }, 1, function () runnerwu("mpc stop","mpdwidget") end)
					))


music_prev = wibox.widget.imagebox()
music_prev:set_image(wicon(beautiful.widget_prev,"white"))
music_prevc = wibox.layout.margin(music_prev)
music_prevc:buttons(awful.util.table.join(
		       awful.button({ }, 1, function () runnerwu("mpc prev","mpdwidget") end)
					))


music_next = wibox.widget.imagebox()
music_next:set_image(wicon(beautiful.widget_next,"white"))
music_nextc = wibox.layout.margin(music_next)
music_nextc:buttons(awful.util.table.join(
		      awful.button({ }, 1, function () runnerwu("mpc next","mpdwidget") end)
					))


-- MPD Icon
mpdicon = wibox.widget.imagebox()
mpdicon:set_image(wicon(beautiful.widget_mpd,"brightblue"))
-- Initialize MPD Widget
mpdwidget = wibox.widget.textbox()
mpdwidget:set_font(beautiful.widget_font)
mpdwidgetc = wibox.layout.margin(mpdwidget)
vicious.register(mpdwidget, 
		 vicious.widgets.mpd,
		 function (widget, args)
		    local s = ""
		    if args["{state}"] == "Stop" then 
		       music_playc:set_widget(music_play)
		       music_pausec:set_widget(nil)
		       music_nextc:set_widget(nil)
		       music_prevc:set_widget(nil)
		       music_stopc:set_widget(nil)
		       mpdwidgetc:set_widget(nil)
		       s = s .. "stopped"
		    elseif args["{state}"] == "Pause" then
		       music_playc:set_widget(music_play)
		       music_pausec:set_widget(nil)
		       music_nextc:set_widget(music_next)
		       music_prevc:set_widget(music_prev)
		       music_stopc:set_widget(music_stop)
		       mpdwidgetc:set_widget(mpdwidget)
		       s = s .. "paused"
		    elseif args["{state}"] == "Play" then 
		       music_playc:set_widget(nil)
		       music_pausec:set_widget(music_pause)
		       music_nextc:set_widget(music_next)
		       music_prevc:set_widget(music_prev)
		       music_stopc:set_widget(music_stop)
		       mpdwidgetc:set_widget(mpdwidget)
		       s = s .. args["{Title}"]..' - '.. args["{Artist}"]
		    else
		       music_playc:set_widget(music_play)
		       music_pausec:set_widget(nil)
		       music_nextc:set_widget(nil)
		       music_prevc:set_widget(nil)
		       music_stopc:set_widget(nil)
		       mpdwidgetc:set_widget(nil)
		       s = s .. "off"
		    end
		    return s
		 end, 5)

mpdicon:buttons(
   awful.util.table.join(
      awful.button({ }, 1, function () runner("ncmpcpp",true) end)
			))


-- Post processing of widgets
netwidget = wibox.layout.fixed.horizontal()
for i = 1, #netint do
   netwidget:add(netinticon[i])   
   netwidget:add(netintpretext[i])
   netwidget:add(netintinfo[i])
   netwidget:add(netupdowniconc[i])
   netwidget:add(netupinfo[i])
   if i ~= #netint then
      netwidget:add(netsep)
   end
end

cpubarwidget = wibox.layout.fixed.horizontal()
cpubarwidget:add(cpuicon)
for i = 1, cpucores do
   cpubarwidget:add(cpubar[i])
   if i ~= cpucores then
      cpubarwidget:add(cpubarsep)
   end
end
cpubarwidget:add(cpuwidget)

fsgroupwidget = wibox.layout.fixed.horizontal()
for i = 1, #fslist do
   -- with bars: {fspretext[i],fsbar[i],fssuftext[i]}
   fsgroupwidget:add(fspretext[i])
   fsgroupwidget:add(fssuftext[i])
   if i ~= #fslist then
      fsgroupwidget:add(fssep)
   end
end
fsgroupwidget:add(fswidget)

-- Create a wibox for each screen and add it
mywibox = {}
mybottomwibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))
for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s, border_width = 0, height = 18})

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(mytextclock)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)

    mybottomwibox[s] = awful.wibox({ position = "bottom", screen = s, border_width = 0 })

    -- Widgets that are aligned to the left
    local bot_left_layout = wibox.layout.fixed.horizontal()
    bot_left_layout:add(sepl)
    bot_left_layout:add(volicon)
    bot_left_layout:add(volumewidget)
    bot_left_layout:add(sepc)
    bot_left_layout:add(baticon)
    bot_left_layout:add(batwidget)
    bot_left_layout:add(sepc)
    bot_left_layout:add(memicon)
    bot_left_layout:add(memwidget)
    bot_left_layout:add(sepc)
    bot_left_layout:add(fsgroupwidget)
    bot_left_layout:add(sepc)
    bot_left_layout:add(cpubarwidget)
    bot_left_layout:add(space)
    bot_left_layout:add(tempwidget)
    bot_left_layout:add(sepr)

    -- Widgets that are aligned to the right
    local bot_right_layout = wibox.layout.fixed.horizontal()
    bot_right_layout:add(sepl)
    bot_right_layout:add(mpdicon)
    bot_right_layout:add(mpdwidgetc)
    bot_right_layout:add(music_prevc)
    bot_right_layout:add(music_stopc)
    bot_right_layout:add(music_pausec)
    bot_right_layout:add(music_playc)
    bot_right_layout:add(music_nextc)
    bot_right_layout:add(sepc)
    bot_right_layout:add(mailicon)
    bot_right_layout:add(mailwidget)
    bot_right_layout:add(space)
    bot_right_layout:add(sepc)
    bot_right_layout:add(netwidget)
    bot_right_layout:add(sepc)
    bot_right_layout:add(pacicon)
    bot_right_layout:add(pacwidget)
    bot_right_layout:add(sepc)
    bot_right_layout:add(uptimeicon)
    bot_right_layout:add(uptimewidget)
    bot_right_layout:add(sepr)
    -- Now bring it all together 
    local bot_layout = wibox.layout.align.horizontal()
    bot_layout:set_left(bot_left_layout)
    bot_layout:set_right(bot_right_layout)

    mybottomwibox[s]:set_widget(bot_layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    --awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
 -- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "b",      awful.tag.viewprev       ),
    awful.key({ modkey,           }, "f",      awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "n",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "p",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    -- awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "n", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "p", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "n", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "p", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () runner(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "=",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "-",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "=",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "-",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "=",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "-",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "h", awful.client.restore),

    -- Prompt
    -- awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),

    -- Custom keys
    awful.key({ modkey, "Shift"   }, "Return", function () runner("/bin/bash",true,true)       end),
    awful.key({ modkey,           }, "r",      function () runner("bashrun")                   end),
    awful.key({ modkey,           }, "F1",     function () runner("uzblicious -n")             end),
    awful.key({ modkey, "Shift"   }, "F1",     function () runner("uzblicious -b history -n")  end),
    awful.key({ modkey,           }, "F2",     function () runner("mc",true)                   end),
    awful.key({ modkey, "Shift"   }, "F2",     function () runner("mc",true,true)              end),
    awful.key({ modkey,           }, "F3",     function () runner("emacs")                     end),
    awful.key({ modkey, "Shift"   }, "F3",     function () runner("emacs -u " .. user,false,true) end),
    awful.key({ modkey,           }, "F4",     function () runner("ncmpcpp",true)              end),
    awful.key({ modkey,           }, "F11",    function () runner("~/.scripts/togglekblayout") end),
    awful.key({ modkey,           }, "F12",    function () runner("slimlock")                  end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey, "Shift"   }, "m",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey,           }, "w",      function (c) c:kill()                         end),
    awful.key({ modkey, "Shift"   }, "t",      awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "h",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
		     size_hints_honor = false } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { name = "bashrun" },
      properties = { floating = true, focus = true, border_width = 0, x = 0, y = 0, width = 1600, height = 18 } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } }
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Disable sloppy focus on mouse hover over
    -- c:connect_signal("mouse::enter", function(c)
    --     if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
    --         and awful.client.focus.filter(c) then
    --         client.focus = c
    --     end
    -- end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local title = awful.titlebar.widget.titlewidget(c)
        title:buttons(awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                ))

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(title)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

function run_once(prg,arg_string,pname,tag)
    if not prg then
        do return nil end
    end

    if not pname then
       pname = prg
    end

    if not tag then
    else
	awful.tag.viewonly(tags[1][tag])
    end

    local io   = io.popen("pgrep -u " .. user .. " -x '" .. pname .. "'")
    local pids = io:read("*a")
    io:close()
    if pids == "" then
       if not arg_string then
	  awful.util.spawn_with_shell(prg)
       else
	  awful.util.spawn_with_shell(prg .. " " .. arg_string)
       end
    end
    if not tag then
    else
        awful.tag.viewonly(tags[1][1])
    end

end

-- run_once("unclutter","-idle 3 -root",nil) screw u unclutter
-- run_once("wicd-client -t",nil,"/usr/bin/python2 -O /usr/share/wicd/gtk/wicd-client.py") curses all the way baby
-- run_once("conky",nil,nil) wiboxing for now
run_once("~/.scripts/netapps.sh",nil,"dropbox",nil)
run_once("wmname LG3D",nil,nil,nil)
run_once("redshift -l 55:4",nil,"redshift",nil)
run_once("~/.scripts/randomwall space",nil,nil,nil)
-- run_once("~/.scripts/Space2Ctrl/start_Space2Ctrl.sh",nil,"Space2Ctrl",nil)
run_once("devmon",nil,"devmon",nil)