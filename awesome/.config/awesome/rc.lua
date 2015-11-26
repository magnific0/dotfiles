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
-- Theme handling library
beautiful = require("beautiful")

-- Widgets
confdir   = awful.util.getdir("config")
homedir   = os.getenv("HOME")
user      = os.getenv("USER")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(confdir .. "/zenburn.lua")

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
    awful.layout.suit.tile,
--    awful.layout.suit.tile.left,
--    awful.layout.suit.tile.bottom,
--    awful.layout.suit.tile.top,
--    awful.layout.suit.fair,
--    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max
--    awful.layout.suit.max.fullscreen,
--    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags

tags = {
   names  = { "net", "office", "dev", "media" },
   layout = { layouts[2], layouts[2], layouts[2], layouts[2] }
}

for s = 1, screen.count() do
   -- Each screen has its own tag table.
   tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    --awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
 -- }}}

-- {{{ Key bindings (shortcuts, keybinds, keymap)
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "b",
        function()
           awful.tag.viewprev()
           -- Always focus on tag master, because KDE loses entire focus history
           c = awful.client.getmaster()
           if c then client.focus = c c:raise() end
        end),
    awful.key({ modkey,           }, "f",
        function()
           awful.tag.viewnext()
           -- Always focus on tag master, because KDE loses entire focus history
           c = awful.client.getmaster()
           if c then client.focus = c c:raise() end
        end),
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
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "]",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "[",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "]",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "[",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "]",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "[",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "h", awful.client.restore)
)



clientkeys = awful.util.table.join(
    awful.key({ modkey, "Shift"   }, "m",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey,           }, "w",      function (c) if not(string.find(c.class, 'lasma')) then c:kill() end end),
    awful.key({ modkey, "Shift"   }, "t",      awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey, "Shift"   }, "d",      awful.client.movetoscreen                        ),
--    awful.key({ modkey, "Shift"   }, "d",      function (c) awful.screen.focus((c.current_screen() + 1 ) % screencount )                            ),
    awful.key({ modkey,           }, "d",
        function()
           awful.screen.focus_relative( 1)
           -- Always focus on tag master, because KDE loses entire focus history
           c = awful.client.getmaster()
           if c then client.focus = c c:raise() end
        end),
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

-- Manipulate windows with mouse
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
   -- For class name com-mathworks-util-PostVMInit does NOT work, mathworks instead does work.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     -- Fix for KDE
                     maximized_vertical = false,
                     maximized_horizontal = false,
                     -- End fix for KDE
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
    { rule = { class = "XCalc" },
      properties = { floating = true, ontop = true } },
    { rule = { class = "smplayer" },
      properties = { floating = true, ontop = true } },
    { rule = { class = "kcalc" },
      properties = { floating = true, ontop = true } },
    { rule = { name = "urxvtq" },
      properties = { floating = true, ontop = true, border_width = 0, x = 0, y = 0, width = 1920, height = 400 } },
    -- For flash fullscreen videos launched by uzbl
    { rule = { instance = "plugin-container" },
      properties = { fullscreen = true, floating = true, ontop = true } },
    { rule = { name = "exe", class = "Exe" },
      properties = { fullscreen = true, floating = true, ontop = true } },
    { rule = { class = "MATLAB", name="Figure" },
      properties = { floating = true, ontop = true } },
    -- Fix for KDE
    { rule = { class = "krunner" },
      properties = { floating = true, border_width = 0, ontop=true } },
    { rule = { class = "plasmashell" },
      properties = { border_width = 0, focusable = false, sticky = true, floating=true } },
    { rule = { class = "systemsettings" },
      properties = { border_width = 0, floating=true } },
    { rule = { type = "notification" },
      properties = { border_width = 0, floating=true } },
    { rule = { class = "plasmashell", name = "Desktop" },
      properties = { border_width = 0, focusable = false, minimized = true } },
    {  rule = { class = "Plasma-desktop" },
       properties = { floating = true },
       callback = function(c)
          c:geometry( { width = 600 , height = 500 } )
       end }
    -- End fix for KDE
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

end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
