-- https://github.com/mpv-player/mpv/issues/3500#issuecomment-305646994
mp.register_event("file-loaded", function()
    local hasvid = mp.get_property_osd("video") ~= "no"
    local notcover = mp.get_property_osd("video-format") ~= "mjpeg"

    mp.commandv("script-message", "osc-visibility", ((hasvid and notcover) and "auto" or "always"), "no-osd")
    mp.commandv("set", "options/osd-bar", ((hasvid and notcover) and "yes" or "no"))
end)
