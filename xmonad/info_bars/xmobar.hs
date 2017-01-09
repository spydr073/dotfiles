
Config {

     font            = "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=true"
   , additionalFonts = []
   , iconOffset      = -1
   , textOffset      = -1
   , iconRoot        = "/home/spydr/.xmonad/info_bars/xbm/"
   , pickBroadest    = True
   , bgColor         = "black"
   , fgColor         = "#ababab"
   , position        = Top
   , border          = NoBorder
   , borderColor     = "#000000"
   , alpha           = 255  -- in range [0,255]

   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment

   , template = " %StdinReader% \
                \<fc=#000000,#232b2b><icon=corner-lt.xbm/></fc>\
                \<fc=#555555,#232b2b> <icon=spkr.xbm/></fc>\
                \<fc=#4a5d23,#232b2b> %volume% </fc>\
                \<fc=#000000,#232b2b><icon=corner-rb.xbm/></fc>\

                \}%date%{\

                \<fc=#000000,#232b2b>\
                \<icon=corner-lt.xbm/>%multicpu%<icon=corner-rb.xbm/>\
                \<icon=corner-lt.xbm/>%memory%<icon=corner-rb.xbm/>\
                \<icon=corner-lt.xbm/>%coretemp%<icon=corner-rb.xbm/>\
                \<icon=corner-lt.xbm/>%dynnetwork%<icon=corner-rb.xbm/>\
                \<icon=corner-lt.xbm/>%battery%<icon=corner-rb.xbm/>\
                \</fc> "


   , lowerOnStart     = True    -- send to bottom of window stack on start
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , allDesktops      = True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , persistent       = True    -- enable/disable hiding (True = disabled)
   , commands         = [
        Run DynNetwork [
            "--template" , "<fc=#555555,#232b2b>\
                           \ <icon=net_up.xbm/> <tx> \
                           \ <icon=net_down.xbm/> <rx> \
                           \</fc>"
          , "--minwidth" , "4"
          , "--Low"      , "1000" -- units: kB/s
          , "--High"     , "5000" -- units: kB/s
          , "--low"      , "#4a5d23,#232b2b"
          , "--normal"   , "#b8860b,#232b2b"
          , "--high"     , "#7c1c05,#232b2b"
        ] 10

      , Run MultiCpu [
            "--template" , "<fc=#555555,#232b2b> <icon=cpu.xbm/> <autototal> </fc>"
          , "--minwidth" , "2"
          , "--Low"      , "50" -- units: %
          , "--High"     , "85" -- units: %
          , "--low"      , "#4a5d23,#232b2b"
          , "--normal"   , "#b8860b,#232b2b"
          , "--high"     , "#7c1c05,#232b2b"
        ] 10

      , Run CoreTemp [
            "--template" , "<fc=#555555,#232b2b> <icon=temp.xbm/> <core0> </fc>"
          , "--minwidth" , "2"
          , "--Low"      , "70" -- units: °C
          , "--High"     , "80" -- units: °C
          , "--low"      , "#4a5d23,#232b2b"
          , "--normal"   , "#b8860b,#232b2b"
          , "--high"     , "#7c1c05,#232b2b"
        ] 50

      , Run Memory [
            "--template" , "<fc=#555555,#232b2b> <icon=mem.xbm/> <usedratio> </fc>"
          , "--minwidth" , "2"
          , "--Low"      , "30" -- units: %
          , "--High"     , "90" -- units: %
          , "--low"      , "#4a5d23,#232b2b"
          , "--normal"   , "#b8860b,#232b2b"
          , "--high"     , "#7c1c05,#232b2b"
        ] 10

      , Run Battery [
            "--template" , "<acstatus>"
          , "--minwidth" , "2"
          , "--Low"      , "20" -- units: %
          , "--High"     , "80" -- units: %
          , "--low"      , "#7c1c05,#232b2b"
          , "--normal"   , "#b8860b,#232b2b"
          , "--high"     , "#4a5d23,#232b2b"

          , "--"           -- battery specific options
          , "-o"         , "<fc=#7c1c05,#232b2b> <icon=zap.xbm/> </fc><fc=#666666,#232b2b>[<left>,<timeleft>]</fc>"   -- discharging status
          , "-O"         , "<fc=#b8860b,#232b2b> <icon=plug.xbm/> </fc><fc=#666666,#232b2b>[<left>,<timeleft>]</fc>"  -- AC "on" status
          , "-i"         , "<fc=#4a6023,#232b2b>  <icon=full_bat.xbm/>  </fc>"                                        -- charged status
        ] 50

      , Run Date         "<fc=#666666,#000000>%a %F %H:%M %z</fc>"
                         "date" 10

      , Run Com          "/bin/bash"
                         [ "-c" , "/home/spydr/.xmonad/scripts/get-volume.sh" ]
                         "volume"
                         3

      , Run StdinReader
     ]

   }
