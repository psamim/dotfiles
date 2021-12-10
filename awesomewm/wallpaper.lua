local naughty = require("naughty")
local gears = require("gears")
local awful = require("awful")

local config = {}
local timer = nil

local index = 65535
local images = {}

local function mergeTables(src1, src2)
   local dest = src1
   for i, v in pairs(src2) do
      dest[i] = v
   end
   return dest
end

local function listDirFiles(dir,filter)
   local homedir = os.getenv('HOME')
   if not filter then
       filter = function(s) return true end
   end
   dir = dir:gsub('^~/', homedir..'/')
   if dir:sub(#dir) ~= '/' then
      dir = dir..'/'
   end
   local cmd = 'ls -p1 '..dir
   local files = {}
   local output = io.popen(cmd, 'r')
   for file in output:lines() do
      if not file:match('/') and filter(file) then
         table.insert(files, dir..file)
      end
      if file:match('/') then
          local subdirfiles = listDirFiles( dir..file, filter)
          for _,v in pairs( subdirfiles ) do
             table.insert(files, v)
          end
      end
   end
   output:close()
   return files
end

local function getAllImages()
   images = listDirFiles(config.path)
   if index >= #images then
       index = math.random(1, #images)
   end
end

local function getNextImage()
   index = index + 1
   if index > #images then
       index = 1
   end
   return images[index]
end

local function getPrevImage()
   index = index - 1
   if index < 1 then
       index = #images
   end
   return images[index]
end

local function setWallpaper(pathToImage)
   for s in screen do
      gears.wallpaper.maximized(pathToImage, s, false)
   end
   if timer then
      timer:again()
   end
   if config.show_notify then
      naughty.notify({
         text = "Wallpaper changed\n"..pathToImage,
         timeout = 1
      })
   end
end

local function onClick()
   local filename = getNextImage()
   setWallpaper(filename)
end

local function onSecondClick()
   local filename = getPrevImage()
   setWallpaper(filename)
end

local function setClickListener() 
   root.buttons(gears.table.join(
      root.buttons(),
      awful.button({}, 1, onClick)
   ))
   root.buttons(gears.table.join(
      root.buttons(),
      awful.button({}, 2, onSecondClick)
   ))
end

local function startTimer()
   timer = gears.timer {
      timeout = config.timeout,
      autostart = true,
      callback = onClick
   }
end

local function start(cfg)
   config = mergeTables({
      path = '~/pics/wallpapers/',
      show_notify = true,
      timeout = 60*15,
      change_on_click = true
   }, cfg)
   math.randomseed(os.time())
   getAllImages()
   onClick()
   if config.change_on_click then 
      setClickListener() 
   end
   if config.timeout > 0 then
      startTimer()
   end
end

return {
   start = start
}
