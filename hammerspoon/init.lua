-- https://www.mortensoncreative.com/blog/break-up-with-your-mouse-2
hs.loadSpoon("ModalMgr")

spoon.ModalMgr:new("hyperM")
local hyper_modal = spoon.ModalMgr.modal_list["hyperM"]

--# EMOJI SELECTOR #--
-- https://aldur.github.io/articles/hammerspoon-emojis/

-- Build the list of emojis to be displayed.
local choices = {}
for _, emoji in ipairs(hs.json.decode(io.open("emojis/emojis.json"):read())) do
    table.insert(choices,
	{text=emoji['name'],
	    subText=table.concat(emoji['kwds'], ", "),
	    image=hs.image.imageFromPath("emojis/" .. emoji['id'] .. ".png"),
	    chars=emoji['chars']
	})
end

-- Focus the last used window.
local function focusLastFocused()
    local wf = hs.window.filter
    local lastFocused = wf.defaultCurrentSpace:getWindows(wf.sortByFocusedLast)
    if #lastFocused > 0 then lastFocused[1]:focus() end
end

-- Create the chooser.
-- On selection, copy the emoji and type it into the focused application.
local chooser = hs.chooser.new(function(choice)
    if not choice then focusLastFocused(); return end
    hs.pasteboard.setContents(choice["chars"])
    focusLastFocused()
    hs.eventtap.keyStrokes(hs.pasteboard.getContents())
end)

chooser:searchSubText(true)
chooser:choices(choices)

-- TODO Tiered modes? For example W to trigger window mode with it's own bindings
-- TODO Switch through multiple windows of an application (?)
-- Maybe that's: if already on the application switch to a different window, add
-- shift modifier to jump through windows
local BINDINGS = {
   {
      description = 'Exit',
      key = 'escape',
      onEnter = function() end,
   },
   {
      description = 'Exit',
      key = 'g',
      modifiers = {'ctrl'},
      onEnter = function() end,
   },
   {
      description = 'Toggle cheat sheet',
      key = '/',
      onEnter = function() spoon.ModalMgr:toggleCheatsheet() end,
      keepModalOpen = true,
   },
   {
      description = 'Reload config',
      key = 'r',
      onEnter = function() hs.reload() end,
   },
   {
      description = 'Window left',
      key = 'h',
      onEnter = function()
	 local win = hs.window.focusedWindow()
	 local f = win:frame()
	 local screen = win:screen()
	 local max = screen:frame()

	 f.x = max.x
	 f.y = max.y
	 f.w = max.w / 2
	 f.h = max.h
	 win:setFrame(f)
      end,
   },
   {
      description = 'Window right',
      key = 'l',
      onEnter = function()
	 local win = hs.window.focusedWindow()
	 local f = win:frame()
	 local screen = win:screen()
	 local max = screen:frame()

	 f.x = max.x + (max.w / 2)
	 f.y = max.y
	 f.w = max.w / 2
	 f.h = max.h
	 win:setFrame(f)
      end,
   },
   {
      description = 'Window fullscreen',
      key = 'return',
      onEnter = function()
	 local win = hs.window.focusedWindow()
	 local f = win:frame()
	 local screen = win:screen()
	 local max = screen:frame()

	 f.x = max.x
	 f.y = max.y
	 f.w = max.w
	 f.h = max.h
	 win:setFrame(f)
      end,
   },
   {
      description = 'Emoji Selector',
      key = '1',
      onEnter = function()
	 chooser:show()
      end,
   },
   {
      description = '[F]irefox',
      key = 'F',
      onEnter = function() hs.application.launchOrFocus('Firefox') end,
   },
   {
      description = '[E]macs',
      key = 'E',
      onEnter = function() hs.application.launchOrFocus('Emacs') end,
   },
   {
      description = 'i[T]erm',
      key = 'T',
      onEnter = function() hs.application.launchOrFocus('Iterm') end,
   },
   {
      description = '[S]lack',
      key = 'S',
      onEnter = function() hs.application.launchOrFocus('Slack') end,
   },
   {
      description = 'i[M]essage',
      key = 'M',
      onEnter = function() hs.application.launchOrFocus('Messages') end,
   },
   {
      description = 'Amazon [C]hime',
      key = 'C',
      onEnter = function() hs.application.launchOrFocus('Amazon Chime') end,
   },
   {
      description = 'S[p]otify',
      key = 'P',
      onEnter = function() hs.application.launchOrFocus('Spotify') end,
   },
   {
      description = '[O]utlook',
      key = 'O',
      onEnter = function() hs.application.launchOrFocus('Microsoft Outlook') end,

   },
}

for _, binding in ipairs(BINDINGS) do
   hyper_modal:bind(
      binding.modifiers,
      binding.key,
      binding.description,
      function ()
	 binding.onEnter()
	 if not binding.keepModalOpen then
	    spoon.ModalMgr:deactivate({"hyperM"})
	 end
      end
   )
end

hs.hotkey.bind('ctrl', 'space', function()
   spoon.ModalMgr:deactivateAll()
   -- Show the keybindings cheatsheet once appM is activated
   spoon.ModalMgr:activate({"hyperM"}, "#FFBD2E", false)
end)
