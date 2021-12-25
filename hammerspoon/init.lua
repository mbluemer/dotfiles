-- https://www.mortensoncreative.com/blog/break-up-with-your-mouse-2
hs.loadSpoon("ModalMgr")
local K = require('keys')

spoon.ModalMgr:new("hyperM")
local hyper_modal = spoon.ModalMgr.modal_list["hyperM"]

-- TODO Tiered modes? For example W to trigger window mode with it's own bindings
-- TODO Switch through multiple windows of an application
local BINDINGS = {
   {
      description = 'Exit',
      key = 'escape',
      onEnter = function() end,
   },
   {
      description = 'Exit',
      key = 'g',
      modifiers = {K.ctrl},
      onEnter = function() end,
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
}

for _, binding in ipairs(BINDINGS) do
   hyper_modal:bind(
      binding.modifiers,
      binding.key,
      binding.description,
      function ()
	 binding.onEnter()
	 spoon.ModalMgr:deactivate({"hyperM"})
      end
   )
end

hs.hotkey.bind(K.ctrl, 'space', function()
   spoon.ModalMgr:deactivateAll()
   -- Show the keybindings cheatsheet once appM is activated
   spoon.ModalMgr:activate({"hyperM"}, "#FFBD2E", true)
end)
