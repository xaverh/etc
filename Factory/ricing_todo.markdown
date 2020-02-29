# Todo

-   grml's zsh
-   nnn
-   sxiv
-   move tag of current client to another screen
-   open tags by name
-   zoom like in dwm

## Awesome Tag Model

-   Tags können manuell angelegt werden mit awful.tag.add()

```lua
awful.tag.add("First tag", {
    icon               = "/path/to/icon1.png",
    layout             = awful.layout.suit.tile,
    master_fill_policy = "master_width_factor",
    gap_single_client  = true,
    gap                = 15,
    screen             = s,
    selected           = true,
})

awful.tag.add("Second tag", {
    icon = "/path/to/icon2.png",
    layout = awful.layout.suit.max,
    screen = s,
})
```

To get a tag from its name:

```lua
local t = awful.tag.find_by_name(awful.screen.focused(), "name")
```

Delete the current tag

```lua
local function delete_tag()
    local t = awful.screen.focused().selected_tag
    if not t then return end
    t:delete()
end
```

Create a new tag at the end of the list

```lua
local function add_tag()
    awful.tag.add("NewTag", {
        screen = awful.screen.focused(),
        layout = awful.layout.suit.floating }):view_only()
end
```

Rename the current tag

```lua
local function rename_tag()
    awful.prompt.run {
        prompt       = "New tag name: ",
        textbox      = awful.screen.focused().mypromptbox.widget,
        exe_callback = function(new_name)
            if not new_name or #new_name == 0 then return end

            local t = awful.screen.focused().selected_tag
            if t then
                t.name = new_name
            end
        end
    }
end
```

Move the focused client to a new tag

```lua
local function move_to_new_tag()
    local c = client.focus
    if not c then return end

    local t = awful.tag.add(c.class,{screen= c.screen })
    c:tags({t})
    t:view_only()
end
```

Copy the current tag at the end of the list

```lua
local function copy_tag()
    local t = awful.screen.focused().selected_tag
    if not t then return end

    local clients = t:clients()
    local t2 = awful.tag.add(t.name, awful.tag.getdata(t))
    t2:clients(clients)
    t2:view_only()
end
```
