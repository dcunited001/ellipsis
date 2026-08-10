dc = dc or {}
dc.ws = dc.ws or {}

-- TODO find lua helper to "extend" properties for table references

function dc.ws.binds(mod, key, name)
    hl.bind(table.concat({ mod, key }, "+"),
        hl.dsp.focus({ workspace = name }),
        { description = "Toggle WS: " .. name })
    hl.bind(table.concat({ mod, "SHIFT", key }, "+"),
        hl.dsp.window.move({ workspace = name }),
        { description = "Move Win to WS: " .. name })
end

function dc.ws.binds_special(mod, key, name)
    hl.bind(table.concat({ mod, key }, "+"),
        hl.dsp.workspace.toggle_special(name),
        { description = "Toggle WS: " .. name })
    hl.bind(table.concat({ mod, "SHIFT", key }, "+"),
        hl.dsp.window.move({ workspace = "special:" .. name }),
        { description = "Move Win to WS: " .. name })
end

function dc.elmap(li, fn)
    local res = {}
    for i, el in ipairs(li) do
        -- table.insert(res, i) -- fn(e))
        res[i] = fn(el)
    end
    return res
end

function dc.activities_title(act)
    return "(" .. act.prefix .. ")(.*)(" .. act.tab .. ")(.*)"
end

function dc.activities_launch(tab, socket)
    return "uwsm app -- doomclient " .. (socket and "-s " .. socket or "")
        .. " -- -ce '(activities-resume (cdr (assoc \""
        .. tab .. "\" activities-activities)))'"
end
