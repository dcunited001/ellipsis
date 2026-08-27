local function map(list, fn)
    local result = {}
    for i, v in ipairs(list) do
        result[i] = fn(v, i)
    end
    return result
end

-- Example 1: Transform numbers
local numbers = { 1, 2, 3, 4 }
local doubled = map(numbers, function(val)
    return val * 2
end)

-- Output: 2, 4, 6, 8
for _, v in ipairs(doubled) do print(v) end
-- for i, title in ipairs(emacs_tabs) do

-- Example 1: Transform numbers
local numbers = { 1, 2, 3, 4 }
local doubled = map(numbers, function(val)
    return val * 2
end)
