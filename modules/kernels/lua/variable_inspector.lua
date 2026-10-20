-- Variable inspector for the Lua (xeus-lua) Jupyter kernel.
--
-- Lua has no notion of "user variables" -- an assignment at the top level of a cell
-- goes into _G alongside everything the standard library and the kernel put there.
-- So we snapshot the names present at startup and list only what appeared since.
--
-- Output goes through io.write rather than print: xeus-lua's print routes through
-- pprint, which would quote and reformat our JSON. io.write publishes the string
-- verbatim.
--
-- The frontend runs this file once at startup, then calls
-- __codedown_variable_inspector_list() and
-- __codedown_variable_inspector_inspect('<name>').

__codedown_vi = {}

local M = __codedown_vi

-- Escape a Lua string into a JSON string literal, including the quotes.
function M.json_str(s)
  s = tostring(s)
  s = s:gsub('[\\"]', '\\%0')
  s = s:gsub('\n', '\\n'):gsub('\r', '\\r'):gsub('\t', '\\t')
  -- Remaining control characters have no short escape.
  s = s:gsub('%c', function(c) return string.format('\\u%04x', c:byte()) end)
  return '"' .. s .. '"'
end

-- A table is "array-like" if its only keys are 1..#t. Those get a shape and a
-- single-column table in the detail view; everything else is shown as a map.
function M.is_array(t)
  local n = 0
  for _ in pairs(t) do n = n + 1 end
  return n == #t
end

function M.type_of(v)
  local t = type(v)
  if t == 'table' then
    return M.is_array(v) and 'array' or 'table'
  end
  return t
end

-- A short human-readable rendering. Tables are summarized rather than dumped,
-- since the listing shows one line per variable.
function M.content_of(v)
  local t = type(v)
  if t == 'string' then
    return v
  elseif t == 'table' then
    if M.is_array(v) then
      local parts = {}
      for i = 1, math.min(#v, 20) do
        parts[#parts + 1] = M.short(v[i])
      end
      if #v > 20 then parts[#parts + 1] = '...' end
      return '{' .. table.concat(parts, ', ') .. '}'
    else
      local keys = {}
      for k in pairs(v) do keys[#keys + 1] = tostring(k) end
      table.sort(keys)
      local parts = {}
      for i = 1, math.min(#keys, 20) do
        parts[#parts + 1] = keys[i] .. ' = ' .. M.short(v[keys[i]])
      end
      if #keys > 20 then parts[#parts + 1] = '...' end
      return '{' .. table.concat(parts, ', ') .. '}'
    end
  end
  return tostring(v)
end

-- One nesting level deeper than content_of goes, so a table of tables doesn't
-- expand without bound.
function M.short(v)
  if type(v) == 'table' then
    return M.is_array(v) and ('array(' .. #v .. ')') or 'table'
  elseif type(v) == 'string' then
    return string.format('%q', v)
  end
  return tostring(v)
end

function M.size_of(v)
  local t = type(v)
  if t == 'string' then return #v end
  if t == 'table' then
    local n = 0
    for _ in pairs(v) do n = n + 1 end
    return n
  end
  return nil
end

function M.truncate(s)
  if #s > 150 then return s:sub(1, 150) .. ' ...' end
  return s
end

-- Names that exist but should never be listed: our own helpers, and the kernel's.
function M.hidden(name)
  return name:sub(1, 13) == '__codedown_vi'
      or name == '__codedown_variable_inspector_list'
      or name == '__codedown_variable_inspector_inspect'
end

function M.user_names()
  local names = {}
  for k, _ in pairs(_G) do
    if type(k) == 'string' and not M.baseline[k] and not M.hidden(k) then
      names[#names + 1] = k
    end
  end
  table.sort(names)
  return names
end

function __codedown_variable_inspector_list()
  local out = {}
  for _, name in ipairs(M.user_names()) do
    local v = _G[name]
    local ty = M.type_of(v)
    local size = M.size_of(v)
    local is_array = (ty == 'array')
    out[#out + 1] = string.format(
      '%s:{"type":%s,"size":%s,"shape":%s,"content":%s,"isMatrix":%s}',
      M.json_str(name),
      M.json_str(ty),
      size and tostring(size) or 'null',
      is_array and ('[' .. #v .. ']') or 'null',
      M.json_str(M.truncate(M.content_of(v))),
      tostring(is_array))
  end
  io.write('{' .. table.concat(out, ',') .. '}\n')
end

function __codedown_variable_inspector_inspect(name)
  local v = _G[name]
  if v == nil then
    io.write(string.format('{"name":%s,"isMatrix":false,"content":"<undefined>"}\n',
                           M.json_str(name)))
    return
  end

  local ty = M.type_of(v)
  local size = M.size_of(v)
  local is_array = (ty == 'array')

  local tbl = ''
  if ty == 'array' then
    local rows = {}
    for i = 1, #v do rows[#rows + 1] = '[' .. M.json_str(M.short(v[i])) .. ']' end
    tbl = ',"table":{"columns":["value"],"data":[' .. table.concat(rows, ',') .. ']}'
  elseif ty == 'table' then
    local keys = {}
    for k in pairs(v) do keys[#keys + 1] = tostring(k) end
    table.sort(keys)
    local rows = {}
    for _, k in ipairs(keys) do
      rows[#rows + 1] = '[' .. M.json_str(k) .. ',' .. M.json_str(M.short(v[k])) .. ']'
    end
    tbl = ',"table":{"columns":["key","value"],"data":[' .. table.concat(rows, ',') .. ']}'
  end

  io.write(string.format(
    '{"name":%s,"type":%s,"size":%s,"shape":%s,"isMatrix":%s,"content":%s%s}\n',
    M.json_str(name),
    M.json_str(ty),
    size and tostring(size) or 'null',
    is_array and ('[' .. #v .. ']') or 'null',
    tostring(is_array),
    M.json_str(M.truncate(M.content_of(v))),
    tbl))
end

-- Snapshot what's already in _G, last, so the inspector's own definitions above
-- are included in the baseline and stay out of the listing.
M.baseline = {}
for k, _ in pairs(_G) do
  M.baseline[k] = true
end
