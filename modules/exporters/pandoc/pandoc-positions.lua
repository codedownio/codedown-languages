-- Strip commonmark's sourcepos wrappers, marking where each word of running text starts and ends
local quiet = 0

-- Marked as a whole: their LaTeX re-scans its argument (soul's \st, \ul) or sets it small
local units = { Strikeout = true, Underline = true, Subscript = true, Superscript = true }

local function pos(attr, which)
  local p = attr.attributes["data-pos"]
  if not p then return nil end
  local l1, c1, l2, c2 = p:match("@(%d+):(%d+)%-(%d+):(%d+)")
  if which == "start" then return l1 .. ":" .. c1 end
  return l2 .. ":" .. c2
end

-- Code, images and autolinks carry their own position; wrapped like the rest, they mark as words
local function wrap_own(el)
  local p = el.attributes["data-pos"]
  if not p then return nil end
  if el.t == "Link" then
    for _, inner in ipairs(el.content) do
      if inner.t == "Span" and inner.attributes["wrapper"] then return nil end
    end
  end
  return pandoc.Span({ el }, { wrapper = "1", ["data-pos"] = p })
end

local function is_gap(el)
  return el == nil or el.t == "Space" or el.t == "SoftBreak" or el.t == "LineBreak" or el.t == "Note"
end

-- Left inside a group, an end marker would cost the group's last letter its italic correction
local function is_end(el)
  return el and el.t == "RawInline" and el.text:find("^\\cdposend{") ~= nil
end

local function is_marker(el)
  return el and el.t == "RawInline" and el.text:find("^\\cdpos") ~= nil
end

-- Written \textbf, \texttt or \textsc
local upright = { Strong = true, Code = true, SmallCaps = true }

local function unwrap(inlines)
  local flat = pandoc.Inlines{}
  local attrs = {}
  for _, el in ipairs(inlines) do
    if el.t == "Span" and el.attributes["wrapper"] then
      for _, inner in ipairs(unwrap(el.content)) do
        flat:insert(inner)
        attrs[#flat] = (#el.content == 1) and el.attr or nil
      end
    else
      flat:insert(el)
    end
  end
  if quiet > 0 then return flat end

  local out = pandoc.Inlines{}
  for i, el in ipairs(flat) do
    local word = el.t == "Str" or el.t == "Math" or el.t == "Code" or el.t == "Image" or el.t == "Link" or units[el.t]
    if word and attrs[i] and is_gap(flat[i - 1]) then
      out:insert(pandoc.RawInline("latex", "\\cdpos{" .. pos(attrs[i], "start") .. "}"))
    end
    local ends = pandoc.Inlines{}
    while not units[el.t] and el.content and el.t ~= "Note" and is_end(el.content[#el.content]) do
      ends:insert(1, el.content:remove())
    end
    out:insert(el)
    out:extend(ends)
    if word and attrs[i] and is_gap(flat[i + 1]) then
      out:insert(pandoc.RawInline("latex", "\\cdposend{" .. pos(attrs[i], "end") .. "}"))
    end
  end
  for j = #out - 2, 1, -1 do
    if is_end(out[j]) and (out[j + 1].t == "Space" or out[j + 1].t == "SoftBreak") then
      local k = j + 2
      while is_marker(out[k]) do k = k + 1 end
      if out[k] and upright[out[k].t] then out:insert(j, pandoc.RawInline("latex", "\\cdposic")) end
    end
  end
  return out
end

local function unwrap_blocks(blocks)
  local out = pandoc.Blocks{}
  for _, b in ipairs(blocks) do
    if b.t == "Div" and b.attributes["wrapper"] then out:extend(b.content) else out:insert(b) end
  end
  return out
end

local function without_markers(el)
  quiet = quiet + 1
  el = el:walk({ Inlines = unwrap })
  quiet = quiet - 1
  return el
end

function Pandoc(doc)
  doc = doc:walk({
    Code = wrap_own, Image = wrap_own, Link = wrap_own,
    Strikeout = without_markers, Underline = without_markers,
    Subscript = without_markers, Superscript = without_markers,
  })
  return doc:walk({ Inlines = unwrap, Blocks = unwrap_blocks })
end
