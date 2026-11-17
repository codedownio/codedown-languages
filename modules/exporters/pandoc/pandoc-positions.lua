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

local function is_gap(el)
  return el == nil or el.t == "Space" or el.t == "SoftBreak" or el.t == "LineBreak"
end

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
    local word = el.t == "Str" or el.t == "Math" or el.t == "Code" or units[el.t]
    if word and attrs[i] and is_gap(flat[i - 1]) then
      out:insert(pandoc.RawInline("latex", "\\cdpos{" .. pos(attrs[i], "start") .. "}"))
    end
    out:insert(el)
    if word and attrs[i] and is_gap(flat[i + 1]) then
      out:insert(pandoc.RawInline("latex", "\\cdposend{" .. pos(attrs[i], "end") .. "}"))
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
    Header = without_markers, Table = without_markers, Figure = without_markers,
    Strikeout = without_markers, Underline = without_markers,
    Subscript = without_markers, Superscript = without_markers,
  })
  return doc:walk({ Inlines = unwrap, Blocks = unwrap_blocks })
end
