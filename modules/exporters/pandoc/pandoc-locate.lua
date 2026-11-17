-- The locate program for pandoc-export-server (RENDER_QUERY in codedownio/lsp-export-server). Reads
-- RENDER_JOB.cdpos: "h height" with the page height, then "s|e line:col page x y size" per word
-- start and end, x and y in sp from the page's bottom-left, size in pt.

local env = os.getenv
local SP_PER_BP = 65536 * 72.27 / 72

-- An output line's extent above and below the baseline, as fractions of the font size
local ASCENT, DESCENT = 0.8, 0.25

local function read_words(path)
  local f = io.open(path)
  if not f then return nil end
  local height
  local words, open = {}, nil
  for line in f:lines() do
    local kind, l, c, page, x, y, size = line:match("^(%a) (%d+):(%d+) (%d+) (%-?%d+) (%-?%d+) ([%d.]+)")
    if kind == "s" then
      open = { line = tonumber(l), col = tonumber(c), page = tonumber(page), x = tonumber(x) / SP_PER_BP,
               y = tonumber(y) / SP_PER_BP, size = tonumber(size) }
    elseif kind == "e" and open then
      open.end_line, open.end_col = tonumber(l), tonumber(c)
      open.end_page, open.end_x, open.end_y = tonumber(page), tonumber(x) / SP_PER_BP, tonumber(y) / SP_PER_BP
      words[#words + 1] = open
      open = nil
    else
      local h = line:match("^h ([%d.]+)pt")
      if h then height = tonumber(h) * 72 / 72.27 end
    end
  end
  f:close()
  return words, height
end

local function top_of(w, height) return height - w.y - ASCENT * w.size * 72 / 72.27 end
local function depth(w) return (ASCENT + DESCENT) * w.size * 72 / 72.27 end

local function forward(words, height, line)
  local boxes, order = {}, {}
  for _, w in ipairs(words) do
    if w.line <= line and w.end_line >= line then
      -- A word broken across output lines only marks where it starts
      local right = (w.end_page == w.page and w.end_y == w.y) and w.end_x or w.x
      local key = w.page .. ":" .. w.y
      local b = boxes[key]
      if not b then
        b = { page = w.page, left = w.x, right = right, top = top_of(w, height), height = depth(w) }
        boxes[key] = b
        order[#order + 1] = b
      else
        b.left = math.min(b.left, w.x)
        b.right = math.max(b.right, right)
      end
    end
  end
  local out = {}
  local first_page = order[1] and order[1].page
  for _, b in ipairs(order) do
    if b.page == first_page then
      out[#out + 1] = string.format("%d %.3f %.3f %.3f %.3f", b.page, b.left, b.top, b.right - b.left, b.height)
    end
  end
  return out
end

-- The word under the point, else the nearest one on the closest output line
local function inverse(words, height, page, x, y)
  local best, best_score
  for _, w in ipairs(words) do
    if w.page == page then
      local top = top_of(w, height)
      local bottom = top + depth(w)
      local dy = (y < top and top - y) or (y > bottom and y - bottom) or 0
      local right = (w.end_page == w.page and w.end_y == w.y) and w.end_x or w.x
      local dx = (x < w.x and w.x - x) or (x > right and x - right) or 0
      local score = dy * 1000 + dx
      if not best_score or score < best_score then best, best_score = w, score end
    end
  end
  if not best then return nil end
  local col = best.col
  local right = best.end_y == best.y and best.end_x or nil
  if right and best.end_line == best.line and right > best.x and x > best.x then
    local frac = math.min(1, (x - best.x) / (right - best.x))
    col = best.col + math.floor(frac * (best.end_col - best.col) + 0.5)
  end
  return string.format("%d %d %s", best.line, col, env("RENDER_DOCUMENT"))
end

local words, height = read_words(env("RENDER_OUT") .. "/" .. env("RENDER_JOB") .. ".cdpos")
local result = io.open(env("RENDER_RESULT"), "w")
if words and height then
  if env("RENDER_QUERY") == "forward" then
    for _, l in ipairs(forward(words, height, tonumber(env("RENDER_LINE")))) do result:write(l, "\n") end
  else
    local answer = inverse(words, height, tonumber(env("RENDER_PAGE")), tonumber(env("RENDER_X")), tonumber(env("RENDER_Y")))
    if answer then result:write(answer, "\n") end
  end
end
result:close()
