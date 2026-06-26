# Variable inspector for the Ruby (iruby) Jupyter kernel.
#
# Everything lives in a single module constant, CodedownVariableInspector. Being a
# constant (not a local), it stays out of the variable listing. iruby shares the
# top-level binding across cells, so the list/inspect commands pass `binding` and we
# read the user's locals from it. iruby keeps `_`-prefixed history variables (_i,
# __, _1, ...) in that binding, so we filter those out.
#
# The frontend runs this once at startup, then calls
# CodedownVariableInspector.list(binding) and
# CodedownVariableInspector.inspect_var(binding, "name").

require 'json'
require 'objspace'

module CodedownVariableInspector
  MAX_CONTENT = 150
  MAX_ROWS = 10000

  def self.preview(val)
    c = (val.inspect rescue "<uninspectable>")
    c.length > MAX_CONTENT ? c[0, MAX_CONTENT] + " ..." : c
  end

  def self.shape_of(val)
    case val
    when Array, Hash, String then [val.length]
    else nil
    end
  end

  def self.size_of(val)
    ObjectSpace.memsize_of(val)
  rescue StandardError
    nil
  end

  def self.info(val)
    {
      type: val.class.name,
      size: size_of(val),
      shape: shape_of(val),
      content: preview(val),
      isMatrix: val.is_a?(Array)
    }
  end

  def self.user_var?(name)
    !name.start_with?("_")
  end

  def self.list(b)
    out = {}
    b.local_variables.each do |sym|
      name = sym.to_s
      next unless user_var?(name)
      out[name] = info(b.local_variable_get(sym))
    end
    puts JSON.generate(out)
    nil
  end

  # Coerce a value to something JSON.generate can handle in a table cell.
  def self.jsonable(x)
    case x
    when Numeric, String, true, false, nil then x
    else (x.inspect rescue x.to_s)
    end
  end

  def self.table_of(val)
    if val.is_a?(Array)
      { columns: ["value"], data: val.first(MAX_ROWS).map { |x| [jsonable(x)] } }
    elsif val.is_a?(Hash)
      { columns: ["key", "value"], data: val.first(MAX_ROWS).map { |k, v| [jsonable(k), jsonable(v)] } }
    end
  end

  def self.inspect_var(b, name)
    sym = name.to_sym
    unless b.local_variables.include?(sym)
      puts JSON.generate({ name: name, isMatrix: false, content: "<undefined>" })
      return nil
    end
    val = b.local_variable_get(sym)
    out = {
      name: name,
      type: val.class.name,
      size: size_of(val),
      shape: shape_of(val),
      isMatrix: val.is_a?(Array),
      content: (val.inspect rescue "<uninspectable>"),
      table: table_of(val)
    }
    puts JSON.generate(out)
    nil
  end
end
