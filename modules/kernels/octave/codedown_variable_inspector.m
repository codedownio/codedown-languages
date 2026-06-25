% Variable inspector for the Octave (octave_kernel) Jupyter kernel.
%
% Defines a single command-line function, codedown_variable_inspector. Functions
% don't appear in `who`, so it stays hidden from its own listing. The frontend
% runs this file once at startup, then calls codedown_variable_inspector() to list
% variables and codedown_variable_inspector('name') to inspect one.
%
% The user's variables live in the base workspace, which the function reaches with
% evalin('base', ...). jsonencode is built into Octave (>= 7).

function codedown_variable_inspector(varargin)
  MAXLEN = 150;
  MAXROWS = 10000;

  if nargin >= 1
    nm = varargin{1};
    if ~evalin('base', ['exist(''' nm ''', ''var'')'])
      o = struct('name', nm, 'isMatrix', false, 'content', '<undefined>');
      printf('%s\n', jsonencode(o));
      return;
    end
    v = evalin('base', nm);
    w = evalin('base', ['whos(''' nm ''')']);
    ismat = (isnumeric(v) || islogical(v)) && ndims(v) == 2 && numel(v) > 1;
    o = struct();
    o.name = nm;
    o.type = class(v);
    o.size = w.bytes;
    o.shape = w.size;
    o.isMatrix = ismat;
    try
      c = evalc('disp(v)');
    catch
      c = '<unprintable>';
    end
    o.content = strtrim(c);
    if ismat
      m = v;
      if rows(m) > MAXROWS
        m = m(1:MAXROWS, :);
      end
      cols = cell(1, columns(m));
      for j = 1:columns(m)
        cols{j} = num2str(j);
      end
      o.table = struct('columns', {cols}, 'data', m);
    end
    printf('%s\n', jsonencode(o));
    return;
  end

  names = evalin('base', 'who');
  if isempty(names)
    printf('{}\n');
    return;
  end
  out = struct();
  for i = 1:numel(names)
    nm = names{i};
    v = evalin('base', nm);
    w = evalin('base', ['whos(''' nm ''')']);
    info = struct();
    info.type = class(v);
    info.size = w.bytes;
    info.shape = w.size;
    try
      c = evalc('disp(v)');
    catch
      c = '<unprintable>';
    end
    c = strtrim(regexprep(c, '\s+', ' '));
    if numel(c) > MAXLEN
      c = [c(1:MAXLEN) ' ...'];
    end
    info.content = c;
    info.isMatrix = (isnumeric(v) || islogical(v)) && ndims(v) == 2 && numel(v) > 1;
    out.(nm) = info;
  end
  printf('%s\n', jsonencode(out));
end
