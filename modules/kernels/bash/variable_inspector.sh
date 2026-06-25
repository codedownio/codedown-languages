# Variable inspector for the Bash (bash_kernel) Jupyter kernel.
#
# Defines a few helper functions (functions don't show up among variables, so they
# stay hidden from the listing) and snapshots the set of variables that already
# exist at startup. The listing then shows only variables defined since — i.e. the
# user's own variables, not the inherited environment.
#
# We enumerate variables by parsing `declare -p` rather than `compgen -v`, because
# Nixpkgs' bash is built without programmable completion (no compgen).
#
# The frontend runs this once at startup, then calls
# __codedown_variable_inspector_list and
# __codedown_variable_inspector_inspect '<name>'.

# Print the names of all currently-defined variables, one per line.
__codedown_vi_names() {
  local line rest name
  while IFS= read -r line; do
    case "$line" in
      "declare "*) ;;
      *) continue ;;
    esac
    rest=${line#declare }   # drop "declare "
    rest=${rest#* }         # drop the flags field
    name=${rest%%=*}        # keep the name (before any '=')
    case "$name" in
      __codedown_vi_*) ;;   # skip the inspector's own helpers/locals
      [a-zA-Z_]*) printf '%s\n' "$name" ;;
    esac
  done < <(declare -p 2>/dev/null)
}

# JSON-encode a string argument.
__codedown_vi_json_str() {
  local s=$1
  s=${s//\\/\\\\}
  s=${s//\"/\\\"}
  s=${s//$'\n'/\\n}
  s=${s//$'\t'/\\t}
  s=${s//$'\r'/\\r}
  printf '"%s"' "$s"
}

# True if the named variable existed at startup (so we should hide it).
__codedown_vi_is_baseline() {
  case "$__codedown_vi_baseline" in
    *$'\n'"$1"$'\n'*) return 0 ;;
    *) return 1 ;;
  esac
}

__codedown_vi_typeof() {
  case "$1" in
    "declare -A"*) printf assoc ;;
    "declare -a"*) printf array ;;
    "declare -i"*) printf integer ;;
    *) printf scalar ;;
  esac
}

# Emit the JSON object body (no surrounding braces) for one variable.
__codedown_vi_emit() {
  local name=$1
  local decl type content shape size ismatrix
  decl=$(declare -p "$name" 2>/dev/null)
  type=$(__codedown_vi_typeof "$decl")

  if [ "$type" = array ] || [ "$type" = assoc ]; then
    local -n __codedown_vi_ref="$name"
    size=${#__codedown_vi_ref[@]}
    shape="[$size]"
    content="${__codedown_vi_ref[*]}"
    ismatrix=true
  else
    content=${!name}
    shape=null
    size=${#content}
    ismatrix=false
  fi

  if [ ${#content} -gt 150 ]; then
    content="${content:0:150} ..."
  fi

  printf '%s:{"type":%s,"size":%s,"shape":%s,"content":%s,"isMatrix":%s}' \
    "$(__codedown_vi_json_str "$name")" \
    "$(__codedown_vi_json_str "$type")" \
    "$size" "$shape" \
    "$(__codedown_vi_json_str "$content")" \
    "$ismatrix"
}

__codedown_variable_inspector_list() {
  local __codedown_vi_first=1 __codedown_vi_name
  printf '{'
  while IFS= read -r __codedown_vi_name; do
    [ -z "$__codedown_vi_name" ] && continue
    case "$__codedown_vi_name" in __codedown_vi_*) continue ;; esac
    __codedown_vi_is_baseline "$__codedown_vi_name" && continue
    [ $__codedown_vi_first -eq 0 ] && printf ','
    __codedown_vi_first=0
    __codedown_vi_emit "$__codedown_vi_name"
  done < <(__codedown_vi_names | sort)
  printf '}\n'
}

__codedown_variable_inspector_inspect() {
  local name=$1
  if ! declare -p "$name" >/dev/null 2>&1; then
    printf '{"name":%s,"isMatrix":false,"content":"<undefined>"}\n' "$(__codedown_vi_json_str "$name")"
    return
  fi

  local decl type content shape size ismatrix table
  decl=$(declare -p "$name" 2>/dev/null)
  type=$(__codedown_vi_typeof "$decl")

  if [ "$type" = array ] || [ "$type" = assoc ]; then
    local -n __codedown_vi_ref="$name"
    size=${#__codedown_vi_ref[@]}
    shape="[$size]"
    content="${__codedown_vi_ref[*]}"
    ismatrix=true
    local rows="" elem
    for elem in "${__codedown_vi_ref[@]}"; do
      [ -n "$rows" ] && rows="$rows,"
      rows="$rows[$(__codedown_vi_json_str "$elem")]"
    done
    table=',"table":{"columns":["value"],"data":['"$rows"']}'
  else
    content=${!name}
    shape=null
    size=${#content}
    ismatrix=false
    table=''
  fi

  printf '{"name":%s,"type":%s,"size":%s,"shape":%s,"isMatrix":%s,"content":%s%s}\n' \
    "$(__codedown_vi_json_str "$name")" \
    "$(__codedown_vi_json_str "$type")" \
    "$size" "$shape" "$ismatrix" \
    "$(__codedown_vi_json_str "$content")" \
    "$table"
}

# Snapshot pre-existing variable names (newline-wrapped for easy matching).
__codedown_vi_baseline=$'\n'"$(__codedown_vi_names)"$'\n'
