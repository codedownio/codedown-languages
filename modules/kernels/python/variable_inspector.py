# Variable inspector for the Python (ipykernel) Jupyter kernel.
#
# Introspection logic derived from
# https://github.com/lckr/jupyterlab-variableInspector/blob/master/src/inspectorscripts.ts
# BSD 3-Clause "New" or "Revised" License
# https://github.com/lckr/jupyterlab-variableInspector/blob/ffff68edcd8f0912a4af7951cbe58b307afe920d/LICENSE
#
# Everything is wrapped in a single object, `_codedown_variable_inspector`, so the
# inspector adds exactly one name to the user namespace (and that name is hidden
# from its own listing). The frontend runs this file once at kernel startup, then
# calls `_codedown_variable_inspector.list()` and
# `_codedown_variable_inspector.inspect('<name>')`.


def _codedown_make_variable_inspector():
    import json
    import sys
    from IPython import get_ipython
    from IPython.core.magics.namespace import NamespaceMagics

    own_name = "_codedown_variable_inspector"
    max_content = 150
    max_rows = 10000

    class _VariableInspector:
        def __init__(self):
            self._nms = NamespaceMagics()
            self._nms.shell = get_ipython().kernel.shell
            # Optional libraries, populated lazily by _check_imported(). Kept as
            # instance attributes so they never leak into the user namespace.
            self._np = None
            self._pd = None
            self._pyspark = None
            self._tf = None
            self._K = None
            self._torch = None
            self._ipywidgets = None

        def _check_imported(self):
            if 'numpy' in sys.modules:
                import numpy
                self._np = numpy

            if 'pandas' in sys.modules:
                import pandas
                self._pd = pandas

            if 'pyspark' in sys.modules:
                import pyspark
                self._pyspark = pyspark

            if 'tensorflow' in sys.modules or 'keras' in sys.modules:
                import tensorflow
                self._tf = tensorflow
                try:
                    import keras.backend as K
                    self._K = K
                except ImportError:
                    try:
                        import tensorflow.keras.backend as K
                        self._K = K
                    except ImportError:
                        self._K = None

            if 'torch' in sys.modules:
                import torch
                self._torch = torch

            if 'ipywidgets' in sys.modules:
                import ipywidgets
                self._ipywidgets = ipywidgets

        def _getsizeof(self, x):
            # Returns the size in bytes, or None when it can't be determined.
            if type(x).__name__ in ['ndarray', 'Series']:
                return x.nbytes
            elif self._pyspark and isinstance(x, self._pyspark.sql.DataFrame):
                return None
            elif self._tf and isinstance(x, self._tf.Variable):
                return None
            elif self._torch and isinstance(x, self._torch.Tensor):
                return x.element_size() * x.nelement()
            elif self._pd and type(x).__name__ == 'DataFrame':
                return x.memory_usage().sum()
            else:
                return sys.getsizeof(x)

        def _getshapeof(self, x):
            # Returns a list of ints (dimensions), or None.
            if self._pd and isinstance(x, self._pd.DataFrame):
                return list(x.shape)
            if self._pd and isinstance(x, self._pd.Series):
                return list(x.shape)
            if self._np and isinstance(x, self._np.ndarray):
                return list(x.shape)
            if self._pyspark and isinstance(x, self._pyspark.sql.DataFrame):
                return [None, len(x.columns)]
            if self._tf and isinstance(x, self._tf.Variable):
                return [int(d) for d in x.shape]
            if self._tf and isinstance(x, self._tf.Tensor):
                return [int(d) for d in x.shape]
            if self._torch and isinstance(x, self._torch.Tensor):
                return list(x.shape)
            if isinstance(x, list):
                return [len(x)]
            if isinstance(x, dict):
                return [len(x)]
            return None

        def _getcontentof(self, x):
            # A short, friendly preview string.
            if self._pd and isinstance(x, self._pd.DataFrame):
                colnames = ', '.join(x.columns.map(str))
                content = "Columns: %s" % colnames
            elif self._pd and isinstance(x, self._pd.Series):
                content = str(x.values).replace(" ", ", ")[1:-1]
                content = content.replace("\\n", "")
            elif self._np and isinstance(x, self._np.ndarray):
                content = x.__repr__()
            else:
                content = str(x)

            if len(content) > max_content:
                return content[:max_content] + " ..."
            else:
                return content

        def _is_matrix(self, x):
            if self._pd and isinstance(x, self._pd.DataFrame):
                return True
            if self._pd and isinstance(x, self._pd.Series):
                return True
            if self._np and isinstance(x, self._np.ndarray) and len(x.shape) <= 2:
                return True
            if self._pyspark and isinstance(x, self._pyspark.sql.DataFrame):
                return True
            if self._tf and isinstance(x, self._tf.Variable) and len(x.shape) <= 2:
                return True
            if self._tf and isinstance(x, self._tf.Tensor) and len(x.shape) <= 2:
                return True
            if self._torch and isinstance(x, self._torch.Tensor) and len(x.shape) <= 2:
                return True
            if isinstance(x, list):
                return True
            return False

        def _is_widget(self, typ):
            return bool(self._ipywidgets and issubclass(typ, self._ipywidgets.DOMWidget))

        def _keep(self, name):
            if name == own_name:
                return False
            ns = self._nms.shell.user_ns
            try:
                obj = ns[name]
            except KeyError:
                return False
            try:
                if isinstance(obj, str):
                    return True
                if self._tf and isinstance(obj, self._tf.Variable):
                    return True
                if self._pd and (isinstance(obj, self._pd.core.frame.DataFrame)
                                 or isinstance(obj, self._pd.core.series.Series)):
                    return True
                if str(obj)[0] == "<":
                    return False
                if str(obj).startswith("_Feature"):
                    # removes tf/keras objects
                    return False
                return True
            except Exception:
                return False

        def _default(self, o):
            if self._np and isinstance(o, self._np.number):
                return int(o)
            raise TypeError

        def _table(self, x):
            # Returns {"columns": [...], "data": [[...], ...]} for tabular things,
            # capped at max_rows, or None if x isn't tabular.
            if self._pyspark and isinstance(x, self._pyspark.sql.DataFrame):
                if self._pd:
                    return self._table(x.limit(max_rows).toPandas())
                return None
            if self._tf and isinstance(x, (self._tf.Variable, self._tf.Tensor)) and self._K:
                return self._table(self._K.get_value(x))
            if self._torch and isinstance(x, self._torch.Tensor):
                return self._table(x.cpu().numpy())
            if self._pd and isinstance(x, self._pd.DataFrame):
                df = x.head(max_rows)
                columns = [str(c) for c in df.columns]
                data = json.loads(df.to_json(orient="values", default_handler=self._default, force_ascii=False))
                return {"columns": columns, "data": data}
            if self._pd and isinstance(x, self._pd.Series):
                s = x.head(max_rows)
                data = json.loads(s.to_json(orient="values", default_handler=self._default, force_ascii=False))
                return {"columns": ["value"], "data": [[v] for v in data]}
            if self._np and isinstance(x, self._np.ndarray):
                arr = x[:max_rows]
                if arr.ndim <= 1:
                    return {"columns": ["value"], "data": [[v] for v in arr.tolist()]}
                return {"columns": [str(i) for i in range(arr.shape[1])], "data": arr.tolist()}
            if isinstance(x, list):
                rows = x[:max_rows]
                if rows and isinstance(rows[0], (list, tuple)):
                    ncol = max(len(r) for r in rows)
                    return {"columns": [str(i) for i in range(ncol)], "data": [list(r) for r in rows]}
                return {"columns": ["value"], "data": [[v] for v in rows]}
            return None

        def list(self):
            self._check_imported()
            ns = self._nms.shell.user_ns
            result = {}
            for name in self._nms.who_ls():
                if not self._keep(name):
                    continue
                x = ns[name]
                size = self._getsizeof(x)
                result[name] = {
                    "type": type(x).__name__,
                    "size": int(size) if size is not None else None,
                    "shape": self._getshapeof(x),
                    "content": str(self._getcontentof(x)),
                    "isMatrix": self._is_matrix(x),
                    "isWidget": self._is_widget(type(x)),
                }
            print(json.dumps(result, ensure_ascii=False))

        def inspect(self, name):
            self._check_imported()
            ns = self._nms.shell.user_ns
            try:
                x = ns[name]
            except KeyError:
                print(json.dumps({
                    "name": name, "type": None, "size": None, "shape": None,
                    "isMatrix": False, "content": "<undefined>", "table": None,
                }, ensure_ascii=False))
                return

            is_mat = self._is_matrix(x)
            size = self._getsizeof(x)
            table = None
            if is_mat:
                try:
                    table = self._table(x)
                except Exception:
                    table = None
            print(json.dumps({
                "name": name,
                "type": type(x).__name__,
                "size": int(size) if size is not None else None,
                "shape": self._getshapeof(x),
                "isMatrix": is_mat,
                "content": str(x),
                "table": table,
            }, ensure_ascii=False))

        def display_widget(self, widget):
            from IPython.display import display
            display(widget)

        def delete_variable(self, name):
            self._nms.shell.user_ns.pop(name, None)

    return _VariableInspector()


_codedown_variable_inspector = _codedown_make_variable_inspector()
del _codedown_make_variable_inspector
