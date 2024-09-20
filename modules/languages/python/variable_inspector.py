# Derived from https://github.com/lckr/jupyterlab-variableInspector/blob/master/src/inspectorscripts.ts
# BSD 3-Clause "New" or "Revised" License
# https://github.com/lckr/jupyterlab-variableInspector/blob/ffff68edcd8f0912a4af7951cbe58b307afe920d/LICENSE

import json
import sys
from IPython import get_ipython
from IPython.core.magics.namespace import NamespaceMagics


_codedown_variableinspector_nms = NamespaceMagics()
_codedown_variableinspector_Jupyter = get_ipython()
_codedown_variableinspector_nms.shell = _codedown_variableinspector_Jupyter.kernel.shell

__np = None
__pd = None
__pyspark = None
__tf = None
__K = None
__torch = None
__ipywidgets = None


def _check_imported():
    global __np, __pd, __pyspark, __tf, __K, __torch, __ipywidgets

    if 'numpy' in sys.modules:
        # don't really need the try
        import numpy as __np

    if 'pandas' in sys.modules:
        import pandas as __pd

    if 'pyspark' in sys.modules:
        import pyspark as __pyspark

    if 'tensorflow' in sys.modules or 'keras' in sys.modules:
        import tensorflow as __tf

        try:
            import keras.backend as __K
        except ImportError:
            try:
                import tensorflow.keras.backend as __K
            except ImportError:
                __K = None

    if 'torch' in sys.modules:
        import torch as __torch

    if 'ipywidgets' in sys.modules:
        import ipywidgets as __ipywidgets

def _codedown_variableinspector_getsizeof(x):
    if type(x).__name__ in ['ndarray', 'Series']:
        return x.nbytes
    elif __pyspark and isinstance(x, __pyspark.sql.DataFrame):
        return None
    elif __tf and isinstance(x, __tf.Variable):
        return None
    elif __torch and isinstance(x, __torch.Tensor):
        return x.element_size() * x.nelement()
    elif __pd and type(x).__name__ == 'DataFrame':
        return x.memory_usage().sum()
    else:
        return sys.getsizeof(x)

def _codedown_variableinspector_getshapeof(x):
    if __pd and isinstance(x, __pd.DataFrame):
        return x.shape
    if __pd and isinstance(x, __pd.Series):
        return x.shape
    if __np and isinstance(x, __np.ndarray):
        return x.shape
    if __pyspark and isinstance(x, __pyspark.sql.DataFrame):
        return ["?", len(x.columns)]
    if __tf and isinstance(x, __tf.Variable):
        return x.shape
    if __tf and isinstance(x, __tf.Tensor):
        return x.shape
    if __torch and isinstance(x, __torch.Tensor):
        return x.shape
    if isinstance(x, list):
        return [len(x)]
    if isinstance(x, dict):
        return [len(x)]
    return None

def _codedown_variableinspector_getcontentof(x):
    # returns content in a friendly way for python variables
    # pandas and numpy
    if __pd and isinstance(x, __pd.DataFrame):
        colnames = ', '.join(x.columns.map(str))
        content = "Columns: %s" % colnames
    elif __pd and isinstance(x, __pd.Series):
        content = str(x.values).replace(" ", ", ")[1:-1]
        content = content.replace("\\n", "")
    elif __np and isinstance(x, __np.ndarray):
        content = x.__repr__()
    else:
        content = str(x)

    if len(content) > 150:
        return content[:150] + " ..."
    else:
        return content

def _codedown_variableinspector_is_matrix(x):
    # True if type(x).__name__ in ["DataFrame", "ndarray", "Series"] else False
    if __pd and isinstance(x, __pd.DataFrame):
        return True
    if __pd and isinstance(x, __pd.Series):
        return True
    if __np and isinstance(x, __np.ndarray) and len(x.shape) <= 2:
        return True
    if __pyspark and isinstance(x, __pyspark.sql.DataFrame):
        return True
    if __tf and isinstance(x, __tf.Variable) and len(x.shape) <= 2:
        return True
    if __tf and isinstance(x, __tf.Tensor) and len(x.shape) <= 2:
        return True
    if __torch and isinstance(x, __torch.Tensor) and len(x.shape) <= 2:
        return True
    if isinstance(x, list):
        return True
    return False

def _codedown_variableinspector_is_widget(x):
    return __ipywidgets and issubclass(x, __ipywidgets.DOMWidget)

def _codedown_variableinspector_dict_list():
    _check_imported()
    def keep_cond(v):
        try:
            obj = eval(v)
            if isinstance(obj, str):
                return True
            if __tf and isinstance(obj, __tf.Variable):
                return True
            if __pd and __pd is not None and (
                isinstance(obj, __pd.core.frame.DataFrame)
                or isinstance(obj, __pd.core.series.Series)):
                return True
            if str(obj)[0] == "<":
                return False
            if  v in ['__np', '__pd', '__pyspark', '__tf', '__K', '__torch', '__ipywidgets']:
                return obj is not None
            if str(obj).startswith("_Feature"):
                # removes tf/keras objects
                return False
            return True
        except:
            return False
    values = _codedown_variableinspector_nms.who_ls()
    print(json.dumps({
        _v: {
            "type": type(eval(_v)).__name__,
            "size": int(_codedown_variableinspector_getsizeof(eval(_v))),
            "shape": _codedown_variableinspector_getshapeof(eval(_v)),
            "content": str(_codedown_variableinspector_getcontentof(eval(_v))),
            "isMatrix": _codedown_variableinspector_is_matrix(eval(_v)),
            "isWidget": _codedown_variableinspector_is_widget(type(eval(_v)))
        } for _v in values if keep_cond(_v)
    }, ensure_ascii=False))

def _codedown_variableinspector_getmatrixcontent(x, max_rows=10000):
    # to do: add something to handle this in the future
    threshold = max_rows

    if __pd and __pyspark and isinstance(x, __pyspark.sql.DataFrame):
        df = x.limit(threshold).toPandas()
        return _codedown_variableinspector_getmatrixcontent(df.copy())
    elif __np and __pd and type(x).__name__ == "DataFrame":
        if threshold is not None:
            x = x.head(threshold)
        x.columns = x.columns.map(str)
        return x.to_json(orient="table", default_handler=_codedown_variableinspector_default, force_ascii=False)
    elif __np and __pd and type(x).__name__ == "Series":
        if threshold is not None:
            x = x.head(threshold)
        return x.to_json(orient="table", default_handler=_codedown_variableinspector_default, force_ascii=False)
    elif __np and __pd and type(x).__name__ == "ndarray":
        df = __pd.DataFrame(x)
        return _codedown_variableinspector_getmatrixcontent(df)
    elif __tf and (isinstance(x, __tf.Variable) or isinstance(x, __tf.Tensor)):
        df = __K.get_value(x)
        return _codedown_variableinspector_getmatrixcontent(df)
    elif __torch and isinstance(x, __torch.Tensor):
        df = x.cpu().numpy()
        return _codedown_variableinspector_getmatrixcontent(df)
    elif isinstance(x, list):
        s = __pd.Series(x)
        return _codedown_variableinspector_getmatrixcontent(s)

def _codedown_variableinspector_displaywidget(widget):
    display(widget)

def _codedown_variableinspector_default(o):
    if isinstance(o, __np.number): return int(o)
    raise TypeError

def _codedown_variableinspector_deletevariable(x):
    exec("del %s" % x, globals())
