__storage : dict = dict()

def add(key : object, value : object) -> None:

    if(key in __storage):
        return None
    __storage[key] = value

def get(key : object) -> None:
     if (key not in __storage):
         raise KeyError(f"Key : {key} not in DataMover")
     return __storage[key]

def set(key : object, value : object) -> None:
     if (key not in __storage):
         raise KeyError(f"Key : {key} not in DataMover")
     __storage[key] = value

def delete(key: object):
    if (key not in __storage):
         raise KeyError(f"Key : {key} not in DataMover")
    del __storage[key]