exports.getHookConsFFI = function(nothing) {
  return function(just) {
    return function(key) {
      return function(hooks) {
        return hooks.hasOwnProperty(key) ? just(hooks[key]) : nothing;
      }
    }
  }
}


exports.setHookUnionFFI = function(toSet) {
    return function(hooks) {
      var o = Object.assign(Object.assign({}, hooks), toSet);
      return o;
  }
}

exports.setHookConsFFI = function(key) {
  return function(val) {
    return function(hooks) {
      var o = Object.assign({}, hooks);
      o[key] = val;
      return o;
    }
  }
}
